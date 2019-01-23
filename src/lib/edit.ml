
open Teash
open Containers
open Lens.Infix
open Common

type mode =
  | Insert
  | Normal
[@@deriving show]

type field = {
  engine: unit Zed_edit.t;
  cursor: Zed_cursor.t;
  ctx: unit Zed_edit.context;
}

let pp_field fmt (t : field) = Format.fprintf fmt "<field>"
let show_field x = Format.asprintf "%a" pp_field x

module Lang = Lang.Simpl

type state = {
  mode: mode;
  focus: Focus.t;
  structure: Lang.t node;
  debug: string;
  field: field;
  completions: string list;
}
[@@deriving lens, show]

type msg =
  | Deeper
  | Shallower
  | Next
  | Prev
  | NextHole
  | ToInsert
  | ToNormal
  | UpdateCompletions
  | CommitCompletion
  | Key of Notty.Unescape.key 

let init () = {
  mode = Normal;
  focus = Focus.initial;
  structure = Lang.example;
  debug = Focus.show Focus.initial;
  field = (
    let engine = Zed_edit.create () in
    let cursor = Zed_edit.new_cursor engine in
    let ctx = Zed_edit.context engine cursor in
    { engine; cursor; ctx });
  completions = [];
}, Cmd.msg UpdateCompletions

let key_to_cmd = function
  | `Arrow `Up, _mods
  | `ASCII 'k', _mods -> Cmd.msg Shallower
  | `Arrow `Down, _mods
  | `ASCII 'j', _mods -> Cmd.msg Deeper
  | `Arrow `Left, _mods
  | `ASCII 'h', _mods -> Cmd.msg Prev
  | `Arrow `Right, _mods
  | `ASCII 'l', _mods -> Cmd.msg Next
  | `ASCII 'n', _mods -> Cmd.msg NextHole
  | `ASCII 'i', _mods -> Cmd.msg ToInsert
  | `ASCII 'q', _mods -> App.exit
  | _ -> Cmd.none

let update_debug state =
  let d = Focus.show state.focus in
  let d1 =
    match next_postorder state.focus state.structure (fun n -> equal_node Lang.equal n Empty) with
    | None -> "no next node"
    | Some f -> Focus.show f
  in
  let d2 = d ^ "; next = " ^ d1 in
  (state_debug ^= d2) state

let get_field_text f = Zed_edit.text f.engine |> Zed_rope.to_string

let clear_field f =
  Zed_edit.(get_action Delete_prev_line f.ctx); f

let update state = function
  | Deeper ->
    let f1 = Focus.deeper state.focus in
    let s1 =
      if Focus.validate f1 state.structure then
        (state_focus ^= f1) state
      else state
    in
    s1 |> update_debug, Cmd.none
  | Shallower ->
    let f1 = Focus.shallower state.focus in
    let s1 =
      if Focus.validate f1 state.structure then
        (state_focus ^= f1) state
      else state
    in
    s1 |> update_debug, Cmd.none
  | Next ->
    let f1 = Focus.next state.focus in
    let s1 =
      if Focus.validate f1 state.structure then
        (state_focus ^= f1) state
      else state
    in
    s1 |> update_debug, Cmd.none
  | Prev ->
    let f1 = Focus.prev state.focus in
    let s1 =
      if Focus.validate f1 state.structure then
        (state_focus ^= f1) state
      else state
    in
    s1 |> update_debug, Cmd.none
  | NextHole ->
    (match next_postorder state.focus state.structure (fun n -> equal_node Lang.equal n Empty) with
     | None -> state, Cmd.none
     | Some f -> (state_focus ^= f) state, Cmd.none
    )
  | ToInsert -> (state_mode ^= Insert) state, Cmd.none
  | ToNormal ->
    state |> (state_mode ^= Normal) |> (state_field ^%= clear_field), Cmd.none
  | CommitCompletion ->
    let text = get_field_text state.field in
    let compl = match_completions text (Lang.completions |> List.map fst) in
    (match compl with
     | [c] ->
       let ast = List.assoc ~eq:String.equal c Lang.completions in
       state |> (state_structure ^%= (fun s -> modify_ast state.focus s ast))
       |> (state_focus ^%= Focus.deeper)
       |> (state_field ^%= clear_field)
     , Cmd.msg UpdateCompletions
     | _ -> state, Cmd.none
    )
  | UpdateCompletions ->
    let text = get_field_text state.field in
    let compl =
      if String.is_empty text then []
      else match_completions text (Lang.completions |> List.map fst)
    in
    (state_completions ^= compl) state, Cmd.none
  | Key key -> (
      match state.mode with
      | Insert -> (
          match key with
          | `Escape, _mods -> state, Cmd.msg ToNormal
          | `Tab, _mods
          | `Enter, _mods -> state, Cmd.msg CommitCompletion
          | _ ->
            let act = match key with
              | `ASCII c, _mods -> Some (Zed_edit.Insert (CamomileLibrary.UChar.of_char c))
              | `Backspace, _mods -> Some (Zed_edit.Delete_prev_char)
              | `Arrow `Left, _mods -> Some (Zed_edit.Prev_char)
              | `Arrow `Right, _mods -> Some (Zed_edit.Next_char)
              | _ -> None
            in
            Option.iter (fun a -> Zed_edit.get_action a state.field.ctx) act;
            state, Cmd.msg UpdateCompletions
        );
      | Normal -> state, (key_to_cmd key)
    )

let render_field state =
  let open Notty.I in
  let text = get_field_text state.field in
  let cursor = match state.mode with
    | Normal -> string Styles.normal ""
    | Insert -> string Styles.cursor " "
  in
  string Styles.normal text <|> cursor

let view state = Notty.(
    [
      (* I.strf "counter: %d" counter; *)
      (* I.string A.(fg lightred) "up - inc, down - dec, s - set to 42, r - reset, q - quit"; *)

      (* (let a1 = A.(fg lightwhite ++ bg red) *)
      (* and a2 = A.(fg red) in *)
      (* I.(string a1 "Rad" <|> (string a2 "stuff!" |> vpad 1 0))) *)
      (* ; *)

      (* render_if 1 2 3 *)
      Lang.render state.focus state.structure;
      I.string Styles.normal state.debug;
      I.string Styles.normal (state.mode |> show_mode);
      render_field state;
      I.vcat (state.completions |> List.map (I.string Styles.normal));
    ] |> I.vcat
  )

let subscriptions _model =
  Keyboard.presses (fun key -> Key key)

let main () =
  App.run {
    init;
    update;
    view;
    subscriptions;
    shutdown = (fun _model -> ());
  } ()