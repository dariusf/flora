
open Teash
open Common
open Types
open Core

type mode =
  | Insert
  | Normal

let pp_mode fmt (m : mode) =
  let s =
    match m with
    | Insert -> "Insert"
    | Normal -> "Normal"
  in Format.fprintf fmt "%s" s
let show_mode x = Format.asprintf "%a" pp_mode x

type field = {
  engine: unit Zed_edit.t;
  cursor: Zed_cursor.t;
  ctx: unit Zed_edit.context;
}

let pp_field fmt (t : field) = Format.fprintf fmt "<field>"
let show_field x = Format.asprintf "%a" pp_field x

type state = {
  screen_dimensions: int * int;
  scroll: int * int;
  mode: mode;
  focus: Focus.t;
  structure: (Lang.t, Lang.m) Node.t;
  debug: string;
  field: field;
  completions: (string * Notty.image) list;
  undo: ((Lang.t, Lang.m) Node.t * Focus.t) list;
}
[@@deriving lens]

(* this goes outside the flow of TEA because it's too cumbersome to send
   a Render message each time anything changes. *)
let editor_dimensions = ref (max_int, max_int)

type msg =
  | Deeper
  | Shallower
  | Next
  | Prev
  | NextHole
  | PrevHole
  | InsertMode
  | NormalMode
  | UpdateCompletions
  | Undo
  | CommitCompletion
  | Resize of int * int
  | Scroll of int * int
  | Key of Notty.Unescape.key 

let init () =
  let w, h =
    (* TODO stderr *)
    match Notty_unix.winsize Unix.stdout with
    | None -> raise (Failure "not a tty, cannot determine window size")
    | Some (w, h) -> w, h
  in
  {
    screen_dimensions = (w, h);
    scroll = (0, 0);
    mode = Normal;
    focus = Focus.initial;
    structure = Lang.example |> Node.uphold_invariants;
    debug = Focus.show Focus.initial;
    field = (
      let engine = Zed_edit.create () in
      let cursor = Zed_edit.new_cursor engine in
      let ctx = Zed_edit.context engine cursor in
      { engine; cursor; ctx });
    completions = [];
    undo = [];
  }, Cmd.msg UpdateCompletions

let key_to_cmd is_hole screen_height key =
  match key with
  | `Arrow `Up, _ -> Cmd.msg (Scroll (0, -1))
  | `Arrow `Down, _ -> Cmd.msg (Scroll (0, 1))
  | `Arrow `Left, _ -> Cmd.msg (Scroll (-1, 0))
  | `Arrow `Right, _ -> Cmd.msg (Scroll (1, 0))
  (* not sure why ctrl + letter is uppercased *)
  | `ASCII 'F', [`Ctrl] -> Cmd.msg (Scroll (0, screen_height - 1))
  | `ASCII 'B', [`Ctrl] -> Cmd.msg (Scroll (0, - (screen_height - 1)))
  | `ASCII 'k', _ -> Cmd.msg Shallower
  | `ASCII 'j', _ -> Cmd.msg Deeper
  | `ASCII 'h', _ -> Cmd.msg Prev
  | `ASCII 'l', _ -> Cmd.msg Next
  | `ASCII 'u', _ -> Cmd.msg Undo
  | `ASCII 'N', _ -> Cmd.msg PrevHole
  | `ASCII 'n', _ -> Cmd.msg NextHole
  | `ASCII 'i', _ when is_hole -> Cmd.msg InsertMode
  | `ASCII 'q', _ -> App.exit
  | _ -> Cmd.none

let update_debug state =
  let d = Focus.show state.focus in
  let d1 =
    match Node.next_postorder state.focus state.structure Node.is_empty with
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
  | PrevHole ->
    (match Node.prev_postorder state.focus state.structure Node.is_empty with
     | None -> state, Cmd.none
     | Some f -> (state_focus ^= f) state |> update_debug, Cmd.none
    )
  | NextHole ->
    (match Node.next_postorder state.focus state.structure Node.is_empty with
     | None -> state, Cmd.none
     | Some f -> (state_focus ^= f) state |> update_debug, Cmd.none
    )
  | InsertMode -> (state_mode ^= Insert) state, Cmd.none
  | NormalMode ->
    state |> (state_mode ^= Normal) |> (state_field ^%= clear_field), Cmd.none
  | CommitCompletion ->
    let text = get_field_text state.field in
    let compl, literal =
      match state.completions with
      | [] -> parse_completions text Lang.guessed_completions, true
      | [c, image] -> List.assoc_opt ~eq:String.equal c Lang.completions |> Option.to_list, false
      | _ -> [], false
    in
    (match compl with
     | [ast] ->
       let ast1 = Node.modify state.focus state.structure ast |> Node.uphold_invariants in
       let old_ast = state.structure in
       let old_focus = state.focus in
       let s = state
               |> (state_structure ^= ast1)
               |> (state_focus ^%= (fun f -> if not literal then Focus.deeper f else f))
               |> (state_field ^%= clear_field)
               |> (state_undo ^%= (fun s -> ((old_ast, old_focus) :: s) |> List.take 5))
       in
       (* s, Cmd.batch ([Cmd.msg UpdateCompletions] @ if literal then [Cmd.msg ToNormal] else []) *)
       s, Cmd.batch ([Cmd.msg UpdateCompletions] @ if literal then [Cmd.msg NextHole] else [])
     | _ ->
       (* TODO print cannot guess *)
       state, Cmd.none
    )
  | Undo ->
    let s =
      match state.undo with
      | [] -> state
      | (s1, f) :: rest -> state |> (state_structure ^= s1) |> (state_undo ^= rest) |> (state_focus ^= f)
    in
    s, Cmd.none
  | UpdateCompletions ->
    let text = get_field_text state.field in
    (* let hole = Node.get state.focus state.structure in *)
    let meta, hole = get_with_predicate state.focus state.structure in
    (* let pred =
       match hole with
       | Empty ->  *)
    let pred = meta |> Option.map Lang.get_predicate
               |> Option.get_or ~default:(fun _ -> true)
               (* |> Option.get_lazy (fun _ -> raise (Failure "no predicate")) *)
    in
    let compl =
      (* if String.is_empty text then [] *)
      (* else *)
      match_completions pred hole text Lang.completions
    in
    (state_completions ^= compl) state, Cmd.none
  | Resize (w, h) -> (state_screen_dimensions ^= (w, h)) state, Cmd.none
  | Scroll (dw, dh) ->
    (state_scroll ^%= fun (w, h) -> (
          let (we, he) = !editor_dimensions in
          let w1 = w + dw in
          let h1 = h + dh in
          clamp ~high:(we - 1) ~low:0 w1,
          clamp ~high:(he - 1) ~low:(state.screen_dimensions
                                     (* 2: 1 for status bar, 1 for buffer. *)
                                     (* TODO might need to make this more general *)
                                     |> fun (_, h) -> -h + 2) h1
        )) state, Cmd.none
  | Key key -> (
      match state.mode with
      | Insert -> (
          match key with
          | `Escape, _mods -> state, Cmd.msg NormalMode
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
      | Normal ->
        let is_hole = Node.(get state.focus state.structure |> is_empty) in
        let _, screen_height = state.screen_dimensions in
        state, (key_to_cmd is_hole screen_height key)
    )

let render_field state =
  let open Notty.I in
  let text = get_field_text state.field in
  let cursor = match state.mode with
    | Normal -> string Styles.normal ""
    | Insert -> string Styles.cursor " "
  in
  string Styles.normal text <|> cursor

let term_resize () =
  Sub.registration "terminal:resize" (fun { push; term; } ->
      Notty_lwt.Term.events term
      |> Lwt_stream.filter_map
        (function
          | `Resize (w, h) -> Some (Resize (w, h))
          | _ -> None
        )
      |> Lwt_stream.iter (fun msg -> push (Some msg))
    )

let crop_to w h image =
  (* the semantics of overlaying make the composite width the max of the arguments',
      but we want it to be the min. note that this pads too. *)
  let open Notty.I in
  let iw, ih = width image, height image in
  crop ~l:0 ~r:(iw - w) ~t:0 ~b:(ih - h) image

let view state =
  Notty.I.(
    let w, h = state.screen_dimensions in
    let viewport = void w (h - 1) in
    let status = void w 1 in
    let editor = [
      Lang.render state.focus state.structure;
      string Styles.normal state.debug;
      string (match state.mode with Normal -> Styles.normal_mode | Insert -> Styles.insert_mode) (state.mode |> show_mode);
      render_field state;
      (let compl = state.completions |> List.map snd in
       match compl with
       | [] when not (String.is_empty (get_field_text state.field)) -> string Styles.normal "<no matches; guess?>"
       | _ -> vcat compl);
    ] |> vcat in
    (* observe editor dimensions before cropping to the viewport *)
    editor_dimensions := (width editor, height editor);
    let (ws, hs) = state.scroll in
    let editor = crop ~l:ws ~t:hs editor
                 |> crop_to (width viewport) (height viewport) in
    (editor </> viewport) <-> (string Styles.normal "lul" </> status)
  )

let subscriptions _model =
  Sub.batch [
    Keyboard.presses (fun key -> Key key);
    term_resize ();
  ]

let main () =
  App.run {
    init;
    update;
    view;
    subscriptions;
    shutdown = (fun _model -> ());
  } ()