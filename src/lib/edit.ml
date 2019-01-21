
open Teash
open Containers
open Lens.Infix
open Common

type mode =
  | Insert
  | Normal
[@@deriving show]

type field = {
  engine: Zed_edit.t;
  cursor: Zed_cursor.t;
  ctx: Zed_edit.context;
}

type state = {
  mode: mode;
  focus: Focus.t;
  structure: Lang.Simpl.t node;
  debug: string;
  field: field;
}
[@@deriving lens, show]

type msg =
  | Deeper
  | Shallower
  | Next
  | Prev
  | ToInsert
  | Key of Notty.Unescape.key 

let init () = {
  mode = Normal;
  focus = Focus.initial;
  structure = Lang.Simpl.example;
  debug = Focus.show Focus.initial;
  field =
    let engine = Zed_edit.create () in
    let cursor = Zed_edit.new_cursor engine in
    let ctx = Zed_edit.context engine cursor in
    {
      engine; cursor; ctx
    }
}, Cmd.none

let key_to_cmd = function
  | (`Arrow `Up), _mods -> Cmd.msg Shallower
  | (`Arrow `Down), _mods -> Cmd.msg Deeper
  | (`Arrow `Left), _mods -> Cmd.msg Prev
  | (`Arrow `Right), _mods -> Cmd.msg Next
  (* | (`ASCII 's'), _mods -> Cmd.msg (Set 42)
     | (`ASCII 'r'), _mods -> Cmd.msg Reset *)
  | (`ASCII 'i'), _mods -> Cmd.msg ToInsert
  | (`ASCII 'q'), _mods -> App.exit
  | _ -> Cmd.none

let update_debug state =
  (state_debug ^= (Focus.show state.focus)) state

let update state = function
  | Deeper -> (state_focus ^%= Focus.deeper) state |> update_debug, Cmd.none
  | Shallower -> (state_focus ^%= Focus.shallower) state |> update_debug, Cmd.none
  | Next -> (state_focus ^%= Focus.next) state |> update_debug, Cmd.none
  | Prev -> (state_focus ^%= Focus.prev) state |> update_debug, Cmd.none
  | ToInsert -> (state_mode ^= Insert) state, Cmd.none
  | Key key -> state, (key_to_cmd key)

let view state = Notty.(
    [
      (* I.strf "counter: %d" counter; *)
      (* I.string A.(fg lightred) "up - inc, down - dec, s - set to 42, r - reset, q - quit"; *)

      (* (let a1 = A.(fg lightwhite ++ bg red) *)
      (* and a2 = A.(fg red) in *)
      (* I.(string a1 "Rad" <|> (string a2 "stuff!" |> vpad 1 0))) *)
      (* ; *)

      (* render_if 1 2 3 *)
      Lang.Simpl.render state.focus state.structure;
      I.string Styles.normal state.debug;
      I.string Styles.normal (state |. state_mode |> show_mode);

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