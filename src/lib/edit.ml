
open Teash
open Containers
open Lens.Infix

module Focus
  (* : sig
     type t
     val initial : t
     val deeper : t -> t
     val shallower : t -> int -> t
     val view_deeper : t -> int -> t
     (* val current : t -> int option *)
     val is_current : t -> int -> bool
     end *)
= struct
  module D = CCFQueue
  type t = int D.t

  (* an empty deque is invalid *)
  let initial = D.singleton 0

  (* D.empty *)
  (* TODO need to limit this by the depth of the structure *)
  (* TODO in general (e.g. next position) depends on the structure *)
  let deeper d = D.snoc d 0

  (* let d1, _ = D.take_back_exn d in d1 *)
  (* let current d = D.take_front d |> Option.map fst *)
  let topmost d = D.size d = 1

  let is_current d n = topmost d && snd (D.take_back_exn d) = n

  let shallower d =
    if topmost d then d
    else let d1, _ = D.take_back_exn d in d1

  let next d =
    let d1, e = D.take_back_exn d in
    D.snoc d (e + 1)

  let prev d =
    let d1, e = D.take_back_exn d in
    D.snoc d (e - 1)

  let view_deeper d n =
    (* if D.is_empty d then *)
    if is_current d n then d
    else let _, d1 = D.take_front_exn d in d1
end

type 'a node =
  | Empty
  | Static of 'a * (string * 'a node) list
  | Dynamic of 'a * 'a node list

module Styles = struct
  let keyword = Notty.A.(fg red)
  let hole = Notty.A.(fg lightblack)
  let normal = Notty.A.empty
  let focused = Notty.A.(bg yellow ++ fg black)
end

type simpl =
  | And
  | If

(* let tag = function
   | Empty -> failwith "no tag for empty"
   | Static (t, _) -> t
   | Dynamic (t, _) -> t *)

let items = function
  | Empty -> []
  | Static (_, i) -> List.map snd i
  | Dynamic (_, i) -> i

let draw focus n text =
  let open Notty.I in
  let s =
    if Focus.is_current focus n then
      Styles.focused
    else
      match text with
      | "..." -> Styles.hole
      | "if" | "else" -> Styles.keyword
      | "&&" -> Styles.normal
      | _ -> Styles.normal
  in
  string s text

let rec render_simpl focus node =
  let open Notty.I in
  match node with
  | Empty -> draw focus 0 "..."
  | Static (tag, _)
  | Dynamic (tag, _) ->
    match tag with
    | And ->
      let [left; right] = items node in
      render_simpl (Focus.view_deeper focus 0) left <|> 
      (draw focus 0 " && ") <|>
      render_simpl (Focus.view_deeper focus 1) right
    | If ->
      let [cond; conseq; alt] = items node in
      (draw focus 0 "if" <|>
       (draw focus 0 " (" <|>
        render_simpl (Focus.view_deeper focus 0) cond <|>
        draw focus 0 ") ")
       <|> draw focus 0 " {")
      <->
      (render_simpl (Focus.view_deeper focus 1) conseq |> hpad 2 0)
      <->
      (draw focus 0 "} " <|>
       (draw focus 0 "else") <|>
       draw focus 0 " {")
      <->
      (render_simpl (Focus.view_deeper focus 2) alt |> hpad 2 0)
      <->
      (draw focus 0 "}")

type state = {
  focus: Focus.t;
  structure: simpl node;
  debug: string
}
[@@deriving lens]

type msg =
  | Deeper
  | Shallower
  | Next
  | Prev
  (* | Inc
     | Dec
     | Set of int
     | Reset*)
  | Key of Notty.Unescape.key 

let example = Static (If, [
    "cond", Static (And, ["left", Empty; "right", Empty]);
    "conseq", Empty;
    "alt", Empty;
  ])

let init () = {
  focus = Focus.initial;
  structure = example;
  debug = "";
}, Cmd.none

let key_to_cmd = function
  | (`Arrow `Up), _mods -> Cmd.msg Shallower
  | (`Arrow `Down), _mods -> Cmd.msg Deeper
  | (`Arrow `Left), _mods -> Cmd.msg Prev
  | (`Arrow `Right), _mods -> Cmd.msg Next
  (* | (`ASCII 's'), _mods -> Cmd.msg (Set 42)
     | (`ASCII 'r'), _mods -> Cmd.msg Reset *)
  | (`ASCII 'q'), _mods -> App.exit
  | _ -> Cmd.none

let update state = function
  | Deeper ->
    (state_focus ^%= Focus.deeper) state, Cmd.none
  | Shallower -> (state_focus ^%= Focus.shallower) state, Cmd.none
  (*   | Set n -> n, Cmd.none
       | Reset -> 0, Cmd.none *)
  | Next -> (state_focus ^%= Focus.next) state, Cmd.none
  | Prev -> (state_focus ^%= Focus.prev) state, Cmd.none
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
      render_simpl state.focus state.structure;
      I.string Styles.normal state.debug;

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