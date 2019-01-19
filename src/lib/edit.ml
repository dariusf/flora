
open Teash
open Containers
open Lens.Infix

module Focus : sig
  type t
  val initial : t
  val is_focused : t -> bool
  val view_deeper : t -> int -> t
  val show : t -> string
  val deeper : t -> t
  val shallower : t -> t
  val on_last : t -> (int -> int) -> t
  val next : t -> t
  val prev : t -> t
end = struct
  module D = CCFQueue
  (**
     tracks the path to the highest child focused.
     an empty queue means everything below that is highlighted
     (due to focus being inherited).
     nope is to distinguish the empty case from that.
  *)
  type t = Nope | F of int D.t

  let initial =
    F D.empty

  (* these operate on intermediate focus structures
     and thus are allowed to contain Nope *)

  let is_focused d =
    match d with
    | Nope -> false
    | F d -> D.is_empty d

  let view_deeper fd n =
    match fd with
    | Nope -> fd
    | F d ->
      if D.is_empty d then F d
      else
        let e, d1 = D.take_front_exn d in
        if e = n then F d1 else Nope

  let show d =
    match d with
    | Nope -> "nope"
    | F d ->
      let f = d |> CCFQueue.to_list in
      match f with
      | [] -> "<empty>"
      | _ -> f |> List.map string_of_int |> String.concat ";" |> fun a -> "[" ^ a ^ "]"

  (* TODO need to limit this by the depth of the structure *)
  (* TODO in general (e.g. next position) depends on the structure *)

  (* these operate on the entire state and thus will never see a Nope *)

  let deeper (F d) = F (D.snoc d 0)

  let shallower (F d) =
    if D.is_empty d then F d
    else let d1, _ = D.take_back_exn d in F d1

  let on_last (F d) f =
    if D.is_empty d then F d
    else
      let d1, e = D.take_back_exn d in
      F (D.snoc d1 (f e))

  let next d =
    on_last d (fun e -> e + 1)

  let prev d =
    on_last d (fun e -> e - 1)

end

type 'a node =
  | Empty
  | Static of 'a * 'a node list
  | Dynamic of 'a * 'a node list
[@@deriving show]

module Styles = struct
  open Notty.A
  let keyword = fg red
  let hole = fg lightblack
  let normal = empty
  let focused = bg yellow ++ fg black
end

type simpl =
  | And
  | If
[@@deriving show]

let draw focus text =
  let open Notty.I in
  let s =
    if focus then
      Styles.focused
    else
      match text with
      | "..." -> Styles.hole
      | "if" | "else" -> Styles.keyword
      | "&&" -> Styles.normal
      | _ -> Styles.normal
  in
  string s text

(** A catamorphism which also computes focus for a given node *)
let rec with_focus focus node
    (f : bool -> Notty.I.t Containers.List.t -> 'a node -> Notty.I.t) =
  match node with
  | Empty ->
    let this_focused = Focus.is_focused focus in
    f this_focused [] node
  | Static (tag, items)
  | Dynamic (tag, items) ->
    let fs = List.mapi (fun i e ->
        let deeper = Focus.view_deeper focus i in
        with_focus deeper e f
      ) items in
    f (Focus.is_focused focus) fs node

let render_simpl focus node =
  let open Notty.I in
  let f this_fc fs node =
    match node with
    | Empty -> draw this_fc "..."
    | Static (tag, _)
    | Dynamic (tag, _) ->
      match tag, fs with
      | And, [left; right] ->
        left <|> 
        (draw this_fc " && ") <|>
        right
      | If, [cond; conseq; alt] ->
        (draw this_fc "if" <|>
         (draw this_fc " (" <|>
          cond <|>
          draw this_fc ") {"))
        <->
        (conseq |> hpad 2 0)
        <->
        (draw this_fc "} " <|>
         (draw this_fc "else") <|>
         draw this_fc " {")
        <->
        (alt |> hpad 2 0)
        <->
        (draw this_fc "}")
      | _ -> failwith ("invalid combination of args: " ^ show_node pp_simpl node ^ " and " ^ string_of_int (List.length fs))
  in
  with_focus focus node f

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
  | Key of Notty.Unescape.key 

let example = Static (If, [
    Static (And, [Empty; Empty]);
    Empty;
    Empty;
  ])

let init () = {
  focus = Focus.initial;
  structure = example;
  debug = Focus.show Focus.initial;
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

let update_debug state =
  (state_debug ^= (Focus.show state.focus)) state

let update state = function
  | Deeper -> (state_focus ^%= Focus.deeper) state |> update_debug, Cmd.none
  | Shallower -> (state_focus ^%= Focus.shallower) state |> update_debug, Cmd.none
  | Next -> (state_focus ^%= Focus.next) state |> update_debug, Cmd.none
  | Prev -> (state_focus ^%= Focus.prev) state |> update_debug, Cmd.none
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