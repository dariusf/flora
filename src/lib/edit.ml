
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
  (**
     tracks the path to the highest child focused.
     an empty queue means everything below that is highlighted
     (due to focus being inherited).
     nope is to distinguish the empty case from that.
  *)
  type t = Nope | F of int D.t

  let initial =
    F D.empty
  (* D.singleton 0 *)

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

  (* this is partial because we don't expect a nope to be stored *)
  let deeper (F d) = F (D.snoc d 0)

  (* let d1, _ = D.take_back_exn d in d1 *)
  (* let current d = D.take_front d |> Option.map fst *)
  (* let topmost d = D.size d = 1 *)

  (* see deeper for why this is partial *)
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
  let keyword = Notty.A.(fg red)
  let hole = Notty.A.(fg lightblack)
  let normal = Notty.A.empty
  let focused = Notty.A.(bg yellow ++ fg black)
end

type simpl =
  | And
  | If
[@@deriving show]

(* let tag = function
   | Empty -> failwith "no tag for empty"
   | Static (t, _) -> t
   | Dynamic (t, _) -> t *)

(* let items = function
   | Empty -> []
   | Static (_, i) -> List.map snd i
   | Dynamic (_, i) -> i *)

(* TODO parent focus *)
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

(* a catamorphism which also computes focus for a given node *)
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

       (* |> is_focused *)
      ) items in
    (* e) items in *)
    (* failwith "" *)
    (* let children = List.map (fun (e, fc) -> *)
    (* with_focus Focus.(view_deeper focus)) items in *)
    f (Focus.is_focused focus) fs node

(* let a =
   with_focus 

   match tag with
   | And ->
    let [left; right] = items node in
    render_simpl (Focus.view_deeper focus 0) left <|> 
    (draw focus " && ") <|>
    render_simpl (Focus.view_deeper focus 1) right
   | If ->
    let [cond; conseq; alt] = items node in
    (draw focus "if" <|>
     (draw focus " (" <|>
      render_simpl (Focus.view_deeper focus 0) cond <|>
      draw focus ") ")
     <|> draw focus " {")
    <->
    (render_simpl (Focus.view_deeper focus 1) conseq |> hpad 2 0)
    <->
    (draw focus "} " <|>
     (draw focus "else") <|>
     draw focus " {")
    <->
    (render_simpl (Focus.view_deeper focus 2) alt |> hpad 2 0)
    <->
    (draw focus "}") *)


let rec render_simpl focus node =
  let open Notty.I in
  let f this_fc fs node =
    match node with
    | Empty -> draw this_fc "..."
    | Static (tag, _)
    | Dynamic (tag, _) ->
      match tag, fs with
      | And, [left; right] ->
        (* let [left; right] = items in *)
        (* render_simpl fl left *)
        left <|> 
        (draw this_fc " && ") <|>
        right
      (* render_simpl fr right *)
      | If, [cond; conseq; alt] ->
        (* let [cond; conseq; alt] = items in *)
        (* let  = fs in *)
        (draw this_fc "if" <|>
         (draw this_fc " (" <|>
          (* render_simpl fc cond <|> *)
          cond <|>
          draw this_fc ") ")
         <|> draw this_fc " {")
        <->
        (
          (* render_simpl fcs conseq *)
          conseq
          |> hpad 2 0)
        <->
        (draw this_fc "} " <|>
         (draw this_fc "else") <|>
         draw this_fc " {")
        <->
        (
          alt
          (* render_simpl fa alt *)
          |> hpad 2 0)
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
  (* | Inc
     | Dec
     | Set of int
     | Reset*)
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
  | Deeper ->
    (state_focus ^%= Focus.deeper) state |> update_debug, Cmd.none
  | Shallower -> (state_focus ^%= Focus.shallower) state |> update_debug, Cmd.none
  (*   | Set n -> n, Cmd.none
       | Reset -> 0, Cmd.none *)
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