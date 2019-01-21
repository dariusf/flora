
let is_int s =
  try ignore (int_of_string s); true
  with _ -> false

module Styles = struct
  open Notty.A
  let keyword = fg red
  let hole = fg lightblack
  let normal = empty
  let focused = bg yellow ++ fg black
  let value = fg (Notty.A.rgb_888 147 112 219)
end

type 'a node =
  | Empty
  | Static of 'a * 'a node list
  | Dynamic of 'a * 'a node list
[@@deriving show]

module Focus : sig
  type t
  [@@deriving show]

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
  [@@deriving show]

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