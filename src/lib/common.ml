
open Containers

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
  let cursor = bg lightblack
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
  val is_parent_focused : t -> bool
  val view_deeper : t -> int -> t
  val show : t -> string
  val deeper : t -> t
  val shallower : t -> t
  val on_last : t -> (int -> int) -> t
  val next : t -> t
  val prev : t -> t

  (* TODO this depends on nodes and maybe shouldn't be here *)
  val validate : t -> 'a node -> bool
end = struct
  module D = CCFQueue
  (**
     tracks the path to the highest child focused.
     an empty queue means everything below that is highlighted
     (due to focus being inherited).
     nope is to distinguish the empty case from that.
  *)
  type t = Nope | ByParent | F of int D.t
  [@@deriving show]

  let initial =
    F D.empty

  (* these operate on intermediate focus structures
     and thus are allowed to contain Nope *)

  let is_focused d =
    match d with
    | Nope -> false
    | ByParent -> false
    | F d -> D.is_empty d

  let is_parent_focused d =
    match d with
    | Nope -> false
    | ByParent -> true
    | F d -> D.is_empty d

  let view_deeper fd n =
    match fd with
    | Nope
    | ByParent -> fd
    | F d ->
      if D.is_empty d then
        ByParent
      else
        let e, d1 = D.take_front_exn d in
        if e = n then F d1 else Nope

  let show d =
    match d with
    | Nope -> "Nope"
    | ByParent -> "ByParent"
    | F d ->
      let f = d |> CCFQueue.to_list in
      match f with
      | [] -> "<empty>"
      | _ -> f |> List.map string_of_int |> String.concat ";" |> fun a -> "[" ^ a ^ "]"

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
    on_last d (fun e -> Int.max 0 (e - 1))

  let rec validate (F d) node =
    match node with
    | Empty -> D.is_empty d
    | Static (_, children)
    | Dynamic (_, children) ->
      (match D.take_front d with
       | Some (e, d1) ->
         List.nth_opt children e
         |> Option.map (validate (F d1))
         |> Option.get_or ~default:false
       | None -> true
      )


end

let logfile = open_out "log.ignore"
let log s =
  IO.write_line logfile s; flush logfile

let next_postorder focus node =
  ()

(** A catamorphism which also computes focus for a given node *)
let rec with_focus focus node
    (f : is_focused:bool -> is_parent_focused:bool -> 'b Containers.List.t -> 'a node -> 'b) =
  match node with
  | Empty ->
    f ~is_focused:(Focus.is_focused focus) ~is_parent_focused:(Focus.is_parent_focused focus) [] node
  | Static (tag, items)
  | Dynamic (tag, items) ->
    let fs = List.mapi (fun i e ->
        let deeper = Focus.view_deeper focus i in
        with_focus deeper e f
      ) items in
    f ~is_focused:(Focus.is_focused focus) ~is_parent_focused:(Focus.is_parent_focused focus) fs node

let rec map_focus focus node
    (f : is_focused:bool -> is_parent_focused:bool -> 'a node -> 'b node) =
  let go ~is_focused ~is_parent_focused children n =
    match n with
    | Empty -> f ~is_focused ~is_parent_focused Empty
    | Static (tag, _) -> f ~is_focused ~is_parent_focused (Static (tag, children))
    | Dynamic (tag, _) -> f ~is_focused ~is_parent_focused (Dynamic (tag, children))
  in
  with_focus focus node go

let modify_ast focus node insertion =
  let f ~is_focused ~is_parent_focused:_ n =
    if is_focused then
      insertion
    else
      n
  in
  map_focus focus node f

let match_completions term completions =
  completions |> List.filter (String.prefix ~pre:term)
