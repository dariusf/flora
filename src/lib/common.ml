
open Containers

let is_int s =
  try ignore (int_of_string s); true
  with _ -> false

module Styles = struct
  open Notty.A
  let keyword = fg red
  let insert_mode = bg green ++ fg black
  let normal_mode = bg yellow ++ fg black
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
[@@deriving show, eq]

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
  val add : int -> t -> t
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
      f |> List.map string_of_int |> String.concat ";" |> fun a -> "[" ^ a ^ "]"

  (* these operate on the entire state and thus will never see a Nope *)

  let deeper (F d) = F (D.snoc d 0)
  let add i (F d) = F (D.snoc d i)

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

type focus_view = {
  is_focused: bool;
  is_parent_focused: bool;
  focus_relative: Focus.t;
  path_from_root: Focus.t;
}

(** A catamorphism which also computes focus for a given node *)
let cata_focus focus node
    (f : focus_view -> 'b Containers.List.t -> 'a node -> 'b) =
  let rec run relative path node =
    let foc = {
      is_focused = Focus.is_focused relative;
      is_parent_focused = Focus.is_parent_focused relative;
      focus_relative = relative;
      path_from_root = path
    } in
    match node with
    | Empty ->
      f foc [] node
    | Static (tag, items)
    | Dynamic (tag, items) ->
      let child_results = List.mapi (fun i e ->
          run (Focus.view_deeper relative i) (Focus.add i path) e
        ) items in
      f foc child_results node
  in run focus Focus.initial node

let next_postorder focus node pred =
  let seen = ref false in
  let result = ref None in
  (* this relies on the left-to-right, bottom-up traversal order of cata_focus *)
  cata_focus focus node (fun foc _ node ->
      if foc.is_focused then
        seen := true
      else if !seen && Option.is_none !result && pred node then
        result := Some (foc.path_from_root)
      else ()
    );
  !result

let prev_postorder focus node pred =
  let prev = ref None in
  let result = ref None in
  (* also relies on traversal order *)
  cata_focus focus node (fun foc _ node ->
      if not foc.is_focused && pred node && Option.is_none !result then
        prev := Some (foc.path_from_root)
      else if foc.is_focused then
        result := !prev
      else ()
    );
  !result

let rec map_focus focus node
    (f : focus_view -> 'a node -> 'b node) =
  let go foc children n =
    match n with
    | Empty -> f foc Empty
    | Static (tag, _) -> f foc (Static (tag, children))
    | Dynamic (tag, _) -> f foc (Dynamic (tag, children))
  in
  cata_focus focus node go

let modify_ast focus node insertion =
  let f foc n =
    if foc.is_focused then
      insertion
    else
      n
  in
  map_focus focus node f

let match_completions term completions =
  completions |> List.filter (String.prefix ~pre:term)
