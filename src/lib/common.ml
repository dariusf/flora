
open Containers

let is_int s =
  try ignore (int_of_string s); true
  with _ -> false

module Char = struct
  include Char
  let (<=) a b = compare a b <= 0
end

let is_uppercase c = Char.('A' <= c && c <= 'Z')
let is_lowercase c = Char.('a' <= c && c <= 'z')
let is_letter c =
  is_uppercase c || is_lowercase c

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
  let highlighted = fg red
end

module Node_ = struct
  type 'a t =
    | Empty of ('a t -> bool)
    | Static of 'a * 'a t list
    | Dynamic of 'a * 'a t list
  [@@deriving show]

  let empty ?(pred=fun _ -> true) () = Empty pred

  let is_empty = function
    | Empty _ -> true
    | _ -> false
end

module Focus : sig
  type t
  [@@deriving show]

  type view = {
    is_focused: bool;
    is_parent_focused: bool;
    focus_relative: t;
    path_from_root: t;
  }

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
  val add : int -> t -> t
  val validate : t -> 'a Node_.t -> bool
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

  type view = {
    is_focused: bool;
    is_parent_focused: bool;
    focus_relative: t;
    path_from_root: t;
  }

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
    | Node_.Empty _ -> D.is_empty d
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

module Node = struct

  include Node_

  (** A catamorphism which also computes focus for a given node *)
  let cata_focus focus node
      (f : Focus.view -> 'b Containers.List.t -> 'a t -> 'b) =
    let rec run relative path node =
      let foc = {
        Focus.is_focused = Focus.is_focused relative;
        is_parent_focused = Focus.is_parent_focused relative;
        focus_relative = relative;
        path_from_root = path
      } in
      match node with
      | Empty _ ->
        f foc [] node
      | Static (tag, items)
      | Dynamic (tag, items) ->
        let child_results = List.mapi (fun i e ->
            run (Focus.view_deeper relative i) (Focus.add i path) e
          ) items in
        f foc child_results node
    in run focus Focus.initial node

  let next_postorder focus node pred =
    (* TODO not sure if this is a problem, but this impl cannot locate nodes which are children of the focal point *)
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

  let map_focus focus node
      (f : Focus.view -> 'a t -> 'b t) =
    let go foc children n =
      match n with
      | Empty p -> f foc (Empty p)
      | Static (tag, _) -> f foc (Static (tag, children))
      | Dynamic (tag, _) -> f foc (Dynamic (tag, children))
    in
    cata_focus focus node go

  let modify focus node insertion =
    let f foc n =
      if foc.Focus.is_focused then
        insertion
      else
        n
    in
    map_focus focus node f

  let get focus node =
    cata_focus focus node (fun f children this ->
        if f.is_focused then
          [this]
        else
          List.concat children
      )
    |> (fun ns ->
        try
          List.hd
        with Failure _ ->
          raise (Invalid_argument "get_ast: focus invalid")
      )

  let uphold_invariants node =
    (* the initial focus given here doesn't matter as it's not used in f *)
    map_focus Focus.initial node (fun _ n ->
        match n with
        | Dynamic (tag, children) ->
          let c = children |> List.rev |> List.drop_while is_empty |> (fun xs -> (empty ()) :: xs) |> List.rev in
          Dynamic (tag, c)
        | _ -> n
      )
end

let parse_completions : string -> (string -> 'a Node.t option) list -> 'a Node.t list = fun term more ->
  let open Option in
  List.fold_left (fun t c ->
      match t with
      | None -> c term
      | Some _ -> t
    ) None more |> to_list