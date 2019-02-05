
open Common

module Node_ = struct
  type ('tag, 'meta) t =
    | Empty
    | Static of 'tag * ('meta * ('tag, 'meta) t) list
    | Dynamic of 'tag * 'meta * ('tag, 'meta) t list
  [@@deriving show]

  let debug_show n =
    match n with
    | Empty -> "empty"
    | Static _ -> "static"
    | Dynamic _ -> "dynamic"

  let is_empty = function
    | Empty -> true
    | _ -> false

  let tag n =
    match n with
    | Empty -> raise (Invalid_argument ("no tag: " ^ debug_show n))
    | Static (t, _) | Dynamic (t, _, _) -> t
end

module Focus : sig
  (** Says whether or not a given node is in focus, relative to where the focus actually is.
      See implementation for possible values and examples. *)
  type t
  [@@deriving show]

  (**
     - [is_focused]: true iff a node is in focus
     - [is_parent_focused]: true for all nodes whose parent is in focus
     - [focus_relative]: see {!type:t}
     - [path_from_root]: path to this node from the root; also a {!type:t} and not just a {!type:Deque.t} because we use it to move focus here
     - [child_focus]: indicates which child, if any, is in focus; will have exactly one or no true values
  *)
  type view = {
    is_focused: bool;
    is_parent_focused: bool;
    focus_relative: t;
    path_from_root: t;
    child_focus: bool list;
  }
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
  val add : int -> t -> t
  val validate : t -> ('a, 'b) Node_.t -> bool
end = struct
  (* Possible values:
     - Nope: yeah, no
     - ByParent: no, but it's the child of a node in focus
     - []: yes
     - [a, b, ...]: no, but the child reached by following the given path is in focus
  *)
  type t =
    | Nope
    | ByParent
    | F of int Deque.t
  [@@deriving show]

  type view = {
    is_focused: bool;
    is_parent_focused: bool;
    focus_relative: t;
    path_from_root: t;
    child_focus: bool list;
  }
  [@@deriving show]

  let initial =
    F Deque.empty

  (* These operate on intermediate focus values
     and thus are allowed to contain Nope *)

  let is_focused d =
    match d with
    | Nope -> false
    | ByParent -> false
    | F d -> Deque.is_empty d

  let is_parent_focused d =
    match d with
    | Nope -> false
    | ByParent -> true
    | F d -> Deque.is_empty d

  (* The only place relative focus values are generated *)
  let view_deeper fd n =
    match fd with
    | Nope
    | ByParent -> fd
    | F d ->
      if Deque.is_empty d then
        ByParent
      else
        let e, d1 = Deque.take_front_exn d in
        if e = n then F d1 else Nope

  let show d =
    match d with
    | Nope -> "Nope"
    | ByParent -> "ByParent"
    | F d ->
      let f = d |> CCFQueue.to_list in
      f |> List.map string_of_int |> String.concat ";" |> fun a -> "[" ^ a ^ "]"

  (* These operate on the entire state and thus will only see F *)

  let deeper (F d) = F (Deque.snoc d 0)
  let add i (F d) = F (Deque.snoc d i)

  let shallower (F d) =
    if Deque.is_empty d then F d
    else let d1, _ = Deque.take_back_exn d in F d1

  let on_last (F d) f =
    if Deque.is_empty d then F d
    else
      let d1, e = Deque.take_back_exn d in
      F (Deque.snoc d1 (f e))

  let next d =
    on_last d (fun e -> e + 1)

  let prev d =
    on_last d (fun e -> Int.max 0 (e - 1))

  let rec validate (F d) node =
    match node with
    | Node_.Empty -> Deque.is_empty d
    | Static (_, children) ->
      (match Deque.take_front d with
       | Some (e, d1) ->
         List.nth_opt children e
         |> Option.map snd (* drop metadata *)
         |> Option.map (validate (F d1))
         |> Option.get_or ~default:false
       | None -> true
      )
    | Dynamic (_, _, children) ->
      (match Deque.take_front d with
       | Some (e, d1) ->
         List.nth_opt children e
         |> Option.map (validate (F d1))
         |> Option.get_or ~default:false
       | None -> true
      )
end

module Node = struct

  include Node_

  (** A catamorphism which also computes focus for a given node *)
  let cata_focus focus node
      (f : Focus.view -> 'r Containers.List.t -> ('t, 'm) t -> 'r) =
    let rec run relative path node =
      let foc = {
        Focus.is_focused = Focus.is_focused relative;
        is_parent_focused = Focus.is_parent_focused relative;
        focus_relative = relative;
        path_from_root = path;
        child_focus = [];
      } in
      match node with
      | Empty -> f foc [] Empty
      | Static (tag, items) ->
        let child_results = List.mapi (fun i (_m, e) ->
            let child_relative = Focus.view_deeper relative i in
            Focus.is_focused child_relative, run child_relative (Focus.add i path) e
          ) items in
        let child_focus, aggregated = List.split child_results in
        f { foc with child_focus } aggregated node
      | Dynamic (tag, metadata, items) ->
        let child_results = List.mapi (fun i e ->
            let child_relative = Focus.view_deeper relative i in
            Focus.is_focused child_relative, run child_relative (Focus.add i path) e
          ) items in
        let child_focus, aggregated = List.split child_results in
        f { foc with child_focus } aggregated node
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
      (f : Focus.view -> ('a, 'b) t -> ('a, 'b) t) =
    let go foc children n =
      match n with
      | Empty -> f foc n
      | Static (tag, c) ->
        (* let cs = c |> List.split |> fst |> (fun m -> List.combine m children) in *)
        (* this ensures that the metadata and children lists are the same length *)
        let cs = Lens.(list_map second ^= children) c in
        f foc (Static (tag, cs))
      | Dynamic (tag, meta, _) -> f foc (Dynamic (tag, meta, children))
    in
    cata_focus focus node go

  let modify focus node insertion =
    map_focus focus node (fun foc n ->
        if foc.Focus.is_focused then
          insertion
        else
          n
      ) 

  let get focus node =
    cata_focus focus node (fun f children this ->
        if f.is_focused then
          [this]
        else
          children |> List.concat 
      )
    |> function
    | [ns] -> ns
    | _ -> raise (Invalid_argument "Node.get: focus invalid")

  let uphold_invariants node =
    (* the initial focus given here doesn't matter as it's not used in f *)
    map_focus Focus.initial node (fun _ n ->
        match n with
        | Dynamic (tag, meta, children) ->
          let c = children |> List.rev |> List.drop_while is_empty
                  |> (fun xs -> Empty :: xs)
                  |> List.rev
          in
          Dynamic (tag, meta, c)
        | _ -> n
      )
end
