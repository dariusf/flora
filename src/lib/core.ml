
open Common
open Types
open Lang

(* TODO if completions match one thing, just commit recursively? *)
let match_completions pred hole query completions =
  completions
  |> List.filter (fun { node } -> pred (Node.tag node))
  |> List.map (fun { trigger } -> trigger)
  (* TODO put the desc in *)
  |> Fuzzy.Image.rank ~around:(fun c -> [Notty.I.string Styles.highlighted (String.of_char c)]) ~pattern:query
  |> List.map (fun f -> f.Fuzzy.Image.original, f.rendered)

let parse_completions : string -> (string -> ('a, 'b) Node.t option) list -> ('a, 'b) Node.t list = fun term more ->
  let open Option in
  List.fold_left (fun t c ->
      match t with
      | None -> c term
      | Some _ -> t
    ) None more |> to_list

module Lang = Lang.Simpl
(* module Lang = Lang.Sql *)

type node = (Lang.t, Lang.m) Node.t
type focus = Focus.t

let debug_node n = Node.show Lang.pp Lang.pp_m n

(** crashes if given an invalid focus *)
let get_with_predicate focus node =
  Node.cata_focus focus node (fun f children this ->
      if f.is_focused then
        (* this is the node. the None will be filled in when
           traversing back up to the parent *)
        [None, this]
      else
        (* from here, either a child is focused (in which case we propagate),
           or nothing is (in which case we fail) *)
        match this with
        | Empty -> []
        | Static (tag, cs) ->
          (match children |> List.concat with
           | [None, c] ->
             (* figure out which child was in focus so we can get the right metadata *)
             let m = List.find_idx Fun.id f.child_focus |> Option.map fst
                     |> Option.map (List.nth cs)
                     |> Option.map fst
             in [m, c]
           | [meta, c] -> [meta, c]
           | _ -> []
          )
        | Dynamic (tag, m, _) ->
          (match children |> List.concat with
           | [None, c] -> [Some m, c] (* immediate child *)
           | [meta, c] -> [meta, c] (* descendent *)
           | _ -> [])
    )
  |> fun n ->
  match n with
  | [m, c] -> m, c
  | _ ->
    raise (Invalid_argument ("get_with_predicate: focus invalid " ^
                             Focus.show focus ^ " " ^
                             debug_node node))
