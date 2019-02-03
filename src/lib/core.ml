
open Common
open Types

(* TODO if completions match one thing, just commit recursively? *)
let match_completions pred hole query completions =
  completions
  |> List.filter (fun (_, n) -> pred (Node.tag n)) |> List.map fst
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

let debug_node n = log @@ Node.show Lang.pp Lang.pp_m n
