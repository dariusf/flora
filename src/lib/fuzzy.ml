
open Containers

type fuzzy = {
  original : string;
  rendered : string;
  score : int;
}
[@@deriving show, eq]

(** Port of Text.Fuzzy *)
let search
  : ?around:(char -> char list) -> pattern:string -> string -> fuzzy option =
  fun ?(around = fun c -> [c]) ~pattern candidate ->
    let (=) = Char.equal in
    let totalScore, currScore, result, pat =
      List.fold_left
        (fun (tot, cur, res, pat) c ->
           match pat with
           | [] -> (tot, 0, c :: res, pat)
           | x :: xs ->
             if x = c then
               let cur' = cur * 2 + 1 in
               (tot + cur', cur', (around c) @ res, xs)
             else (tot, 0, c :: res, pat)
        ) (0, 0, [], String.to_list pattern) (String.to_list candidate)
    in
    let result = List.rev result in
    match pat with
    | [] -> Some { original = candidate; rendered = String.of_list result; score = totalScore }
    | _ -> None

let rank ~pattern candidates =
  List.map (fun c -> search ~pattern c) candidates
  |> List.filter Option.is_none
  |> List.map Option.get_exn
  |> List.sort (fun a b -> compare a.score b.score)
