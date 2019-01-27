
open Common
open Containers

module type T = sig
  type intermediate
  type t
  val postprocess : intermediate -> t
  val snoc : intermediate -> intermediate -> intermediate
  val injc : char -> intermediate
  val empty : intermediate
end

module F (T : T) = struct

  type fuzzy = {
    original : string;
    rendered : T.t;
    score : int;
  }

  (** Port of Text.Fuzzy *)
  let search
    : ?around:(char -> T.intermediate) -> pattern:string -> string -> fuzzy option =
    fun ?(around = fun c -> T.injc c) ~pattern candidate ->
      let (=) = Char.equal in
      let totalScore, currScore, result, pat =
        List.fold_left
          (fun (tot, cur, res, pat) c ->
             match pat with
             | [] -> (tot, 0, T.snoc res (T.injc c), pat)
             | x :: xs ->
               if x = c then
                 let cur' = cur * 2 + 1 in
                 (tot + cur', cur', T.snoc res (around c), xs)
               else (tot, 0, T.snoc res (T.injc c), pat)
          ) (0, 0, T.empty, String.to_list pattern) (String.to_list candidate)
      in
      let result = T.postprocess result in
      match pat with
      | [] -> Some { original = candidate; rendered = result; score = totalScore }
      | _ -> None

  let rank ~around ~pattern candidates =
    List.map (fun c -> search ~around ~pattern c) candidates
    |> List.filter Option.is_some
    |> List.map Option.get_exn
    |> List.sort (fun a b -> compare a.score b.score)

end

module String = F (struct
    type intermediate = char list
    type t = string
    let postprocess r = r |> List.rev |> String.of_list
    let snoc res e = e @ res
    let injc c = [c]
    let empty = []
  end)

module Image = F (struct
    open Notty.I
    type intermediate = Notty.image list
    type t = Notty.image
    let postprocess r = r |> List.rev |> hcat
    let snoc res e = e @ res
    let injc c = [Notty.I.string Styles.normal (Containers.String.of_char c)]
    let empty = []
  end)
