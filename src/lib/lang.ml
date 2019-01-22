
open Containers
open Common

module Simpl = struct

  type t =
    | And
    | If
    | Bool of bool
    | Call of string
    | Block
  [@@deriving show]

  let draw focus text =
    (* Printf.printf "%s %s f:%s\n" text (string_of_bool (text = "true")) (string_of_bool focus); *)
    let open Notty.I in
    let s =
      if focus then
        Styles.focused
      else
        (
          match text with
          | "..." -> Styles.hole
          | "if" | "else" -> Styles.keyword
          | "&&" -> Styles.normal
          | "true" | "false" -> Styles.value
          (* this behaves weirdly when combined with the true/false case *)
          | _ when is_int text -> Styles.value
          | _ -> Styles.normal
        )
    in
    string s text

  let completions = [
    "if", Static (If, [Empty; Empty; Empty]);
    "and", Static (And, [Empty; Empty]);
  ]

  let render focus node =
    let open Notty.I in
    let f this_fc fs node =
      match node with
      | Empty -> draw this_fc "..."
      | Static (tag, _)
      | Dynamic (tag, _) ->
        match tag, fs with
        | And, [left; right] ->
          left <|> 
          (draw this_fc " && ") <|>
          right
        | If, [cond; conseq; alt] ->
          (draw this_fc "if" <|>
           (draw this_fc " (" <|>
            cond <|>
            draw this_fc ") {"))
          <->
          (conseq |> hpad 2 0)
          <->
          (draw this_fc "} " <|>
           (draw this_fc "else") <|>
           draw this_fc " {")
          <->
          (alt |> hpad 2 0)
          <->
          (draw this_fc "}")
        | Bool b, [] ->
          draw this_fc (string_of_bool b)
        | Call f, args ->
          draw this_fc f <|>
          draw this_fc "(" <|>
          hcat (List.intersperse (draw this_fc ", ") args) <|>
          draw this_fc ")"
        | Block, args ->
          List.map (fun s -> s <|> draw this_fc ";") args |> vcat
        | _ -> failwith ("invalid combination of args: " ^ show_node pp node ^ " and " ^ string_of_int (List.length fs))
    in
    with_focus focus node f

  let example = Static (If, [
      Static (And,
              [Static (Bool true, []); Static (Bool false, [])]);
      Dynamic (Block, [
          Dynamic (Call "print", [Static (Bool true, []); Static (Bool false, [])]);
          Empty;]);
      Empty;
    ])

end
