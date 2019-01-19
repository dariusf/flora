
open Common

module Simpl = struct

  type t =
    | And
    | If
  [@@deriving show]

  let draw focus text =
    let open Notty.I in
    let s =
      if focus then
        Styles.focused
      else
        match text with
        | "..." -> Styles.hole
        | "if" | "else" -> Styles.keyword
        | "&&" -> Styles.normal
        | _ -> Styles.normal
    in
    string s text

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
        | _ -> failwith ("invalid combination of args: " ^ show_node pp node ^ " and " ^ string_of_int (List.length fs))
    in
    with_focus focus node f

  let example = Static (If, [
      Static (And, [Empty; Empty]);
      Empty;
      Empty;
    ])

end
