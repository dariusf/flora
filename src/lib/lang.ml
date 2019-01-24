
open Containers
open Common

module type Def = sig
  type t
  [@@deriving show, eq]

  (* val draw : bool -> string -> Notty.image *)
  val completions : (string * t Common.node) list
  val render : Focus.t -> t Common.node -> Notty.image
  val example : t Common.node
end

module Simpl : Def = struct

  type t =
    | And
    | If
    | Bool of bool
    | Call of string
    | Block
  [@@deriving show, eq]

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
    let f { is_parent_focused } fs node =
      match node with
      | Empty -> draw is_parent_focused "..."
      | Static (tag, _)
      | Dynamic (tag, _) ->
        match tag, fs with
        | And, [left; right] ->
          left <|> 
          (draw is_parent_focused " && ") <|>
          right
        | If, [cond; conseq; alt] ->
          (draw is_parent_focused "if" <|>
           (draw is_parent_focused " (" <|>
            cond <|>
            draw is_parent_focused ") {"))
          <->
          (conseq |> hpad 2 0)
          <->
          (draw is_parent_focused "} " <|>
           (draw is_parent_focused "else") <|>
           draw is_parent_focused " {")
          <->
          (alt |> hpad 2 0)
          <->
          (draw is_parent_focused "}")
        | Bool b, [] ->
          draw is_parent_focused (string_of_bool b)
        | Call f, args ->
          draw is_parent_focused f <|>
          draw is_parent_focused "(" <|>
          hcat (List.intersperse (draw is_parent_focused ", ") args) <|>
          draw is_parent_focused ")"
        | Block, args ->
          List.map (fun s -> s <|> draw is_parent_focused ";") args |> vcat
        | _ -> failwith ("invalid combination of args: " ^ show_node pp node ^ " and " ^ string_of_int (List.length fs))
    in
    cata_focus focus node f

  let example = Static (If, [
      Static (And,
              [Static (Bool true, []); Static (Bool false, [])]);
      Dynamic (Block, [
          Dynamic (Call "print", [Static (Bool true, []); Static (Bool false, [])]);
          Empty;]);
      Empty;
    ])

  let example = Static (If, [Empty;
                             Dynamic (Block, [
                                 Dynamic (Call "print", [Static (Bool true, []); Static (Bool false, [])]);
                                 Empty;]);
                             Empty])

end
