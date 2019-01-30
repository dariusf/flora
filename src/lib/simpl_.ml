
open Containers
open Common

type t =
  | And
  | If
  | Call of string
  | Block
  | Int of int
  | Bool of bool
  | Float of float
  | Var of string
  | String of string
  | Op of string
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

let is_expression n =
  match Node.tag n with
  | Call _ | Int _ | Bool _ | Float _ | Var _ | String _ | Op _ | And -> true
  | If | Block -> false

let is_statement n = not (is_expression n)

let generate_holes t =
  let open Node in
  match t with
  | Block -> empty ~pred:is_statement ()
  | Call _ -> empty ~pred:is_expression ()
  | _ -> empty ()

let completions = Node.[
    "if", Static (If, [
        empty ~pred:is_expression ();
        empty ~pred:is_statement ();
        empty ~pred:is_statement ()
      ]);
    "and", Static (And, [
        empty ~pred:is_expression ();
        empty ~pred:is_expression ()
      ]);
    "&&", Static (Op "&&", [
        empty ~pred:is_expression ();
        empty ~pred:is_expression ()
      ]);
    "=", Static (Op "=", [
        empty ~pred:is_expression ();
        empty ~pred:is_expression ()
      ]);
    "+", Static (Op "+", [
        empty ~pred:is_expression ();
        empty ~pred:is_expression ()
      ]);
  ]

let guessed_completions = [
  Option.wrap (fun i -> Int (int_of_string i));
  Option.wrap (fun f -> Float (float_of_string f));
  Option.wrap (fun b -> Bool (bool_of_string b));
  Option.wrap (fun s ->
      let l = String.length s in
      if Char.(equal s.[0] '"' && equal s.[l - 1] '"') then
        String (String.sub s 1 (l - 2))
      else
        raise (Invalid_argument "not a string"));
  Option.wrap (fun s ->
      let l = String.length s in
      if Char.(equal s.[0] '"' && equal s.[l - 1] '"') then
        String (String.sub s 1 (l - 2))
      else
        raise (Invalid_argument "not a string"));
  (fun v ->
     if not (String.is_empty v) && String.for_all (fun l -> is_letter l || Char.equal l '_') v then
       Some (Var v)
     else 
       None);
] |> List.map (fun c -> Fun.compose c (Option.map (fun b -> Node.Static (b, []))))

let render focus node =
  let open Notty.I in
  let f Focus.{ is_parent_focused } fs node =
    match node with
    | Node.Empty _ -> draw is_parent_focused "..."
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
      | Int i, [] ->
        draw is_parent_focused (string_of_int i)
      | Float f, [] ->
        draw is_parent_focused (string_of_float f)
      | String s, [] ->
        draw is_parent_focused ("\"" ^ s ^ "\"")
      | Var v, [] ->
        draw is_parent_focused v
      | Call f, args ->
        draw is_parent_focused f <|>
        draw is_parent_focused "(" <|>
        hcat (List.intersperse (draw is_parent_focused ", ") args) <|>
        draw is_parent_focused ")"
      | Op name, [left; right] ->
        hcat (List.intersperse (draw is_parent_focused " ") [
            left;
            draw is_parent_focused name;
            right])
      | Block, args ->
        List.map (fun s -> s <|> draw is_parent_focused ";") args |> vcat
      | _ -> failwith ("invalid combination of args: " ^ Node.show pp node ^ " and " ^ string_of_int (List.length fs))
  in
  Node.cata_focus focus node f

let example = Node.(Static (If, [
    Static (And,
            [Static (Bool true, []); Static (Bool false, [])]);
    Dynamic (Block, [
        Dynamic (Call "print", [Static (Bool true, []); Static (Bool false, [])]);
        empty ();]);
    empty ();
  ]))

let example = Node.(Static (If, [empty ~pred:is_expression ();
                                 Dynamic (Block, [
                                     Dynamic (Call "print", [Static (Bool true, []); Static (Bool false, [])]);
                                   ]);
                                 empty ~pred:is_statement ()]))
