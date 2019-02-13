
open Common
open Types

type t =
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

type m = {
  pred: t -> bool;
}
[@@deriving show]

type s = ()
[@@deriving show]

let no_info = ()

let draw focus text =
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

let is_expression t =
  match t with
  | Call _ | Int _ | Bool _ | Float _ | Var _ | String _ | Op _ -> true
  | If | Block -> false

let is_statement n =
  match n with
  | If | Block | Call _ -> true
  | Int _ | Bool _ | Float _ | Var _ | String _ | Op _ -> false

let get_predicate m = m.pred

let op n = 
  { trigger = n;
    desc = n;
    node = Node.Static (Op n, [
        { pred = is_expression; }, Empty;
        { pred = is_expression; }, Empty;
      ]);
    is_literal = false }

let completions = Node.[
    { trigger = "if";
      desc = "";
      node = Static (If, [
          { pred = is_expression; }, Empty;
          { pred = is_statement; }, Empty;
          { pred = is_statement; }, Empty;
        ]);
      is_literal = true;
    };
    op "&&";
    op "=";
    op "+";
  ]

let dynamic_completions _ = []

let analyze _ = ()

let guessed_completions _ = [{
    trigger = "lol";
    desc = "lol1";
    node = Static (Int (1), []);
    is_literal = true;
  }]

(* let guessed_completions = [
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
   ] |> List.map (fun c -> Fun.compose c (Option.map (fun b -> Node.Static (b, [])))) *)

let render focus node =
  let open Notty.I in
  let f Focus.{ is_parent_focused } fs node =
    match node with
    | Node.Empty -> draw is_parent_focused "..."
    | Static (tag, _)
    | Dynamic (tag, _, _) ->
      match tag, fs with
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
      | _ -> failwith ("invalid combination of args: " ^ Node.show pp pp_m node ^ " and " ^ string_of_int (List.length fs))
  in
  Node.cata_focus focus node f

let example = Node.(Static (If, [{ pred = is_expression }, Empty;
                                 { pred = is_statement }, Dynamic (Block, { pred = is_statement }, [
                                     Dynamic (Call "print", { pred = is_expression }, [Static (Bool true, []); Static (Bool false, [])]);
                                   ]);
                                 { pred = is_statement }, Empty ]))
