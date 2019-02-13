
open Common
open Common.Language
open Types

type t =
  | Select
  | Table
  | Cols
  | Tables
  | Where
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

type s = string list
[@@deriving show]

let no_info = []

let is_var t =
  match t with
  | Var _ -> true
  | _ -> false

let is_table t =
  match t with
  | Table -> true
  | _ -> false

let is_where t =
  match t with
  | Where -> true
  | _ -> false

let is_expression t =
  match t with
  | Int _ | Bool _ | Float _ | Var _ | String _ | Op _ -> true
  | _ -> false

let get_predicate m = m.pred

let select next =
  Node.(Static (Select, [
      { pred = fun _ -> true }, Dynamic (Cols, { pred = is_expression }, []);
      { pred = fun _ -> true }, Dynamic (Tables, { pred = is_table }, []);
      { pred = is_where }, next
    ]))

let table = Node.(Static (Table, [
    { pred = is_var }, Empty;
    { pred = is_var }, Empty;
  ]))

let op name = {
  trigger = name;
  desc = "";
  node = Node.(Static (Op name, [
      { pred = is_expression }, Empty;
      { pred = is_expression }, Empty;
    ]));
  is_literal = false;
}

let project = {
  trigger = ".";
  desc = "";
  node = Node.(Static (Op ".", [
      { pred = is_expression }, Empty;
      { pred = is_var }, Empty;
    ]));
  is_literal = false;
}

let draw focus text =
  let open Notty.I in
  let s =
    if focus then
      Styles.focused
    else
      (
        match text with
        | "..." -> Styles.hole
        | "select" | "from" | "where" -> Styles.keyword
        | "true" | "false" -> Styles.value
        | _ when is_int text -> Styles.value
        | _ -> Styles.normal
      )
  in
  string s text

let completions = Node.[
    { trigger = "select"; desc = ""; node = select Empty; is_literal = false };
    { trigger = "table"; desc = ""; node = table; is_literal = false };
    project;
    op "+";
  ]

let analyze node =
  Node.(cata_focus Focus.initial node (fun _ children this ->
      match this with
      | Static (Table, [_; _, Static (Var v, _)]) -> [v]
      | _ -> List.concat children
    ))

let dynamic_completions s =
  s |> List.map (fun v -> {
        trigger = v;
        desc = "";
        node = Node.Static (Var v, []);
        is_literal = true
      })

let guessed_completions _ = [{
    trigger = "lol";
    desc = "lol1";
    node = Static (Int (1), []);
    is_literal = true;
  }]

(* [
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
      | Select, [cols; tables; next] ->
        (draw is_parent_focused "select") <->
        (cols |> hpad 2 0) <-> 
        (draw is_parent_focused "from") <->
        (tables |> hpad 2 0) <->
        next <->
        (draw is_parent_focused ";")
      | Table, [name; alias] ->
        (name |> hpad 0 1) <|> alias
      | Cols, exprs ->
        (* TODO use snap to ensure the right layout? *)
        hlist exprs
      | Tables, tables ->
        hlist tables
      (* | Where -> *)
      | Bool b, [] ->
        draw is_parent_focused (string_of_bool b)
      | Int i, [] ->
        draw is_parent_focused (string_of_int i)
      | Float f, [] ->
        draw is_parent_focused (string_of_float f)
      | String s, [] ->
        draw is_parent_focused ("'" ^ s ^ "'")
      | Var v, [] ->
        draw is_parent_focused v
      | Op name, [left; right] ->
        let res = [left; draw is_parent_focused name; right] in
        hcat (match name with
            | "." -> res
            | _ -> List.intersperse (draw is_parent_focused " ") res)
      | _ -> failwith ("invalid combination of args: " ^ Node.show pp pp_m node ^ " and " ^ string_of_int (List.length fs))
  in
  Node.cata_focus focus node f

let example = Node.(select Empty)