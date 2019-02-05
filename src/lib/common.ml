
include Containers
include Lens.Infix

module Deque = CCFQueue

let is_int s =
  try ignore (int_of_string s); true
  with _ -> false

let clamp ~high ~low n =
  Int.(max low (min high n))

let min_int = Int.(2 ** 62);;
let max_int = Int.((2 ** 62) - 1);;

module Char_ = struct
  include Char
  let (<=) a b = Char.compare a b <= 0
end

let is_uppercase c = Char_.('A' <= c && c <= 'Z')
let is_lowercase c = Char_.('a' <= c && c <= 'z')
let is_letter c =
  is_uppercase c || is_lowercase c

let init xs =
  match xs with
  | [] -> None
  | _ ->
    let l = List.length xs - 1 in
    Some (List.take_drop l xs)

module Styles = struct
  open Notty.A
  let keyword = fg red
  let insert_mode = bg green ++ fg black
  let normal_mode = bg yellow ++ fg black
  let hole = fg lightblack
  let normal = empty
  let focused = bg yellow ++ fg black
  let value = fg (Notty.A.rgb_888 147 112 219)
  let cursor = bg lightblack
  let highlighted = fg red
end

module Language = struct

  let hlist items =
    match items with
    | [] -> raise (Invalid_argument "hlist: empty list")
    | [x] -> x
    | _ ->
      let f, s = init items |> Option.get_exn in
      ((f |> List.map Notty.I.(fun i -> i <|> string Styles.normal ",")) @ s)
      |> Notty.I.vcat

end

let logfile = open_out "log.ignore"
let log s =
  IO.write_line logfile s; flush logfile
