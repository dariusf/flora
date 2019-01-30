
open Containers
open Common

module type Def = sig
  type t
  [@@deriving show, eq]

  (* val draw : bool -> string -> Notty.image *)
  val completions : (string * t node) list
  val guessed_completions : (string -> t node option) list
  val generate_holes : t -> t node
  val render : focus -> t node -> Notty.image
  val example : t node
end

module Simpl : Def = Simpl_