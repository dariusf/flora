
open Common
open Types

module type Def = sig
  type t
  [@@deriving show, eq]

  type m
  [@@deriving show]

  val completions : (string * (t, m) Node.t) list
  val guessed_completions : (string -> (t, m) Node.t option) list
  val render : Focus.t -> (t, m) Node.t -> Notty.image

  (* TODO maybe move metadata up to this interface *)
  val get_predicate : m -> (t -> bool)
  val example : (t, m) Node.t
end

module Simpl : Def = Simpl_
module Sql : Def = Sql_
