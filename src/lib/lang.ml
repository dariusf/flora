
open Common
open Types

module type Def = sig
  type t
  [@@deriving show, eq]

  type m
  [@@deriving show]

  val completions : (string * (t, m) Node.t) list
  val dynamic_completions : (t, m) Node.t -> (string * (t, m) Node.t) list
  val guessed_completions : (string -> (t, m) Node.t option) list
  val render : Focus.t -> (t, m) Node.t -> Notty.image

  (* TODO maybe move metadata up to this interface *)
  val get_predicate : m -> (t -> bool)
  val example : (t, m) Node.t
end

module type Definition = sig
  include Def
  val all_completions : (t, m) Node.t -> (string * (t, m) Node.t) list
end

module BaseDefinition (F : Def) : Definition = struct
  include F
  let all_completions node = completions @ dynamic_completions node
end

module Simpl : Definition = BaseDefinition (Simpl_)
module Sql : Definition = BaseDefinition (Sql_)

(** {1:compl The Completion Pipeline}

    First possible completions for the hole in focus are generated:
    - {{!type:Def.completions}static}
    - {{!type:Def.dynamic_completions}AST-based}
    - {{!type:Def.guessed_completions}input-based}

    These are then filtered based on:
    - the committed input
    - predicates on the hole
    - semantic checks

*)