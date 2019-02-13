
open Common
open Types

module type Def = sig
  (** A language's characteristic tag for AST nodes *)
  type t
  [@@deriving show, eq]

  (** Metadata associated with each hole *)
  type m
  [@@deriving show]

  (** Semantic information *)
  type s
  [@@deriving show]

  val completions : (t, m) completion list
  val dynamic_completions : s -> (t, m) completion list
  (* val guessed_completions : (string -> (t, m) Node.t option) list *)
  val guessed_completions : string -> (t, m) completion list
  val render : Focus.t -> (t, m) Node.t -> Notty.image
  val analyze : (t, m) Node.t -> s
  val no_info : s

  (* TODO maybe move metadata up to this interface *)
  val get_predicate : m -> (t -> bool)
  val example : (t, m) Node.t
end

module type Definition = sig
  include Def
  val all_completions : s -> string -> (t, m) completion list
  (* (string * (t, m) Node.t) *)
end

module BaseDefinition (F : Def) : Definition = struct
  include F
  let all_completions s i =
    completions @
    dynamic_completions s @
    guessed_completions i
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