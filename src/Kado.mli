(** The [antikado] library implements the syntax and semantics of cofibrations as described by {{:https://doi.org/10.1017/S0960129521000347} ABCFHL}. *)

(** {1 Syntax} *)

(** The abstract syntax of the restricted predicate logic of cofibrations. *)
module Syntax : module type of Syntax

(** Smart constructors. *)
module Builder : module type of Builder

(** {1 Decision Procedures} *)

(** The {!module:Theory} module implements decision procedures for sequents relative to a theory over the interval, stated in the language of cofibrations. *)
module Theory : module type of Theory

(**/**)

module Graph : module type of Graph
