(** The [antikado] library implements the syntax and semantics of cofibrations as described by {{:https://doi.org/10.1017/S0960129521000347} ABCFHL}. *)

(** {1 Syntax} *)

(** The abstract syntax of the restricted predicate logic of cofibrations. *)

module CofFun : module type of CofFun

module Cof : module type of Cof

(** Cofibration smart constructors. *)

module BuilderFun : module type of BuilderFun

module Builder : module type of Builder

(** {1 Solver} *)

(** The {!module:CofThy} module implements decision procedures for sequents relative to a theory over the interval, stated in the language of cofibrations. *)
module CofThy : module type of CofThy
