(** Parameters of cofibration solvers. *)
module type Param =
sig
  (** The interval algebra *)
  module Dim : sig
    include Map.OrderedType
    val dim0 : t
    val dim1 : t
  end

  (** The cofibration variables *)
  module Var : Map.OrderedType
end

(** The signature of cofibration solvers. *)
module type S =
sig

  (** The type of dimensions. *)
  type dim

  (** The type of cofibration variables. *)
  type var

  (** The type of cofibrations. *)
  type cof = (dim, var) Cof.t

  (** Algebraic theories over the interval. *)
  module Alg :
  sig
    (** The type of an algebraic theory (no unreduced joins). *)
    type t

    (** The empty theory. *)
    val empty : t

    (** Returns the consistency of the theory. *)
    val consistency : t -> [`Consistent | `Inconsistent]

    (** [split thy cofs] returns irreducible joins under additional cofibrations [cofs]. *)
    val split : t -> cof list -> t list

    (** Compute the conjunction of two theories. *)
    val meet2 : t -> t -> t
  end

  (** Disjunctive theories over the interval. *)
  module Disj :
  sig
    (** The type of a disjunctive theory. *)
    type t

    (** The empty theory. *)
    val empty : t

    (** Construct the enveloping disjunctive theory of an algebraic theory. *)
    val envelope_alg : Alg.t -> t

    (** Returns the consistency of the theory. *)
    val consistency : t -> [`Consistent | `Inconsistent]

    (** Assumes the truth of a cofibration. *)
    val assume : t -> cof list -> t

    (** Tests the validity of a sequent against the supplied theory. Equivalent to assuming
        the conjunction of the context and then testing truth. *)
    val test_sequent : t -> cof list -> cof -> bool

    (** [split_alg] returns irreducible joins as algebraic theories. *)
    val split_alg : t -> Alg.t list

    (** Compute the conjunction of two theories. *)
    val meet2 : t -> t -> t
  end
end

(** The cofibration solver. *)
module Make (P : Param) : S with type dim = P.Dim.t and type var = P.Var.t
