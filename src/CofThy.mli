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

    (** [consistency thy] returns the consistency of the theory [thy]. *)
    val consistency : t -> [`Consistent | `Inconsistent]

    (** [split thy cofs] returns irreducible joins under additional cofibrations [cofs]. *)
    val split : t -> cof list -> t list

    (** [meet2 thy1 thy2] computes the conjunction of the two theories [thy1] and [thy2]. *)
    val meet2 : t -> t -> t
  end

  (** Disjunctive theories over the interval. *)
  module Disj :
  sig
    (** The type of a disjunctive theory. *)
    type t

    (** The empty theory. *)
    val empty : t

    (** [consistency thy] returns the consistency of the theory [thy]. *)
    val consistency : t -> [`Consistent | `Inconsistent]

    (** [assume thy cofs] assumes [cofs] and returns the new theory. *)
    val assume : t -> cof list -> t

    (** [test_sequent thy context cof] tests the validity of a sequent against the supplied theory.
        Equivalent to assuming the cofibrations [context] and then testing truthness of [cof]. *)
    val test_sequent : t -> cof list -> cof -> bool

    (** [envelope_alg thy] constructs the enveloping disjunctive theory of the algebraic theory [thy]. *)
    val envelope_alg : Alg.t -> t

    (** [decompose thy] returns irreducible joins as algebraic theories. *)
    val decompose : t -> Alg.t list

    (** [meet2 thy1 thy2] computes the conjunction of the two theories [thy1] and [thy2]. *)
    val meet2 : t -> t -> t
  end
end

(** The cofibration solver. *)
module Make (P : Param) : S with type dim = P.Dim.t and type var = P.Var.t
