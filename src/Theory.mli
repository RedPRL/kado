(** Parameters of cofibration solvers. *)
module type Param =
sig
  (** The interval algebra. *)
  type dim

  (** The type of cofibration variables. *)
  type var

  (** The element 0 in the interval algebra. *)
  val dim0 : dim

  (** The element 1 in the interval algebra. *)
  val dim1 : dim

  (** Comparator for elements of the interval algebra. *)
  val compare_dim : dim -> dim -> int

  (** Comparator for cofibration variables. *)
  val compare_var : var -> var -> int
end

(** The signature of cofibration solvers. *)
module type S =
sig

  (** The type of dimensions. *)
  type dim

  (** The type of cofibration variables. *)
  type var

  (** The type of cofibrations. *)
  type cof = (dim, var) Syntax.free

  (** The type of an algebraic theory (no unreduced joins). *)
  type alg_thy

  (** The type of a disjunctive theory. *)
  type disj_thy

  (** Algebraic theories over the interval. *)
  module Alg :
  sig
    (** The type of an algebraic theory (no unreduced joins). *)
    type t = alg_thy

    (** The empty theory. *)
    val empty : t

    (** [consistency thy] returns the consistency of the theory [thy]. *)
    val consistency : t -> [`Consistent | `Inconsistent]

    (** [split thy cofs] returns irreducible joins under additional cofibrations [cofs]. *)
    val split : t -> cof list -> t list

    (** [meet2 thy1 thy2] computes the conjunction of the two theories [thy1] and [thy2].
        This is useful for supporting compilation units with top-level cofibration declarations. *)
    val meet2 : t -> t -> t
  end

  (** Disjunctive theories over the interval. *)
  module Disj :
  sig
    (** The type of a disjunctive theory. *)
    type t = disj_thy

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

    (** [simplify_cof thy cof] returns a potentially simplified cofibration that is equivalent to [cof].
        This is useful for displaying sensible cofibrations to users.
        While it will remove useless cofibrations such as [r=r] and [0=1],
        it does not perform non-local simplification such as reducing [x=1 /\ x=1] to [x=1].
        Also, the simplification is an expensive process and should be used wisely. *)
    val simplify_cof : t -> cof -> cof

    (** [forall_cof thy (r, cof)] computes [forall r. cof] with respect to the equations in the theory [thy],
        using the syntactic quantifier elimination and potentially other simplification procedures
        used in {!val:Builder.Endo.S.eq}, {!val:Builder.Endo.S.join}, and {!val:Builder.Endo.S.meet}. This is slower than {!val:Builder.Endo.S.forall}
        which does not take equations into consideration.

        Note: this is experimental and might be removed if it turns out that we do not need it. *)
    val forall_cof : t -> dim * cof -> cof

    (** [meet2 thy1 thy2] computes the conjunction of the two theories [thy1] and [thy2].
        This is useful for supporting compilation units with top-level cofibration declarations. *)
    val meet2 : t -> t -> t
  end
end

(** The cofibration solver. *)
module Make (P : Param) : S with type dim = P.dim and type var = P.var
