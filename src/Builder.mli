(** Smart constructors for {!type:Syntax.endo}. *)
module Endo :
sig

  (** Parameters of smart constructors. *)
  module type Param =
  sig
    (** The interval algebra. *)
    type dim

    (** The type that embeds cofibrations. *)
    type cof

    (** The element 0 in the interval algebra. *)
    val dim0 : dim

    (** The element 1 in the interval algebra. *)
    val dim1 : dim

    (** Equality checker for elements of the interval algebra. *)
    val equal_dim : dim -> dim -> bool

    (** The embedding of cofibrations to [cof]. *)
    val cof : (dim, cof) Syntax.endo -> cof

    (** Extract the embedded cofibration, if any. *)
    val uncof : cof -> (dim, cof) Syntax.endo option
  end

  (** The signature of smart constructors. *)
  module type S =
  sig
    (** The interval algebra. *)
    type dim

    (** The type that embeds cofibrations. *)
    type cof

    (** The embedding of cofibrations to [cof]. *)
    val cof : (dim, cof) Syntax.endo -> cof

    (** Smarter version of {!val:Syntax.Endo.eq} that checks equality. *)
    val eq : dim -> dim -> cof

    (** [eq0 r] is [eq r dim0]. *)
    val eq0 : dim -> cof

    (** [eq1 r] is [eq r dim1]. *)
    val eq1 : dim -> cof

    (** The bottom cofibration. *)
    val bot : cof

    (** The top cofibration. *)
    val top : cof

    (** Smarter version of {!val:Syntax.Endo.join} that simplifies cofibrations using syntactic criteria.
        For example, [join [meet []]] gives [cof (Meet [])].

        Note that the simplification is attempting to strike a balance between optimality and efficiency,
        and thus it will not perform all possible syntactic reduction. For the best result,
        use only smart constructors (instead of raw constructors) to build cofibrations.
    *)
    val join : cof list -> cof

    (** Smarter version of {!val:Syntax.Endo.meet} that simplifies cofibrations using syntactic criteria.
        See {!val:join}. *)
    val meet : cof list -> cof

    (** [boundary r] gives a cofibration equivalent to [join [eq0 r; eq1 r]] *)
    val boundary : dim -> cof

    (** [forall (r, cof)] computes [forall r. cof], using the syntactic quantifier elimination
        and potentially other simplification used in {!val:eq}, {!val:join}, and {!val:meet}. *)
    val forall : dim * cof -> cof
  end

  (** The implementation of smart constructors. *)
  module Make (P : Param) : S with type dim = P.dim and type cof = P.cof
end

(** Smart constructors for {!type:Syntax.free}. *)
module Free :
sig

  (** Parameters of smart constructors. *)
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

    (** Equality checker for elements of the interval algebra. *)
    val equal_dim : dim -> dim -> bool
  end

  (** The signature of smart constructors. *)
  module type S =
  sig
    (** The interval algebra. *)
    type dim

    (** The type of cofibration variables. *)
    type var

    (** The type of freely constructed cofibrations. *)
    type cof = (dim, var) Syntax.free

    (** Alias of {!val:Syntax.Free.var}. *)
    val var : var -> cof

    (** @open *)
    include Endo.S with type dim := dim and type cof := cof
  end

  (** The implementation of smart constructors. *)
  module Make (P : Param) : S with type dim = P.dim and type var = P.var
end
