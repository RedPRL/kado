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

    (** The point 0 in the interval algebra. *)
    val dim0 : dim

    (** The point 1 in the interval algebra. *)
    val dim1 : dim

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

    (** Smarter version of {!val:Syntax.Endo.eq} that checks equality. *)
    val eq : dim -> dim -> cof

    (** Smarter version of {!val:Syntax.Endo.bot} that is actually the same. *)
    val bot : cof

    (** Smarter version of {!val:Syntax.Endo.top} that is actually the same. *)
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

    (** [boundary r] gives a cofibration equivalent to [join [eq r dim0; eq r dim1]] *)
    val boundary : dim -> cof
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

    (** The point 0 in the interval algebra. *)
    val dim0 : dim

    (** The point 1 in the interval algebra. *)
    val dim1 : dim
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

    (** @open *)
    include Endo.S with type dim := dim and type cof := cof
  end

  (** The implementation of smart constructors. *)
  module Make (P : Param) : S with type dim = P.dim and type var = P.var
end