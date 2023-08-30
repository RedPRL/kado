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

    (** Equality checker for elements in the interval algebra. *)
    val equal_dim : dim -> dim -> bool

    (** The embedding of cofibrations into [cof]. *)
    val cof : (dim, cof) Syntax.endo -> cof

    (** Extract the embedded cofibration, if any. *)
    val uncof : cof -> (dim, cof) Syntax.endo option
  end

  (** The signature of smart constructors. *)
  module type S =
  sig
    module Param : sig
      (** The interval algebra. *)
      type dim

      (** The type that embeds cofibrations. *)
      type cof
    end
    open Param

    (** The type of built cofibrations. *)
    type t = cof

    (** The embedding of cofibrations to [cof]. *)
    val cof : (dim, t) Syntax.endo -> t

    (** Smarter version of {!val:Syntax.Endo.le}. *)
    val le : dim -> dim -> t

    (** The bottom cofibration. *)
    val bot : t

    (** The top cofibration. *)
    val top : t

    (** Smarter version of {!val:Syntax.Endo.join} that simplifies cofibrations using syntactic criteria.
        For example, [join [meet []]] gives [cof (Meet [])].

        Note that the simplification is attempting to strike a balance between optimality and efficiency,
        and thus it will not perform all possible syntactic reduction. To obtain more reduced cofibrations,
        use only smart constructors (instead of raw constructors) to build cofibrations.
    *)
    val join : t list -> t

    (** Smarter version of {!val:Syntax.Endo.meet} that simplifies cofibrations using syntactic criteria.
        See {!val:join}. *)
    val meet : t list -> t

    (** [eq] is equivalent to [meet [le x y; le y x]]. *)
    val eq : dim -> dim -> t

    (** [eq0 r] is equivalent to [eq r dim0]. *)
    val eq0 : dim -> t

    (** [eq1 r] is equivalent to [eq r dim1]. *)
    val eq1 : dim -> t

    (** [boundary r] is equivalent to [join [eq0 r; eq1 r]]. *)
    val boundary : dim -> t

    (** [forall (r, cof)] computes [forall r. cof], using the syntactic quantifier elimination
        and potentially other simplification procedures used in {!val:le}, {!val:join}, and {!val:meet}.

        Note: [r] cannot be [dim0] or [dim1].
    *)
    val forall : dim * t -> t
  end

  (** The implementation of smart constructors. *)
  module Make (Param : Param) : S with module Param := Param
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
    module Param : sig
      (** The interval algebra. *)
      type dim

      (** The type of cofibration variables. *)
      type var
    end
    open Param

    (** @open *)
    include Endo.S with type Param.cof := (dim, var) Syntax.free and module Param := Param

    (** Alias of {!val:Syntax.Free.var}. *)
    val var : var -> t
  end

  (** The implementation of smart constructors. *)
  module Make (Param : Param) : S with module Param := Param
end
