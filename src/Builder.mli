(** Parameters of smart constructors *)
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

(** The signature of smart constructors *)
module type S =
sig
  (** The interval algebra. *)
  type dim

  (** The type of cofibration variables. *)
  type var

  (** The type of freely constructed cofibrations. *)
  type cof = (dim, var) Cof.t

  (** @open *)
  include BuilderFun.S with type dim := dim and type cof := cof
end

(** The implementation of smart constructors *)
module Make (P : Param) : S with type dim = P.dim and type var = P.var
