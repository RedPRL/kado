module type GenericParam =
sig
  type dim
  type var
end

module GenericMake (P : GenericParam) :
sig
  val eq : P.dim -> P.dim -> (P.dim, P.var) Cof.t
  val bot : (P.dim, P.var) Cof.t
  val top : (P.dim, P.var) Cof.t

  val join : (P.dim, P.var) Cof.t list -> (P.dim, P.var) Cof.t
  val meet : (P.dim, P.var) Cof.t list -> (P.dim, P.var) Cof.t
end
=
struct
  include BuilderFun.GenericMake(
    struct
      include P
      type cof = (P.dim, P.var) Cof.t
      let cof phi = Cof.Cof phi
      let uncof phi = match phi with Cof.Cof phi -> Some phi | _ -> None
    end)
end

module type Param =
sig
  type dim
  type var
  val dim0 : dim
  val dim1 : dim
end

module Make (P : Param) :
sig
  val eq : P.dim -> P.dim -> (P.dim, P.var) Cof.t
  val bot : (P.dim, P.var) Cof.t
  val top : (P.dim, P.var) Cof.t

  val join : (P.dim, P.var) Cof.t list -> (P.dim, P.var) Cof.t
  val meet : (P.dim, P.var) Cof.t list -> (P.dim, P.var) Cof.t
  val boundary : P.dim -> (P.dim, P.var) Cof.t
end
=
struct
  include BuilderFun.Make(
    struct
      include P
      type cof = (P.dim, P.var) Cof.t
      let cof phi = Cof.Cof phi
      let uncof phi = match phi with Cof.Cof phi -> Some phi | _ -> None
    end)
end
