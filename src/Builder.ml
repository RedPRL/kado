module type Param =
sig
  type dim
  type var
  val dim0 : dim
  val dim1 : dim
end

module type S =
sig
  type dim
  type var
  type cof = (dim, var) Cof.t

  val eq : dim -> dim -> cof
  val bot : cof
  val top : cof

  val join : cof list -> cof
  val meet : cof list -> cof
  val boundary : dim -> cof
end

module Make (P : Param) : S with type dim = P.dim and type var = P.var =
struct
  type var = P.var
  include BuilderFun.Make(
    struct
      type dim = P.dim
      let dim0 = P.dim0
      let dim1 = P.dim1
      type cof = (P.dim, P.var) Cof.t
      let cof phi = Cof.Cof phi
      let uncof phi = match phi with Cof.Cof phi -> Some phi | _ -> None
    end)
end
