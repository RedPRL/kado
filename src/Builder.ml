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

  include BuilderFun.S with type dim := dim and type cof := cof
end

module Make (P : Param) : S with type dim = P.dim and type var = P.var =
struct
  module P = struct
    include P
    type cof = (dim, var) Cof.t
    let cof phi = Cof.Cof phi
    let uncof phi = match phi with Cof.Cof phi -> Some phi | _ -> None
  end

  include P
  include BuilderFun.Make(P)
end
