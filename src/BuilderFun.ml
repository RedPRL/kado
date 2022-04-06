module type GenericParam =
sig
  type dim
  type cof
  val cof : (dim, cof) CofFun.t -> cof
  val uncof : cof -> (dim, cof) CofFun.t option
end

module GenericMake (P : GenericParam) :
sig
  val eq : P.dim -> P.dim -> P.cof
  val bot : P.cof
  val top : P.cof

  val join : P.cof list -> P.cof
  val meet : P.cof list -> P.cof
end
=
struct
  open P

  let eq x y = cof @@ if x == y then CofFun.top else CofFun.eq x y
  let bot = cof CofFun.bot
  let top = cof CofFun.top

  let join phis =
    let is_syntactic_top c = match uncof c with Some (Meet []) -> true | _ -> false in
    if List.exists is_syntactic_top phis then
      top
    else
      let expose phi = match uncof phi with Some (Join phis) -> phis | _ -> [phi] in
      match List.concat_map expose phis with
      | [phi] -> phi
      | l -> cof @@ CofFun.join l

  let meet phis =
    let is_syntactic_bot c = match uncof c with Some (Join []) -> true | _ -> false in
    if List.exists is_syntactic_bot phis then
      bot
    else
      let expose phi = match uncof phi with Some (Meet phis) -> phis | _ -> [phi] in
      match List.concat_map expose phis with
      | [phi] -> phi
      | l -> cof @@ CofFun.meet l
end

module type Param =
sig
  include GenericParam
  val dim0 : dim
  val dim1 : dim
end

module Make (P : Param) :
sig
  include module type of GenericMake(P)
  val boundary : P.dim -> P.cof
end
=
struct
  open P
  include GenericMake(P)
  let boundary r = join [eq r dim0; eq r dim1]
end
