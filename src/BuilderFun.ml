module type Param =
sig
  type dim
  type cof
  val dim0 : dim
  val dim1 : dim
  val cof : (dim, cof) CofFun.t -> cof
  val uncof : cof -> (dim, cof) CofFun.t option
end

module type S =
sig
  type dim
  type cof

  val eq : dim -> dim -> cof
  val bot : cof
  val top : cof

  val join : cof list -> cof
  val meet : cof list -> cof
  val boundary : dim -> cof
end

module Make (P : Param) : S with type dim = P.dim and type cof = P.cof =
struct
  open P

  type nonrec dim = dim
  type nonrec cof = cof

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

  let boundary r = join [eq r dim0; eq r dim1]
end
