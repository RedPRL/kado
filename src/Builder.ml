module Endo =
struct
  module type Param =
  sig
    type dim
    type cof
    val dim0 : dim
    val dim1 : dim
    val cof : (dim, cof) Syntax.endo -> cof
    val uncof : cof -> (dim, cof) Syntax.endo option
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

    let eq x y = cof @@
      if x = y then
        Syntax.Endo.top
      else if (x = dim0 && y = dim1) || (x = dim1 && y = dim0) then
        Syntax.Endo.bot
      else
        Syntax.Endo.eq x y

    let bot = cof Syntax.Endo.bot
    let top = cof Syntax.Endo.top

    let join phis =
      let is_syntactic_top c = match uncof c with Some (Meet []) -> true | _ -> false in
      if List.exists is_syntactic_top phis then
        top
      else
        let expose phi = match uncof phi with Some (Join phis) -> phis | _ -> [phi] in
        match List.concat_map expose phis with
        | [phi] -> phi
        | l -> cof @@ Syntax.Endo.join l

    let meet phis =
      let is_syntactic_bot c = match uncof c with Some (Join []) -> true | _ -> false in
      if List.exists is_syntactic_bot phis then
        bot
      else
        let expose phi = match uncof phi with Some (Meet phis) -> phis | _ -> [phi] in
        match List.concat_map expose phis with
        | [phi] -> phi
        | l -> cof @@ Syntax.Endo.meet l

    let boundary r = join [eq r dim0; eq r dim1]
  end
end

module Free =
struct
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
    type cof = (dim, var) Syntax.free

    include Endo.S with type dim := dim and type cof := cof
  end

  module Make (P : Param) : S with type dim = P.dim and type var = P.var =
  struct
    open Syntax.Free

    module P = struct
      include P
      type cof = (dim, var) Syntax.free
      let cof phi = Cof phi
      let uncof phi = match phi with Cof phi -> Some phi | _ -> None
    end

    include P
    include Endo.Make(P)
  end
end
