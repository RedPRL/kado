module Endo =
struct
  module type Param =
  sig
    type dim
    type cof
    val dim0 : dim
    val dim1 : dim
    val equal_dim : dim -> dim -> bool
    val cof : (dim, cof) Syntax.endo -> cof
    val uncof : cof -> (dim, cof) Syntax.endo option
  end

  module type S =
  sig
    module Param : sig
      type dim
      type cof
    end
    open Param

    type t = cof
    val cof : (dim, cof) Syntax.endo -> cof
    val le : dim -> dim -> cof
    val bot : cof
    val top : cof
    val join : cof list -> cof
    val meet : cof list -> cof
    val eq : dim -> dim -> cof
    val eq0 : dim -> cof
    val eq1 : dim -> cof
    val boundary : dim -> cof
    val forall : dim * cof -> cof
  end

  module Make (Param : Param) : S with module Param := Param =
  struct
    include Param

    type t = cof

    let (=) = equal_dim

    let le x y = cof @@
      if dim0 = x || x = y || y = dim1 then
        Syntax.Endo.top
      else if x = dim1 && y = dim0 then
        Syntax.Endo.bot
      else
        Syntax.Endo.le x y

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

    let eq x y = meet [le x y; le y x]

    let eq0 x = eq x dim0

    let eq1 x = eq x dim1

    let boundary r = join [eq0 r; eq1 r]

    let forall (sym, cof) =
      let rec go cof =
        match uncof cof with
        | None -> cof
        | Some Le (x, y) ->
          begin
            match equal_dim x sym, equal_dim y sym with
            | true, true -> top
            | true, false -> if y = dim1 then top else bot
            | false, true -> if x = dim0 then top else bot
            | _ -> eq x y
          end
        | Some Meet phis -> meet @@ List.map go phis
        | Some Join phis -> join @@ List.map go phis
      in
      go cof
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
    val equal_dim : dim -> dim -> bool
  end

  module type S =
  sig
    module Param : sig
      type dim
      type var
    end
    open Param
    include Endo.S with type Param.cof := (dim, var) Syntax.free and module Param := Param
    val var : var -> t
  end

  module Make (Param : Param) : S with module Param := Param =
  struct
    open Syntax.Free

    module Param = struct
      include Param
      type cof = (dim, var) Syntax.free
      let cof phi = Cof phi
      let uncof phi = match phi with Cof phi -> Some phi | _ -> None
    end
    include Param
    include Endo.Make(Param)

    let var = var
  end
end
