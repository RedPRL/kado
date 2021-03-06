type ('r, 'a) endo =
  | Le of 'r * 'r
  | Join of 'a list
  | Meet of 'a list

type ('r, 'v) free =
  | Cof of ('r, ('r, 'v) free) endo
  | Var of 'v

module Endo =
struct
  type ('r, 'a) t = ('r, 'a) endo =
    | Le of 'r * 'r
    | Join of 'a list
    | Meet of 'a list

  let join phis = Join phis
  let meet phis = Meet phis

  let bot = join []
  let top = meet []

  let le x y = Le (x, y)

  let map f =
    function
    | Le _ as phi -> phi
    | Join l -> Join (List.map f l)
    | Meet l -> Meet (List.map f l)

  let dump dump_r dump_a fmt =
    function
    | Le (r1, r2) ->
      Format.fprintf fmt "@[<hv 1>le[@,@[%a@];@,@[%a@]]@]" dump_r r1 dump_r r2
    | Join l ->
      Format.fprintf fmt "@[<hv 1>join[@,%a]@]"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,") dump_a) l
    | Meet l ->
      Format.fprintf fmt "@[<hv 1>meet[@,%a]@]"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,") dump_a) l
end

module Free =
struct
  type nonrec ('r, 'a) endo = ('r, 'a) endo =
    | Le of 'r * 'r
    | Join of 'a list
    | Meet of 'a list

  type ('r, 'v) t = ('r, 'v) free =
    | Cof of ('r, ('r, 'v) t) endo
    | Var of 'v

  let var v = Var v
  let cof c = Cof c

  let le x y = cof @@ Endo.le x y
  let join phis = cof @@ Endo.join phis
  let meet phis = cof @@ Endo.meet phis
  let bot = cof Endo.bot
  let top = cof Endo.top

  let rec dump dump_r dump_v fmt =
    function
    | Cof cof -> Endo.dump dump_r (dump dump_r dump_v) fmt cof
    | Var v -> dump_v fmt v

end
