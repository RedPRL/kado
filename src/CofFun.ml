type ('r, 'a) t =
  | Eq of 'r * 'r
  | Join of 'a list
  | Meet of 'a list

let join phis = Join phis
let meet phis = Meet phis

let bot = join []
let top = meet []

let eq x y = Eq (x, y)

let dump dump_r dump_a fmt =
  function
  | Eq (r1, r2) ->
    Format.fprintf fmt "@[<hv 1>eq[@,@[%a@];@,@[%a@]]@]" dump_r r1 dump_r r2
  | Join l ->
    Format.fprintf fmt "@[<hv 1>join[@,%a]@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,") dump_a) l
  | Meet l ->
    Format.fprintf fmt "@[<hv 1>meet[@,%a]@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,") dump_a) l
