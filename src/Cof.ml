type ('r, 'v) t =
  | Cof of ('r, ('r, 'v) t) CofFun.t
  | Var of 'v

let var v = Var v
let cof c = Cof c

let eq x y = cof @@ CofFun.eq x y
let join phis = cof @@ CofFun.join phis
let meet phis = cof @@ CofFun.meet phis
let bot = cof CofFun.bot
let top = cof CofFun.top

let rec dump dump_r dump_v fmt =
  function
  | Cof cof -> CofFun.dump dump_r (dump dump_r dump_v) fmt cof
  | Var v -> dump_v fmt v
