type ('r, 'a) t =
  | Eq of 'r * 'r
  | Join of 'a list
  | Meet of 'a list

let join phis = Join phis
let meet phis = Meet phis

let bot = join []
let top = meet []

let eq x y = Eq (x, y)
