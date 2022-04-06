type ('r, 'v) t =
  | Cof of ('r, ('r, 'v) t) CofFun.t
  | Var of 'v

val var : 'v -> ('a, 'v) t
val cof : ('r, ('r, 'v) t) CofFun.t -> ('r, 'v) t
val eq : 'a -> 'a -> ('a, 'v) t

val bot : ('a, 'v) t
val top : ('a, 'v) t
