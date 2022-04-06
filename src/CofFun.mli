type ('r, 'a) t =
  | Eq of 'r * 'r
  | Join of 'a list
  | Meet of 'a list

val eq : 'r -> 'r -> ('r, 'a) t
val join : 'a list -> ('r, 'a) t
val meet : 'a list -> ('r, 'a) t

val bot : ('r, 'a) t
val top : ('r, 'a) t
