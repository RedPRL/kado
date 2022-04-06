(** Endofunctors for cofibrations. *)

(** A family of polynomial endofunctors [('r, -) t] indexed in an interpretation of the interval algebra ['r].
    Multiple types in [cooltt] will need to {i include} the langauge of cofibrations, relative to a particular interval algebra ['r],
    and thus we keep ['a] generic. *)
type ('r, 'a) t =
  | Eq of 'r * 'r
  | Join of 'a list
  | Meet of 'a list

(** [eq x y] is [Eq (x, y)] *)
val eq : 'r -> 'r -> ('r, 'a) t

(** [join phis] is [Join phis] *)
val join : 'a list -> ('r, 'a) t

(** [meet phis] is [Meet phis] *)
val meet : 'a list -> ('r, 'a) t

(** [bot] is [Join []] *)
val bot : ('r, 'a) t

(** [top] is [Meet []] *)
val top : ('r, 'a) t
