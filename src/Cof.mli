(** For each interval algebra ['r], we define the {i free monad} [('r, -) t] on the polynomial endofunctor [('r, -) CofFun.t]: each [('r, 'v) t] is the language of cofibrations over an interval algebra ['r], with indeterminates drawn from ['v]. *)
type ('r, 'v) t =
  | Cof of ('r, ('r, 'v) t) CofFun.t
  | Var of 'v

(** [var v] is [Var v] *)
val var : 'v -> ('a, 'v) t

(** [cof phi] is [Cof phi] *)
val cof : ('r, ('r, 'v) t) CofFun.t -> ('r, 'v) t

(** [eq x y] is [Eq (x, y)] *)
val eq : 'a -> 'a -> ('a, 'v) t

(** [join phis] is [Cof (Join phis)] *)
val join : ('a, 'v) t list -> ('a, 'v) t

(** [meet phis] is [Cof (Meet phis)] *)
val meet : ('a, 'v) t list -> ('a, 'v) t

(** [bot] is [Cof (Join [])] *)
val bot : ('a, 'v) t

(** [top] is [Cof (Meet [])] *)
val top : ('a, 'v) t

(** Ugly printer *)
val dump :
  (Format.formatter -> 'r -> unit) ->
  (Format.formatter -> 'v -> unit) ->
  Format.formatter -> ('r, 'v) t -> unit
