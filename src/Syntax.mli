(** A family of polynomial endofunctors [('r, -) t] indexed in an interpretation of the interval algebra ['r].
    This is for multiple types (for example, abstract syntax) to {e embed} the langauge of cofibrations. *)
type ('r, 'a) endo =
  | Le of 'r * 'r
  | Join of 'a list
  | Meet of 'a list

(** For each interval algebra ['r], we define the {e free monad} [('r, -) free] on the polynomial endofunctor [('r, -) endo]:
    each [('r, 'v) t] is the language of cofibrations over an interval algebra ['r], with indeterminates drawn from ['v]. *)
type ('r, 'v) free =
  | Cof of ('r, ('r, 'v) free) endo
  | Var of 'v

(** Stupid constructors for {!type:endo}. *)
module Endo :
sig
  (** A family of polynomial endofunctors [('r, -) t] indexed in an interpretation of the interval algebra ['r].
      This is for multiple types (for example, abstract syntax) to {e embed} the langauge of cofibrations.

      @canonical Kado.Syntax.endo
  *)
  type ('r, 'a) t = ('r, 'a) endo =
    | Le of 'r * 'r
    | Join of 'a list
    | Meet of 'a list

  (** [le x y] is [Le (x, y)] *)
  val le : 'r -> 'r -> ('r, 'a) t

  (** [join phis] is [Join phis] *)
  val join : 'a list -> ('r, 'a) t

  (** [meet phis] is [Meet phis] *)
  val meet : 'a list -> ('r, 'a) t

  (** [bot] is [Join []] *)
  val bot : ('r, 'a) t

  (** [top] is [Meet []] *)
  val top : ('r, 'a) t

  (** [map] is the functoriality of the endo functor [('r, -) t]. *)
  val map : ('a -> 'b) -> ('r, 'a) t -> ('r, 'b) t

  (** Ugly printer. *)
  val dump :
    (Format.formatter -> 'r -> unit) ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> ('r, 'a) t -> unit
end

(** Stupid constructors for {!type:free}. *)
module Free :
sig
  (** A family of polynomial endofunctors [('r, -) t] indexed in an interpretation of the interval algebra ['r].
      This is for multiple types (for example, abstract syntax) to {e embed} the langauge of cofibrations.

      @canonical Kado.Syntax.endo
  *)
  type nonrec ('r, 'a) endo = ('r, 'a) endo =
    | Le of 'r * 'r
    | Join of 'a list
    | Meet of 'a list

  (** For each interval algebra ['r], we define the {e free monad} [('r, -) t] on the polynomial endofunctor [('r, -) endo]:
      each [('r, 'v) t] is the language of cofibrations over an interval algebra ['r], with indeterminates drawn from ['v].

      @canonical Kado.Syntax.free
  *)
  type ('r, 'v) t = ('r, 'v) free =
    | Cof of ('r, ('r, 'v) t) endo
    | Var of 'v

  (** [var v] is [Var v] *)
  val var : 'v -> ('a, 'v) t

  (** [cof phi] is [Cof phi] *)
  val cof : ('r, ('r, 'v) t) endo -> ('r, 'v) t

  (** [le x y] is [Le (x, y)] *)
  val le : 'a -> 'a -> ('a, 'v) t

  (** [join phis] is [Cof (Join phis)] *)
  val join : ('a, 'v) t list -> ('a, 'v) t

  (** [meet phis] is [Cof (Meet phis)] *)
  val meet : ('a, 'v) t list -> ('a, 'v) t

  (** [bot] is [Cof (Join [])] *)
  val bot : ('a, 'v) t

  (** [top] is [Cof (Meet [])] *)
  val top : ('a, 'v) t

  (** Ugly printer. *)
  val dump :
    (Format.formatter -> 'r -> unit) ->
    (Format.formatter -> 'v -> unit) ->
    Format.formatter -> ('r, 'v) t -> unit
end
