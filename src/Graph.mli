module type Vertex =
sig
  type t
  val compare : t -> t -> int

  val initial : t
  val terminal : t
end

module type S =
sig
  type key
  type t

  val empty : t
  val test : key -> key -> t -> bool
  val union : key -> key -> t -> t
  val test_and_union : key -> key -> t -> bool * t

  val merge : t -> t -> t
end

module Make (V : Vertex) : S with type key = V.t
