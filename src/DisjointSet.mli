module type S =
sig
  type key
  type t

  val empty : t
  val test : key -> key -> t -> bool
  val union : key -> key -> t -> t
  val test_and_union : key -> key -> t -> bool * t

  val merge : t -> t -> t

  type finger
  val finger : key -> t -> finger
  val test_finger : key -> finger -> bool
end

module Make (O : Map.OrderedType) : S with type key = O.t
