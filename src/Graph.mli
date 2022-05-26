module type S =
sig
  type key
  type t

  val empty : t
  val test_le : key -> key -> t -> bool
  val union_le : key -> key -> t -> t
  val test_and_union_le : key -> key -> t -> bool * t

  val merge : t -> t -> t
end

module Make (O : Map.OrderedType) : S with type key = O.t
