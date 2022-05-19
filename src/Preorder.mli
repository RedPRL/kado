module type S = 
sig
  type vertex
  type t

  val empty : t
  val insert : vertex -> vertex -> t -> t
  val has_path : vertex -> vertex -> t -> bool

  val union : t -> t -> t
  val dump : (Format.formatter -> vertex -> unit) -> Format.formatter -> t -> unit
end

(** Implemented with the algorithm of Poutré and Leeuwen: https://pure.tue.nl/ws/files/4393029/319321.pdf. *)
module Make (V : Map.OrderedType) : S with type vertex = V.t