(**
   This library is based on "Amortized Efficiency of a Path Retrieval Data Structure" by G.F. Italiano, probably with O(log n) slowdown due to the use of {!module:Map}. Moreover, the analysis in the paper does not apply to a persistent data structure design (here) that supports branching. In other words, the amortized bound might not hold if editing operations are applied on previous versions of graphs, though merely checking reachability is okay.

   This library is part of a larger project that implements cubical type theory, and may be changed or merged at any time.
*)

(** The interface of a graph. *)
module type S =
sig
  (** The type of directed graphs. *)
  type t

  (** The type of vertices. *)
  type vertex

  (** The empty graph. *)
  val empty : t

  (** [mem_vertex v g] tests whether the vertex [v] is in the graph [g]. *)
  val mem_vertex : vertex -> t -> bool

  (** [add_vertex v g] adds the vertex [v] into the graph [g]. *)
  val add_vertex : vertex -> t -> t

  (** [add_edge u v g] adds an edge from [u] to [v] into the graph [g]. The two vertices must be already in the graph, or [Not_found] will be raised. *)
  val add_edge : vertex -> vertex -> t -> t

  (** [reachable u v g] tests whether there is a path from [u] to [v] in the transitive and reflexive closure of [g]. This means reachability is reflexive even if no loop was explicitly added to the graph. If either vertex is not in the graph, [Not_found] will be raised. *)
  val reachable : vertex -> vertex -> t -> bool

  val union : t -> t -> t
end

(** The functor that takes a vertex module and outputs a graph module. *)
module Make : functor (V : Map.OrderedType) -> S with type vertex = V.t
