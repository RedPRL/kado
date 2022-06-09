module type Vertex =
sig
  type t
  val compare : t -> t -> int

  val initial : t (* if you pretent the graph is a category *)
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

module Make (V : Vertex) : S with type key = V.t =
struct
  module M = Map.Make (V)
  module S = Set.Make (V)

  type vertex = V.t
  type key = vertex

  let (=) v1 v2 = V.compare v1 v2 = 0

  type t =
    { reachable : S.t M.t; (* a map from vertices to their reachable vertices *)
      reduced : V.t list M.t; (* the reduced graph exclding (v, 1) *)
    }

  let empty : t =
    { reachable = M.of_seq @@ List.to_seq
          [V.initial, S.of_list [V.initial; V.terminal];
           V.terminal, S.singleton V.terminal];
      reduced = M.of_seq @@ List.to_seq
          [V.initial, []; V.terminal, []] }

  let mem_vertex (v : vertex) (g : t) : bool = M.mem v g.reachable

  let touch_vertex (v : vertex) (g : t) : t =
    if mem_vertex v g then g else
      { reachable =
          M.add v (S.add v (M.find V.terminal g.reachable)) @@
          M.map (fun s -> if S.mem V.initial s then S.add v s else s) @@
          g.reachable;
        reduced =
          (* as an optimization, there's no need to the add edge (v, 1) because 1 is always reachable. *)
          M.update V.initial (fun l -> Some (v :: Option.get l)) @@
          M.add v [] @@
          g.reduced }

  let raw_test (u : vertex) (v : vertex) (g : t) =
    S.mem v (M.find u g.reachable)

  let test (u : vertex) (v : vertex) (g : t) =
    match mem_vertex u g, mem_vertex v g with
    | true, true -> raw_test u v g
    | true, false -> raw_test u V.initial g
    | false, true -> raw_test V.terminal v g
    | false, false -> u = v || raw_test V.terminal V.initial g

  let union (u : vertex) (v : vertex) (g : t) =
    if test u v g then g else
      let g = touch_vertex v @@ touch_vertex u g in
      let rec meld i rx =
        let f rx j = if S.mem j rx then rx else meld j (S.add j rx) in
        List.fold_left f rx (M.find i g.reduced)
      in
      { reduced = M.update u (fun l -> Some (v :: Option.get l)) g.reduced;
        reachable =
          g.reachable
          |> M.map @@ fun rx ->
          if S.mem u rx && not (S.mem v rx) then
            meld v @@ S.add v rx
          else
            rx }

  let test_and_union (u : vertex) (v : vertex) (g : t) =
    test u v g, union u v g

  let merge (g1 : t) (g2 : t) =
    M.fold (fun u -> List.fold_right (union u)) g1.reduced g2
end
