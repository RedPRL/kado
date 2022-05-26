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

module Make (O : Map.OrderedType) : S with type key = O.t =
struct
  module M = Map.Make (O)
  module S = Set.Make (O)

  type vertex = O.t
  type key = vertex

  let (=) v1 v2 = O.compare v1 v2 = 0

  type t =
    { reachable : S.t M.t;
      reduced : O.t list M.t }

  let empty : t =
    { reachable = M.empty; reduced = M.empty }

  let mem_vertex (v : vertex) (g : t) : bool = M.mem v g.reachable

  let touch_vertex (v : vertex) (g : t) : t =
    { reachable = M.update v (function None -> Some (S.singleton v) | _ as s -> s) g.reachable
    ; reduced = M.update v (function None -> Some [] | _ as l -> l) g.reduced
    }

  let test (u : vertex) (v : vertex) (g : t) =
    if mem_vertex u g && mem_vertex v g then
      S.mem v (M.find u g.reachable)
    else
      u = v

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
