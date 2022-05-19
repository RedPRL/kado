module type S = 
sig
  type vertex
  type t

  val empty : t
  val insert : vertex -> vertex -> t -> t
  val has_path : vertex -> vertex -> t -> bool

  val union : t -> t -> t
end

module Make (V : Map.OrderedType) : S with type vertex = V.t = 
struct
  module M = Map.Make (V)
  module S = Set.Make (V)

  type t = 
    {generators : S.t M.t; 
     closure : S.t M.t}

  type vertex = V.t

  let empty : t = 
    {generators = M.empty; 
     closure = M.empty}

  let reachable_vertices x (gph : t) : S.t = 
    match M.find_opt x gph.closure with 
    | None -> S.empty
    | Some s -> s

  let successors x (gph : t) : S.t = 
    match M.find_opt x gph.generators with 
    | None -> S.empty 
    | Some s -> s

  let has_path x y gph =
    (V.compare x y == 0) ||
    S.mem y @@ reachable_vertices x gph

  module Prim =
  struct 
    let add_generator x y gph =
      {gph with generators = M.add x (S.add y (reachable_vertices x gph)) gph.generators}  

    let add_edge x y gph = 
      {gph with closure = M.add x (S.add y (reachable_vertices x gph)) gph.closure} 
  end

  let insert x y gph =
    let gph = Prim.add_generator x y gph in
    if has_path x y gph then gph else 
      let rec loop bnds gph =
        match bnds with 
        | [] -> gph
        | (z, _) :: bnds ->
          if not (has_path z y gph) && has_path z y gph then 
            let reds = [y] in
            let gph = adapt z reds gph in
            loop bnds gph
          else 
            loop bnds gph

      and adapt z reds gph =
        match reds with
        | [] -> gph
        | l :: reds ->
          let gph = Prim.add_edge z l gph in 
          let succs = S.elements @@ successors l gph in 
          let reds = reds @ List.filter (fun m -> not (has_path z m gph)) succs in 
          adapt z reds gph
      in
      loop (M.bindings gph.closure) gph

  let union (gph1 : t) (gph2 : t) : t =
    let rec loop bnds gph = 
      match bnds with 
      | [] -> gph
      | (x, ys) :: bnds -> 
        let rec loop' ys gph = 
          match ys with 
          | [] -> loop bnds gph
          | y :: ys -> 
            let gph = insert x y gph in 
            loop' ys gph
        in
        loop' (S.elements ys) gph
    in 
    loop (M.bindings gph2.generators) gph1
end