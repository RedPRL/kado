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

  let (=) = Int.equal

  type key = O.t
  type t =
    {rank : int M.t;
     parent : key M.t}

  let empty =
    {rank = M.empty;
     parent = M.empty}

  let root x t =
    let rec loop x =
      match M.find_opt x t.parent with
      | Some x -> (loop[@tailcall]) x
      | None -> x
    in
    loop x

  let rank x t =
    Option.value ~default:0 @@
    M.find_opt x t.rank

  let test x y t =
    O.compare x y = 0 ||
    O.compare (root x t) (root y t) = 0

  let test_and_union (x : key) (y : key) (h : t) =
    if O.compare x y = 0 then
      true, h
    else
      let x = root x h in
      let y = root y h in
      if O.compare x y = 0 then
        true, h
      else
        false,
        begin
          let rx = rank x h in
          let ry = rank y h in
          if rx > ry then
            {h with parent = M.add y x h.parent}
          else if rx < ry then
            {h with parent = M.add x y h.parent}
          else
            {rank = M.add x (rx + 1) h.rank;
             parent = M.add y x h.parent}
        end

  let union (x : key) (y : key) (h : t) =
    snd @@ test_and_union x y h

  let merge h1 h2 = M.fold union h2.parent h1
end
