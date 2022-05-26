module Graph = Kado.Graph.Make (Int)
module IntSet = Set.Make (Int)

let dumb_reachable start target edges =
  let rec go target edges visited : bool =
    IntSet.mem target visited || begin
      let new_visited =
        let f visited (u, v) =
          if IntSet.mem u visited && not (IntSet.mem v visited) then
            IntSet.add v visited
          else
            visited
        in
        List.fold_left f visited edges
      in
      if visited == new_visited then false
      else go target edges new_visited
    end
  in
  go target edges (IntSet.singleton start)

let vertices = List.init 10 Fun.id

let gen_vertex = QCheck2.Gen.oneofl vertices
let print_vertex = QCheck.Print.int
let gen_edges = QCheck2.Gen.(small_list @@ pair gen_vertex gen_vertex)
let print_edges = QCheck2.Print.(list @@ pair print_vertex print_vertex)

let test_reachability =
  QCheck2.Test.make ~count:10000 ~name:"all-pair reachability" QCheck2.Gen.(pair gen_edges gen_edges)
    ~print:QCheck2.Print.(pair print_edges print_edges)
  @@ fun (edges, tests) ->
  let graph = List.fold_left (fun g (u, v) -> Graph.union u v g) Graph.empty edges in
  tests |> Stdlib.List.for_all @@ fun (u, v) ->
  Graph.test u v graph == dumb_reachable u v edges

let () =
  exit @@
  QCheck_base_runner.run_tests ~colors:true ~verbose:true ~long:true
    [ test_reachability ]
