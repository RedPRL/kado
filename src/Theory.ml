open StdLabels
open Bwd

module type Param =
sig
  type dim
  type var
  val dim0 : dim
  val dim1 : dim
  val compare_dim : dim -> dim -> int
  val compare_var : var -> var -> int
end

module type S =
sig
  type dim
  type var
  type cof = (dim, var) Syntax.free
  type alg_thy
  type disj_thy

  module Alg :
  sig
    type t = alg_thy
    val empty : t
    val consistency : t -> [`Consistent | `Inconsistent]
    val split : t -> cof list -> t list
    val meet2 : t -> t -> t
  end

  module Disj :
  sig
    type t = disj_thy
    val empty : t
    val consistency : t -> [`Consistent | `Inconsistent]
    val assume : t -> cof list -> t
    val test_sequent : t -> cof list -> cof -> bool
    val envelope_alg : Alg.t -> t
    val decompose : t -> Alg.t list
    val simplify_cof : t -> cof -> cof
    val forall_cof : t -> dim * cof -> cof
    val meet2 : t -> t -> t
  end
end


module Make (P : Param) : S with type dim = P.dim and type var = P.var =
struct
  include P

  open Syntax

  type cof = (dim, var) free

  module Vertex = 
  struct
    type t = dim
    let compare = compare_dim
  end

  module K = Kusariyarou.Make (Vertex)
  module VarSet = Set.Make (struct type t = var let compare = compare_var end)
  module B = Builder.Free.Make (struct include P let equal_dim x y = Int.equal (compare x y) 0 end)

  (** A presentation of an algebraic theory over the language of intervals and cofibrations. *)
  type alg_thy' =
    {graph : K.t;
     true_vars : VarSet.t}

  type atom = LT of dim * dim

  (** A [branch] represents the meet of a bunch of atomic cofibrations. *)
  type branch = VarSet.t * atom list
  type branches = branch list

  (** A [cached_branch] is a [branch] together with an algebraic theory
    * representing the resulting theory at the end of the branch.  *)
  type cached_branch = alg_thy' * branch
  type cached_branches = cached_branch list

  (** As an optimization, we remember whether a theory is consistent or not. *)
  type alg_thy = [ `Consistent of alg_thy' | `Inconsistent ]

  (** A disjoint theory is the join of a list of [cached_branch]. We do not need to
    * remember the common ancestor of these branches (as an algebraic theory), but only
    * the atomic cofibrations labeling the path from the common ancestor to each branch. *)
  type disj_thy = cached_branches

  (** This is to dissect the meet of a list of cofibrations into a list of branches.
    *
    * Possible further optimizations:
    * 1. Should we use [Cof.reduce] to massage the cofibrations first?
    * 2. Should we eagerly factor out common cofibrations to facilitate the refactoring
    *    steps later on? (This does not seem to be helpful in preliminary experiments.)
  *)
  let rec dissect_cofibrations : cof list -> branches =
    function
    | [] -> [VarSet.empty, []]
    | cof :: cofs ->
      match cof with
      | Var v ->
        List.map (dissect_cofibrations cofs)
          ~f:(fun (vars, atoms) -> VarSet.add v vars, atoms)
      | Cof cof ->
        match cof with
        | Meet meet_cofs ->
          dissect_cofibrations @@ meet_cofs @ cofs
        | Join join_cofs ->
          List.concat_map join_cofs
            ~f:(fun join_cof -> dissect_cofibrations @@ join_cof :: cofs)
        | Lt (r, s) ->
          List.map (dissect_cofibrations cofs)
            ~f:(fun (vars, atoms) -> vars, LT (r, s) :: atoms)
        | Eq (r, s) ->
          List.map (dissect_cofibrations cofs)
            ~f:(fun (vars, atoms) -> vars, LT (s, r) :: LT (r, s) :: atoms)


  module Alg =
  struct
    type t = alg_thy
    type t' = alg_thy'

    let emp' =
      {graph = K.add_edge P.dim0 P.dim1 @@ K.add_vertex P.dim0 @@ K.add_vertex P.dim1 K.empty;
       true_vars = VarSet.empty}

    let empty =
      `Consistent emp'

    let consistency =
      function
      | `Consistent _ -> `Consistent
      | `Inconsistent -> `Inconsistent

    let assume_vars (thy : t') vars =
      {thy with true_vars = VarSet.union vars thy.true_vars}

    let test_lt (thy : t') (r, s) =
      if K.mem_vertex r thy.graph && K.mem_vertex s thy.graph then 
        K.reachable r s thy.graph
      else 
        false

    let test_eq (thy : t') (r, s) =
      test_lt thy (r, s) && test_lt thy (s, r)

    (** [unsafe_test_and_assume_lt] fuses [test_lt] and [assume_lt] (if there was one).
      * It is "unsafe" because we do not check consistency here. *)
    let unsafe_test_and_assume_lt thy (r,s) =
      if test_lt thy (r, s) then 
        true, thy 
      else
        let graph = thy.graph in 
        let graph = if K.mem_vertex r graph then graph else K.add_vertex r graph in 
        let graph = if K.mem_vertex s graph then graph else K.add_vertex s graph in 
        let graph = K.add_edge r s graph in 
        false, {thy with graph}

    let test_var (thy : t') v =
      VarSet.mem v thy.true_vars

    (** [reduced_vars] takes out redundant cofibration variables. *)
    let reduce_vars (thy : t') vars =
      VarSet.diff vars thy.true_vars

    (** [reduce_atoms] detects inconsistency of an equation set and takes out
      * redundant equations. *)
    let reduce_atoms (thy : t') atoms =
      let go ((thy', atoms) as acc) (LT (r,s) as atom) =
        match unsafe_test_and_assume_lt thy' (r,s) with
        | true, _ -> acc
        | false, thy' -> thy', Snoc (atoms, atom)
      in
      let thy', atoms = List.fold_left ~f:go ~init:(thy, Emp) atoms in
      match test_eq thy' (P.dim0, P.dim1) with
      | true -> `Inconsistent
      | false -> `Consistent (thy', BwdLabels.to_list atoms)

    (** [reduce_branch] detects inconsistency of a branch and takes out redundant
      * cofibration variables and equations. *)
    let reduce_branch (thy' : t') (vars, atoms) =
      match reduce_atoms thy' atoms with
      | `Inconsistent -> `Inconsistent
      | `Consistent (thy', atoms) ->
        `Consistent (assume_vars thy' vars, (reduce_vars thy' vars, atoms))

    (** [reduce_branches] removes inconsistent branches and takes out redundant
      * cofibration variables and equations. *)
    let reduce_branches (thy' : t') (branches : branches) : cached_branches =
      let go branch =
        match reduce_branch thy' branch with
        | `Inconsistent -> None
        | `Consistent (thy', branch) -> Some (thy', branch)
      in
      List.filter_map ~f:go branches

    (** [split] combines all the optimizers above to split an algebraic theory
      * into multiple ones induced by the input cofibration context. *)
    let split (thy : t) (cofs : cof list) : t list =
      match thy with
      | `Inconsistent -> []
      | `Consistent thy' ->
        match dissect_cofibrations cofs with
        | [] -> []
        | [vars, []] when VarSet.is_empty vars -> [`Consistent thy']
        | dissected_cofs ->
          List.map ~f:(fun (thy', _) -> `Consistent thy') @@
          reduce_branches thy' dissected_cofs

    (** [test] checks whether a cofibration is true within an algebraic theory *)
    let rec test (thy' : alg_thy') : cof -> bool =
      function
      | Cof Eq (r, s) ->
        test_eq thy' (r, s)
      | Cof Lt (r, s) -> 
        test_lt thy' (r, s)
      | Cof Join phis ->
        List.exists ~f:(test thy') phis
      | Cof Meet phis ->
        List.for_all ~f:(test thy') phis
      | Var v ->
        test_var thy' v

    (* XXX: this function was never profiled *)
    let meet2' _thy'1 thy'2 =
      (* TODO: functionality not yet supported by library *)
      `Consistent thy'2

    (* let preorder = Preorder.union thy'1.preorder thy'2.preorder in 
       let true_vars = VarSet.union thy'1.true_vars thy'2.true_vars in
       let thy' = {true_vars; preorder} in
       match test_eq thy' (P.dim0, P.dim1) with
       | true -> `Inconsistent
       | false -> `Consistent thy' *)

    (* XXX: this function was never profiled *)
    let meet2 thy1 thy2 =
      match thy1, thy2 with
      | `Inconsistent, _ | _, `Inconsistent -> `Inconsistent
      | `Consistent thy'1, `Consistent thy'2 ->
        meet2' thy'1 thy'2
  end

  module Disj =
  struct
    type t = disj_thy

    let envelop_alg' alg_thy' : disj_thy =
      [alg_thy', (VarSet.empty, [])]

    let envelope_alg =
      function
      | `Consistent alg_thy' -> envelop_alg' alg_thy'
      | `Inconsistent -> []

    let empty : t = [Alg.emp', (VarSet.empty, [])]

    let consistency =
      function
      | [] -> `Inconsistent
      | _ -> `Consistent

    (** [split thy cofs] adds to the theory [thy] the conjunction of a list of cofibrations [cofs]
      * and calculate the branches accordingly. This is similar to [Alg.split] in the spirit but
      * differs in detail. *)
    let split (thy : t) (cofs : cof list) : t =
      match dissect_cofibrations cofs with
      | [] -> []
      | [vars, []] when VarSet.is_empty vars -> thy
      | dissected_cofs ->
        List.concat_map thy
          ~f:(fun (thy', (vars, eq)) ->
              List.map (Alg.reduce_branches thy' dissected_cofs)
                ~f:(fun (thy', (sub_vars, sub_atoms)) ->
                    thy', (VarSet.union vars sub_vars, eq @ sub_atoms)))

    (** [assume thy cofs] is the same as [split thy cofs] except that it further refactors the
      * branches to optimize future searching. *)
    let assume (thy : t) (cofs : cof list) : t =
      split thy cofs

    let test_sequent thy cx cof =
      List.for_all ~f:(fun (thy', _) -> Alg.test thy' cof) @@
      split thy cx

    let decompose thy =
      List.map thy ~f:(fun (thy', _) -> `Consistent thy')

    let test_eq thy (x, y) =
      let has_true = ref false in
      let () = 
        List.iter thy ~f:(fun (thy', _) ->
            match Alg.test_eq thy' (x, y) with
            | true -> has_true := true
            | _ -> ())
      in
      !has_true 

    (* XXX: this function was never profiled *)
    let simplify_cof thy cof =
      let simplify_var v =
        if List.for_all thy ~f:(fun (thy', _) -> Alg.test_var thy' v) then
          Free.top
        else
          Free.var v
      in
      let rec go =
        function
        | Cof Eq (x, y) ->
          B.eq x y
        | Cof Lt (x, y) ->
          B.lt x y
        | Var v ->
          simplify_var v
        | Cof Join phis ->
          B.join @@ List.map ~f:go phis
        | Cof Meet phis ->
          B.meet @@ List.map ~f:go phis
      in
      go cof

    (* XXX: this function was never profiled *)
    let forall_cof thy (sym, cof) =
      let forall_eq (x, y) =
        match test_eq thy (sym, x), test_eq thy (sym, y) with 
        | true, true -> B.top
        | false, false -> B.eq x y
        | _ -> B.bot
      in
      let forall_lt (x, y) = 
        match test_eq thy (sym, x) with 
        | true -> B.eq x P.dim0
        | _ ->
          match test_eq thy (sym, y) with 
          | true -> B.eq y P.dim1 
          | _ -> B.lt x y
      in
      let rec go =
        function
        | Cof Eq (x, y) ->
          forall_eq (x, y)
        | Cof Lt (x, y) -> 
          forall_lt (x, y)
        | Var v -> Var v
        | Cof Join phis ->
          B.join @@ List.map ~f:go phis
        | Cof Meet phis ->
          B.meet @@ List.map ~f:go phis
      in
      go cof

    (* XXX: this function was never profiled *)
    let meet2 (thy1 : t) (thy2 : t) : t =
      (* a correct but unoptimized theory *)
      List.concat_map thy1
        ~f:(fun (thy'1, (vars1, atoms1)) ->
            List.filter_map thy2
              ~f:(fun (thy'2, (vars2, atoms2)) ->
                  match Alg.meet2' thy'1 thy'2 with
                  | `Inconsistent -> None
                  | `Consistent thy' -> Some (thy', (VarSet.union vars1 vars2, atoms1 @ atoms2))))
  end
end