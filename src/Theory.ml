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
  module Param : sig
    type dim
    type var
  end
  open Param

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

module Make (Param : Param) : S with module Param := Param =
struct
  include Param

  open Syntax

  type cof = (dim, var) free

  module Graph = Graph.Make (struct
      type t = dim
      let compare = compare_dim
      let initial = dim0
      let terminal = dim1
    end)
  module VarSet = Set.Make (struct type t = var let compare = compare_var end)
  module B = Builder.Free.Make (struct
      include Param
      let equal_dim x y = Int.equal (compare_dim x y) 0
    end)

  (** A presentation of an algebraic theory over the language of intervals and cofibrations. *)
  type alg_thy' =
    { le : Graph.t; true_vars : VarSet.t }

  type le = dim * dim

  (** A [branch] represents the meet of a bunch of atomic cofibrations. *)
  type branch = VarSet.t * le list
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
        List.map (fun (vars, eqs) -> VarSet.add v vars, eqs) @@ dissect_cofibrations cofs
      | Cof cof ->
        match cof with
        | Meet meet_cofs ->
          dissect_cofibrations @@ meet_cofs @ cofs
        | Join join_cofs ->
          List.concat_map (fun join_cof -> dissect_cofibrations @@ join_cof :: cofs) join_cofs
        | Le (r, s) ->
          List.map (fun (vars, eqs) -> vars, (r, s) :: eqs) @@ dissect_cofibrations cofs

  module Alg =
  struct
    type t = alg_thy
    type t' = alg_thy'

    let emp' =
      {le = Graph.empty;
       true_vars = VarSet.empty}

    let empty =
      `Consistent emp'

    let consistency =
      function
      | `Consistent _ -> `Consistent
      | `Inconsistent -> `Inconsistent

    let assume_vars (thy : t') vars =
      {thy with true_vars = VarSet.union vars thy.true_vars}

    let test_le (thy : t') (r, s) =
      Graph.test r s thy.le

    let test_inconsistent (thy' : t') = test_le thy' (dim1, dim0)

    (** [unsafe_test_and_assume_eq] fuses [test_eq] and [assume_eq] (if there was one).
      * It is "unsafe" because we do not check consistency here. *)
    let unsafe_test_and_assume_le (thy : t') (r, s) =
      let testing, le = Graph.test_and_union r s thy.le in
      testing, {thy with le}

    let tri_test_le thy (x, y) =
      match unsafe_test_and_assume_le thy (x, y) with
      | true, _ -> `True
      | false, thy' ->
        if test_inconsistent thy' then
          `False
        else
          `Indeterminate

    let test_les (thy : t') les =
      List.for_all (test_le thy) les

    let test_var (thy : t') v =
      VarSet.mem v thy.true_vars

    let test_vars (thy : t') vs =
      VarSet.subset vs thy.true_vars

    let test_branch (thy : t') (vars, les) =
      test_vars thy vars && test_les thy les

    (** [reduced_vars] takes out redundant cofibration variables. *)
    let reduce_vars (thy : t') vars =
      VarSet.diff vars thy.true_vars

    (** [reduce_les] detects inconsistency of an inequality set and takes out
      * redundant inequalities. *)
    let reduce_les (thy : t') les =
      let go ((thy', les) as acc) le =
        match unsafe_test_and_assume_le thy' le with
        | true, _ -> acc
        | false, thy' -> thy', Snoc (les, le)
      in
      let thy', les = List.fold_left go (thy, Emp) les in
      match test_inconsistent thy' with
      | true -> `Inconsistent
      | false -> `Consistent (thy', Bwd.to_list les)

    (** [reduce_branch] detects inconsistency of a branch and takes out redundant
      * cofibration variables and inequalities. *)
    let reduce_branch (thy' : t') (vars, les) =
      match reduce_les thy' les with
      | `Inconsistent -> `Inconsistent
      | `Consistent (thy', les) ->
        `Consistent (assume_vars thy' vars, (reduce_vars thy' vars, les))

    (** [reduce_branches] removes inconsistent branches and takes out redundant
      * cofibration variables and inequalities. *)
    let reduce_branches (thy' : t') branches : cached_branches =
      let go branch =
        match reduce_branch thy' branch with
        | `Inconsistent -> None
        | `Consistent (thy', branch) -> Some (thy', branch)
      in
      List.filter_map go branches

    (** [drop_useless_branches] drops the branches that could be dropped without
      * affecting the coverages. *)
    let drop_useless_branches cached_branches : cached_branches =
      let go_fwd acc (thy', branch) =
        if Bwd.exists (fun (_, branch) -> test_branch thy' branch) acc then
          acc
        else
          Snoc (acc, (thy', branch))
      in
      let cached_branches = List.fold_left go_fwd Emp cached_branches in
      let go_bwd (thy', branch) acc =
        if List.exists (fun (_, branch) -> test_branch thy' branch) acc then
          acc
        else
          (thy', branch) :: acc
      in
      Bwd.fold_right go_bwd cached_branches []

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
          List.map (fun (thy', _) -> `Consistent thy') @@
          drop_useless_branches @@
          reduce_branches thy' dissected_cofs

    (** [test] checks whether a cofibration is true within an algebraic theory *)
    let rec test (thy' : alg_thy') : cof -> bool =
      function
      | Cof Le (r, s) ->
        test_le thy' (r, s)
      | Cof Join phis ->
        List.exists (test thy') phis
      | Cof Meet phis ->
        List.for_all (test thy') phis
      | Var v ->
        test_var thy' v

    (* XXX: this function was never profiled *)
    let meet2' thy'1 thy'2 =
      let thy' =
        {le = Graph.merge thy'1.le thy'2.le;
         true_vars = VarSet.union thy'1.true_vars thy'2.true_vars}
      in
      match test_inconsistent thy' with
      | true -> `Inconsistent
      | false -> `Consistent thy'

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

    (** [refactor_branches] attempts to identify common parts of the branches
      * and shrink the labels. Recall that we do not keep the common ancestor
      * but the paths (as a collection of atomic cofibrations) from the common
      * ancestor, and thus what are changed here are the paths.
      *
      * This optimization seems to be expensive, but it seems to help after
      * we switched from the persistant tables (using [Hashtbl]) to [Map].
    *)
    let refactor_branches cached_branches : t =
      let common_vars =
        let go vars0 (_, (vars1, _)) = VarSet.inter vars0 vars1 in
        match cached_branches with
        | [] -> VarSet.empty
        | (_, (vars, _)) :: branches -> List.fold_left go vars branches
      in
      (* The following is checking whether individual inequalities are useful (not shared
       * by all the algebraic theories). It does not kill every "useless" equation where
       * the uselessness can be only observed after looking at multiple inequalities.
       * Here is one example:
       *
       * branch 1: r=0
       * branch 2: r=i, i=0
       *
       * r=0 will be factored out, but then i=0 should have also been removed. The code
       * would not remove i=0. Here is a more complicated example:
       *
       * branch 1: r=i, i=0
       * branch 2: r=j, j=0
       *
       * Both i=0 and j=0 should be factored out, but the following code is not smart
       * enough to detect them. One could consider more aggressive approaches if [CofThy]
       * becomes the bottleneck again.
      *)
      let useful le =
        List.exists (fun (thy', _) -> not @@ Alg.test_le thy' le) cached_branches
      in
      (* revisit all branches and remove all useless ones identified by the simple criterion above. *)
      cached_branches |>
      List.map (fun (thy', (vars, eqs)) -> thy', (VarSet.diff vars common_vars, List.filter useful eqs))

    (** [split thy cofs] adds to the theory [thy] the conjunction of a list of cofibrations [cofs]
      * and calculate the branches accordingly. This is similar to [Alg.split] in the spirit but
      * differs in detail. *)
    let split (thy : t) (cofs : cof list) : t =
      match dissect_cofibrations cofs with
      | [] -> []
      | [vars, []] when VarSet.is_empty vars -> thy
      | dissected_cofs ->
        let thy =
          thy |> List.concat_map @@ fun (thy', (vars, eq)) ->
          let dissected_cofs = Alg.reduce_branches thy' dissected_cofs in
          dissected_cofs |> List.map @@ fun (thy', (sub_vars, sub_eqs)) ->
          thy', (VarSet.union vars sub_vars, eq @ sub_eqs)
        in
        Alg.drop_useless_branches thy

    (** [assume thy cofs] is the same as [split thy cofs] except that it further refactors the
      * branches to optimize future searching. *)
    let assume (thy : t) (cofs : cof list) : t =
      refactor_branches @@ split thy cofs

    let test_sequent thy cx cof =
      List.for_all (fun (thy', _) -> Alg.test thy' cof) @@
      split thy cx

    let decompose thy =
      List.map (fun (thy', _) -> `Consistent thy') thy

    let test_le thy le = List.for_all (fun (thy', _) -> Alg.test_le thy' le) thy

    let test_eq thy (x, y) = test_le thy (x, y) && test_le thy (y, x)

    (* XXX: this function was never profiled *)
    let tri_test_le thy (x, y) =
      let has_true = ref false in
      let has_indet = ref false in
      let has_false = ref false in
      let () = thy |> List.iter (fun (thy', _) ->
          match Alg.tri_test_le thy' (x, y) with
          | `True -> has_true := true
          | `False -> has_false := true
          | `Indeterminate -> has_indet := true)
      in
      match !has_true, !has_indet, !has_false with
      | _, true, _ | true, _, true -> `Indeterminate
      | _, false, false -> `True
      | false, false, _ -> `False

    (* XXX: this function was never profiled *)
    let simplify_cof thy cof =
      let simplify_le (x, y) =
        match tri_test_le thy (x, y) with
        | `True -> Free.top
        | `False -> Free.bot
        | `Indeterminate -> Free.le x y
      in
      let simplify_var v =
        if List.for_all (fun (thy', _) -> Alg.test_var thy' v) thy then
          Free.top
        else
          Free.var v
      in
      let rec go =
        function
        | Cof Le (x, y) ->
          simplify_le (x, y)
        | Var v ->
          simplify_var v
        | Cof Join phis ->
          B.join @@ List.map go phis
        | Cof Meet phis ->
          B.meet @@ List.map go phis
      in
      go cof

    (* XXX: this function was never profiled *)
    let forall_cof thy (sym, cof) =
      let forall_le (x, y) =
        match test_eq thy (x, sym), test_eq thy (y, sym) with
        | true, true -> Free.top
        | true, false -> if test_le thy (dim1, y) then Free.top else Free.bot
        | false, true -> if test_le thy (x, dim0) then Free.top else Free.bot
        | _ -> Free.le x y
      in
      let rec go =
        function
        | Cof Le (x, y) ->
          forall_le (x, y)
        | Var v -> Var v
        | Cof Join phis ->
          B.join @@ List.map go phis
        | Cof Meet phis ->
          B.meet @@ List.map go phis
      in
      go cof

    (* XXX: this function was never profiled *)
    let meet2 (thy1 : t) (thy2 : t) : t =
      (* a correct but unoptimized theory *)
      let draft =
        thy1 |> List.concat_map @@ fun (thy'1, (vars1, eqs1)) ->
        thy2 |> List.filter_map @@ fun (thy'2, (vars2, eqs2)) ->
        match Alg.meet2' thy'1 thy'2 with
        | `Inconsistent -> None
        | `Consistent thy' -> Some (thy', (VarSet.union vars1 vars2, eqs1 @ eqs2))
      in
      (* potentially expensive optimization *)
      refactor_branches @@ Alg.drop_useless_branches draft
  end
end
