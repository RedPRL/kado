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
    val meet2 : t -> t -> t
  end
end

module Make (P : Param) : S with type dim = P.dim and type var = P.var =
struct
  include P

  open Syntax

  type cof = (dim, var) free

  module UF = DisjointSet.Make (struct type t = dim let compare = compare_dim end)
  module VarSet = Set.Make (struct type t = var let compare = compare_var end)
  module B = Builder.Free.Make (P)

  (** A presentation of an algebraic theory over the language of intervals and cofibrations. *)
  type alg_thy' =
    { classes : UF.t;
      (** equivalence classes of dimensions *)

      true_vars : VarSet.t
    }

  type eq = dim * dim

  (** A [branch] represents the meet of a bunch of atomic cofibrations. *)
  type branch = VarSet.t * eq list
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
          ~f:(fun (vars, eqs) -> VarSet.add v vars, eqs)
      | Cof cof ->
        match cof with
        | Meet meet_cofs ->
          dissect_cofibrations @@ meet_cofs @ cofs
        | Join join_cofs ->
          List.concat_map join_cofs
            ~f:(fun join_cof -> dissect_cofibrations @@ join_cof :: cofs)
        | Eq (r, s) ->
          List.map (dissect_cofibrations cofs)
            ~f:(fun (vars, eqs) -> vars, (r, s) :: eqs)

  module Alg =
  struct
    type t = alg_thy
    type t' = alg_thy'

    let emp' =
      {classes = UF.empty;
       true_vars = VarSet.empty}

    let empty =
      `Consistent emp'

    let consistency =
      function
      | `Consistent _ -> `Consistent
      | `Inconsistent -> `Inconsistent

    let assume_vars (thy : t') vars =
      {thy with true_vars = VarSet.union vars thy.true_vars}

    let test_eq (thy : t') (r, s) =
      UF.test r s thy.classes

    (** [unsafe_test_and_assume_eq] fuses [test_eq] and [assume_eq] (if there was one).
      * It is "unsafe" because we do not check consistency here. *)
    let unsafe_test_and_assume_eq (thy : t') (r, s) =
      let testing, classes = UF.test_and_union r s thy.classes in
      testing, {thy with classes}

    let tri_test_eq thy (x, y) =
      match unsafe_test_and_assume_eq thy (x, y) with
      | true, _ -> `True
      | false, thy' ->
        if test_eq thy' (P.dim0, P.dim1) then
          `False
        else
          `Indeterminate

    let test_eqs (thy : t') eqs =
      List.for_all ~f:(test_eq thy) eqs

    let test_var (thy : t') v =
      VarSet.mem v thy.true_vars

    let test_vars (thy : t') vs =
      VarSet.subset vs thy.true_vars

    let test_branch (thy : t') (vars, eqs) =
      test_vars thy vars && test_eqs thy eqs

    (** [reduced_vars] takes out redundant cofibration variables. *)
    let reduce_vars (thy : t') vars =
      VarSet.diff vars thy.true_vars

    (** [reduce_eqs] detects inconsistency of an equation set and takes out
      * redundant equations. *)
    let reduce_eqs (thy : t') eqs =
      let go ((thy', eqs) as acc) eq =
        match unsafe_test_and_assume_eq thy' eq with
        | true, _ -> acc
        | false, thy' -> thy', Snoc (eqs, eq)
      in
      let thy', eqs = List.fold_left ~f:go ~init:(thy, Emp) eqs in
      match test_eq thy' (P.dim0, P.dim1) with
      | true -> `Inconsistent
      | false -> `Consistent (thy', BwdLabels.to_list eqs)

    (** [reduce_branch] detects inconsistency of a branch and takes out redundant
      * cofibration variables and equations. *)
    let reduce_branch (thy' : t') (vars, eqs) =
      match reduce_eqs thy' eqs with
      | `Inconsistent -> `Inconsistent
      | `Consistent (thy', eqs) ->
        `Consistent (assume_vars thy' vars, (reduce_vars thy' vars, eqs))

    (** [reduce_branches] removes inconsistent branches and takes out redundant
      * cofibration variables and equations. *)
    let reduce_branches (thy' : t') branches : cached_branches =
      let go branch =
        match reduce_branch thy' branch with
        | `Inconsistent -> None
        | `Consistent (thy', branch) -> Some (thy', branch)
      in
      List.filter_map ~f:go branches

    (** [drop_useless_branches] drops the branches that could be dropped without
      * affecting the coverages. *)
    let drop_useless_branches cached_branches : cached_branches =
      let go_fwd acc (thy', branch) =
        if BwdLabels.exists ~f:(fun (_, branch) -> test_branch thy' branch) acc then
          acc
        else
          Snoc (acc, (thy', branch))
      in
      let cached_branches = List.fold_left ~f:go_fwd ~init:Emp cached_branches in
      let go_bwd (thy', branch) acc =
        if List.exists ~f:(fun (_, branch) -> test_branch thy' branch) acc then
          acc
        else
          (thy', branch) :: acc
      in
      BwdLabels.fold_right ~f:go_bwd cached_branches ~init:[]

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
          drop_useless_branches @@
          reduce_branches thy' dissected_cofs

    (** [test] checks whether a cofibration is true within an algebraic theory *)
    let rec test (thy' : alg_thy') : cof -> bool =
      function
      | Cof Eq (r, s) ->
        test_eq thy' (r, s)
      | Cof Join phis ->
        List.exists ~f:(test thy') phis
      | Cof Meet phis ->
        List.for_all ~f:(test thy') phis
      | Var v ->
        test_var thy' v

    (* XXX: this function was never profiled *)
    let meet2' thy'1 thy'2 =
      let thy' =
        {classes = UF.merge thy'1.classes thy'2.classes;
         true_vars = VarSet.union thy'1.true_vars thy'2.true_vars}
      in
      match test_eq thy' (P.dim0, P.dim1) with
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
        | (_, (vars, _)) :: branches -> List.fold_left ~f:go ~init:vars branches
      in
      (* The following is checking whether individual equations are useful (not shared
       * by all the algebraic theories). It does not kill every "useless" equation where
       * the uselessness can be only observed after looking at multiple equations.
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
      let useful eq =
        List.exists cached_branches
          ~f:(fun (thy', _) -> not @@ Alg.test_eq thy' eq)
      in
      (* revisit all branches and remove all useless ones identified by the simple criterion above. *)
      List.map cached_branches
        ~f:(fun (thy', (vars, eqs)) -> thy', (VarSet.diff vars common_vars, List.filter ~f:useful eqs))

    (** [split thy cofs] adds to the theory [thy] the conjunction of a list of cofibrations [cofs]
      * and calculate the branches accordingly. This is similar to [Alg.split] in the spirit but
      * differs in detail. *)
    let split (thy : t) (cofs : cof list) : t =
      match dissect_cofibrations cofs with
      | [] -> []
      | [vars, []] when VarSet.is_empty vars -> thy
      | dissected_cofs ->
        Alg.drop_useless_branches @@
        List.concat_map thy
          ~f:(fun (thy', (vars, eq)) ->
              List.map (Alg.reduce_branches thy' dissected_cofs)
                ~f:(fun (thy', (sub_vars, sub_eqs)) ->
                    thy', (VarSet.union vars sub_vars, eq @ sub_eqs)))

    (** [assume thy cofs] is the same as [split thy cofs] except that it further refactors the
      * branches to optimize future searching. *)
    let assume (thy : t) (cofs : cof list) : t =
      refactor_branches @@ split thy cofs

    let test_sequent thy cx cof =
      List.for_all ~f:(fun (thy', _) -> Alg.test thy' cof) @@
      split thy cx

    let decompose thy =
      List.map thy ~f:(fun (thy', _) -> `Consistent thy')

    (* XXX: this function was never profiled *)
    let simplify_cof thy cof =
      let simplify_eq (x, y) =
        let has_true = ref false in
        let has_indet = ref false in
        let has_false = ref false in
        let () = List.iter thy ~f:(fun (thy', _) ->
            match Alg.tri_test_eq thy' (x, y) with
            | `True -> has_true := true
            | `False -> has_false := true
            | `Indeterminate -> has_indet := true)
        in
        match !has_true, !has_indet, !has_false with
        | _, true, _ | true, _, true -> Free.eq x y
        | _, false, false -> Free.top
        | false, false, _ -> Free.bot
      in
      let simplify_var v =
        if List.for_all thy ~f:(fun (thy', _) -> Alg.test_var thy' v) then
          Free.top
        else
          Free.var v
      in
      let rec go =
        function
        | Cof Eq (x, y) ->
          simplify_eq (x, y)
        | Var v ->
          simplify_var v
        | Cof Join phis ->
          B.join @@ List.map ~f:go phis
        | Cof Meet phis ->
          B.meet @@ List.map ~f:go phis
      in
      go cof

    (* XXX: this function was never profiled *)
    let meet2 (thy1 : t) (thy2 : t) : t =
      (* a correct but unoptimized theory *)
      let draft =
        List.concat_map thy1
          ~f:(fun (thy'1, (vars1, eqs1)) ->
              List.filter_map thy2
                ~f:(fun (thy'2, (vars2, eqs2)) ->
                    match Alg.meet2' thy'1 thy'2 with
                    | `Inconsistent -> None
                    | `Consistent thy' -> Some (thy', (VarSet.union vars1 vars2, eqs1 @ eqs2))))
      in
      (* potentially expensive optimization *)
      refactor_branches @@ Alg.drop_useless_branches draft
  end
end
