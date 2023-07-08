/-

  This file constains data-structures and operations on the internal
  representation of DAEs.

  -/

include "tuple.mc"
include "peval/peval.mc"

include "mexpr/free-vars.mc"
include "mexpr/constant-fold.mc"

include "../miking-pead/ad.mc"

include "./ast_gen.mc"
include "./ast.mc"
include "./pprint.mc"
include "./desugar.mc"
include "./error-print.mc"

let mapSwitchKeyVal = lam cmp.
  mapFoldWithKey
    (lam newMap. lam name. lam index. mapInsert index name newMap)
    (mapEmpty cmp)

lang DAE = MExprFreeVars + MExprPEval + MExprConstantFold + AD
  type Eqn = (Expr, Expr)
  type IEqn = (Name, Expr)
  type Binding = (Name, Expr)
  type Output = (Name, Expr)
  type DepVarNameMap = Map (Name, Int) Name
  type NameDepVarMap = Map Name (Name, Int)

  type DAE = {
    vars : Map Name Type,
    depVarNameMap : DepVarNameMap,
    nameDepVarMap : NameDepVarMap,
    bindings : [Binding],
    ieqns : [IEqn],
    explicitDiffEqns : [(Name, Expr)],
    eqns : [Eqn],
    output : Expr
  }

  type DAEStructure = {
    vars : [Name],
    depVarNameMap : DepVarNameMap,
    nameDepVarMap : NameDepVarMap,
    bindings : [(Name, [Name])],
    ieqns : [(Name, [Name])],
    explicitDiffEqns : [(Name, [Name])],
    eqns : [([Name], [Name])],
    output : [Name]
  }

  type DAEJacobian = {
    bindings : [Binding],
    explicitDiffPartials : [(Name, Map Name Expr)],
    adResidual : [Expr],
    nameTangentNameMap : Map Name Name,
    adPartials : Map Name [Int],
    specPartials : [Map Name Expr]
  }

  type DAEJacobianStructure = {
    bindings : [(Name, [Name])],
    explicitDiffPartials : [(Name, [(Name, [Name])])],
    adResidual : [[Name]],
    nameTangentNameMap : Map Name Name,
    adPartials : [(Name, [Int])],
    specPartials : [[(Name, [Name])]]
  }

  sem daeDer : DAE -> Name -> Option Name
  sem daeDer dae =| name ->
    match
      mapFindOrElse
        (lam. error (join ["daeDer: impossible: ", nameGetStr name]))
        name
        dae.nameDepVarMap
      with (name, i)
    in
    mapLookup (name, succ i) dae.depVarNameMap

  sem daeAntiDer : DAE -> Name -> Option Name
  sem daeAntiDer dae =| name ->
    switch mapLookup name dae.nameDepVarMap
    case Some (name, n) then mapLookup (name, pred n) dae.depVarNameMap
    case None _ then error "impossible"
    end

  sem daeAD : DAE -> Int -> Name -> Option [Name]
  sem daeAD dae n =| name ->
    match
      mapFindOrElse
        (lam. error (join ["daeAD: impossible: ", nameGetStr name]))
        name
        dae.nameDepVarMap
      with (name, j)
    in
    optionMapM
      (lam i.
        mapLookup (name, addi j i) dae.depVarNameMap)
      (create (succ n) (lam i. i))

  sem _daeFreeVarsEqns : [Eqn] -> [Set Name]
  sem _daeFreeVarsEqns =| eqns ->
    map (lam eqn. setUnion (freeVars eqn.0) (freeVars eqn.1)) eqns

  sem daeDepVarNamesEqns : DAE -> [Set Name]
  sem daeDepVarNamesEqns =| dae ->
    let vars = setOfKeys dae.nameDepVarMap in
    map (setIntersect vars) (_daeFreeVarsEqns dae.eqns)

  sem daeDepVarNamesExplicitDiffEqns : DAE -> [Set Name]
  sem daeDepVarNamesExplicitDiffEqns =| dae ->
    let vars = setOfKeys dae.nameDepVarMap in
    map
      (setIntersect vars)
      (map freeVars (unzip dae.explicitDiffEqns).1)

  sem daeDepVarsEqns : DAE -> [Set (Name, Int)]
  sem daeDepVarsEqns =| dae ->
    let vars = setOfKeys dae.nameDepVarMap in
    map
      (lam fv.
        setFold
          (lam s. lam name. setInsert (mapFindExn name dae.nameDepVarMap) s)
          (setEmpty (tupleCmp2 nameCmp subi))
          (setIntersect vars fv))
      (_daeFreeVarsEqns dae.eqns)

  sem _daeFreeVars : DAE -> Set Name
  sem _daeFreeVars =| dae ->
    let fvDefs = foldl (lam fv. lam d. setUnion fv (freeVars d.1)) in
      foldl1 setUnion [
        freeVars dae.output,
        foldl setUnion (setEmpty nameCmp) (_daeFreeVarsEqns dae.eqns),
        foldl fvDefs (setEmpty nameCmp) [
          dae.explicitDiffEqns, dae.ieqns, dae.bindings
        ]
      ]

  sem daeExprStructure : Expr -> [Name]
  sem daeExprStructure =| expr -> sort nameCmp (setToSeq (freeVars expr))

  sem daeDefnStructure : (Name, Expr) -> (Name, [Name])
  sem daeDefnStructure =| defn -> (defn.0, daeExprStructure defn.1)

  sem daeEqnStructure : (Expr, Expr) -> ([Name], [Name])
  sem daeEqnStructure =| eqn -> (daeExprStructure eqn.0, daeExprStructure eqn.1)

  sem daeStructure : DAE -> DAEStructure
  sem daeStructure =| dae ->
    {
      vars = sort nameCmp (mapKeys dae.vars),
      depVarNameMap = dae.depVarNameMap,
      nameDepVarMap = dae.nameDepVarMap,
      bindings = map daeDefnStructure dae.bindings,
      explicitDiffEqns = map daeDefnStructure dae.explicitDiffEqns,
      ieqns = map daeDefnStructure dae.ieqns,
      eqns = map daeEqnStructure dae.eqns,
      output = daeExprStructure dae.output
    }

  sem daeJacobianStructure : DAEJacobian -> DAEJacobianStructure
  sem daeJacobianStructure =| jac ->
    let defStructure = lam def. (def.0, daeExprStructure def.1) in
    let cmp = lam x. lam y. nameCmp x.0 y.0 in
    let explicitDiffPartials =
      map
        (lam p. (p.0, sort cmp (map daeDefnStructure (mapBindings p.1))))
        jac.explicitDiffPartials
    in
    let specPartials =
      map (lam p. sort cmp (map daeDefnStructure (mapBindings p))) jac.specPartials
    in
    {
      bindings = map daeDefnStructure jac.bindings,
      explicitDiffPartials = explicitDiffPartials,
      adResidual = map daeExprStructure jac.adResidual,
      nameTangentNameMap = jac.nameTangentNameMap,
      adPartials = mapBindings jac.adPartials,
      specPartials = specPartials
    }

  sem daeFreeVars : DAE -> Set Name
  sem daeFreeVars =| dae ->
    setSubtract (_daeFreeVars dae) (setOfKeys dae.nameDepVarMap)

  sem daeDepVarNames : DAE -> Set Name
  sem daeDepVarNames =| dae ->
    setIntersect (_daeFreeVars dae) (setOfKeys dae.nameDepVarMap)

  sem daeUpdateVarMap : DAE -> DAE
  sem daeUpdateVarMap =| dae ->
    let names = daeDepVarNames dae in
    let mem = flip setMem names in
    let nameDepVarMap =
      mapFilterWithKey (lam name. lam. mem name) dae.nameDepVarMap
    in
    let depVarNameMap = mapFilterWithKey (lam. mem) dae.depVarNameMap in
    {
      dae with
      nameDepVarMap = nameDepVarMap,
      depVarNameMap = depVarNameMap
    }

  sem daeElimDeadCode : DAE -> DAE
  sem daeElimDeadCode =| dae ->
    let fv = daeFreeVars dae in
    let bindings = filter (lam b. setMem b.0 fv) dae.bindings in
    let eqns =
      filter
        (lam eqn.
          match
            eqExprH
              { varEnv = biEmpty, conEnv = biEmpty }
              {
                varEnv =
                map (lam name. (name, name)) (mapValues dae.depVarNameMap),
                conEnv = biEmpty
              }
              eqn.0
              eqn.1
            with Some _
          then false else true)
        dae.eqns
    in
    { dae with bindings = bindings, eqns = eqns }

  sem daePEval : DAE -> DAE
  sem daePEval =| dae ->
    let nameDepVarMap = dae.depVarNameMap in
    let env =
      foldl
        (lam env. lam binding.
          match binding with (name, expr) in
          let val =
            constantfold
              (pevalBind { pevalCtxEmpty () with env = env } (lam x. x) expr)
          in
          evalEnvInsert name val env)
        (evalEnvEmpty ())
        dae.bindings
    in
    let pevalExpr = lam env. lam expr.
      pevalExpr { pevalCtxEmpty () with env = env } expr
    in
    match
      foldl
        (lam acc. lam eqn.
          match acc with (dae, env) in
          switch eqn
          case (TmVar r, rhs) | (rhs, TmVar r) then
            match daeDer dae r.ident with None _ then (dae, env)
            else
              if setSubset (freeVars rhs) (setOfKeys dae.nameDepVarMap) then (dae, env)
              else
                ({ dae with
                   vars = mapRemove r.ident dae.vars,
                   nameDepVarMap = mapRemove r.ident dae.nameDepVarMap },
                 evalEnvInsert r.ident (pevalExpr env rhs) env)
          case (_, _) then (dae, env)
          end)
        (dae, env)
        dae.eqns
      with (dae, env)
    in
    let pevalExpr = lam expr. constantfold (pevalExpr env expr) in
    let pevalDefs = map (lam d. (d.0, pevalExpr d.1)) in
    let depVarNameMap =
      mapSwitchKeyVal (mapGetCmpFun dae.depVarNameMap) dae.nameDepVarMap
    in
    let dae = {
      dae with
      depVarNameMap = depVarNameMap,
      bindings = pevalDefs dae.bindings,
      ieqns = pevalDefs dae.ieqns,
      explicitDiffEqns = pevalDefs dae.explicitDiffEqns,
      eqns = map (lam eqn. (pevalExpr eqn.0, pevalExpr eqn.1)) dae.eqns,
      output = pevalExpr dae.output
    } in
    if gti (mapSize nameDepVarMap) (mapSize dae.nameDepVarMap) then
      daePEval dae
    else
      daeElimDeadCode dae

  sem _daeDepVarNameSparsityDivision : Int -> DAE -> (Set Name, Set Name)
  sem _daeDepVarNameSparsityDivision bound =| dae ->
    let names = setToSeq (setOfKeys dae.nameDepVarMap) in
    let eqnNames = daeDepVarNamesEqns dae in
    let count = lam name.
      foldl
        (lam count. lam names. if setMem name names then succ count else count)
        0
        eqnNames
    in
    match partition (lam name. lti (count name) bound) names
      with (sparseNames, denseNames)
    in
    let setOfSeq = setOfSeq nameCmp in
    (setOfSeq sparseNames, setOfSeq denseNames)

  sem _daeNonZeroADPartials : DAE -> Map Name [Int]
  sem _daeNonZeroADPartials =| dae ->
    let names = setToSeq (setOfKeys dae.nameDepVarMap) in
    let eqnNames = daeDepVarNamesEqns dae in
    mapFromSeq nameCmp
      (map
         (lam name.
           let idxs =
             mapi
               (lam i. lam names. if setMem name names then Some i else None ())
               eqnNames
           in
           (name, filterOption idxs))
         names)

  sem daeJacobian : Int -> DAE -> DAEJacobian
  sem daeJacobian sparsityBound =| dae ->
    let b = peadAstBuilder (NoInfo ()) in
    let pevalExpr = lam expr. constantfold (pevalExpr (pevalCtxEmpty ()) expr) in
    let residuals = map (uncurry subf_) dae.eqns in
    let xss = setToSeq (setOfKeys dae.nameDepVarMap) in
    -- AD transform bindings
    let bindings =
      map (lam b. (b.0, pevalExpr (adExpr (adCtxEmpty ()) 1 b.1))) dae.bindings
    in
    -- Divide dependent variables between sparse and dense
    match _daeDepVarNameSparsityDivision sparsityBound dae
      with (sparseXss, denseXss)
    in
    -- AD transform residuals if we have dense partials
    let nameTangentNameMap =
      if setIsEmpty denseXss then []
      else
        map (lam name. (name, nameSym (join ["δ", nameGetStr name]))) xss
    in
    let adCtx =
      foldl
        (lam ctx. lam names.
          match names with (name, dname) in
          adCtxEnvInsert name (b.dualnum (nvar_ name) (nvar_ dname)) ctx)
        (adCtxEmpty ())
        nameTangentNameMap
    in
    let adResidual =
      if setIsEmpty denseXss then []
      else
        map (lam expr. pevalExpr (b.tangent (adExpr adCtx 1 expr))) residuals
    in
    let adPartials =
      mapFilterWithKey (lam name. lam. setMem name denseXss) (_daeNonZeroADPartials dae)
    in
    -- Specialize partial derivatives for sparse dependent variables
    let adCtx =
      foldl
        (lam ctx. lam name.
          adCtxEnvInsert name (b.dualnum (nvar_ name) (float_ 0.)) ctx)
        (adCtxEmpty ())
        xss
    in
    let partials = lam expr. lam xs.
      mapMapWithKey
        (lam x. lam.
          let adCtx =
            adCtxEnvInsert x (b.dualnum (nvar_ x) (float_ 1.)) adCtx
          in
          let adExpr = adExpr adCtx 1 in
          pevalExpr (b.tangent (adExpr expr)))
        xs
    in
    let xss = daeDepVarNamesExplicitDiffEqns dae in
    let explicitDiffPartials =
      zipWith
        (lam eqn. lam xs.
          match eqn with (name, rhs) in
          (name, partials (subf_ (float_ 0.) rhs) xs))
        dae.explicitDiffEqns
        xss
    in
    let xss = daeDepVarNamesEqns dae in
    let eqnPartials =
      zipWith partials residuals (map (setIntersect sparseXss) xss)
    in
    {
      bindings = bindings,
      explicitDiffPartials = explicitDiffPartials,
      adResidual = adResidual,
      nameTangentNameMap = mapFromSeq nameCmp nameTangentNameMap,
      adPartials = adPartials,
      specPartials = eqnPartials
    }
end

lang DAEExpr = DAE + DAEAst + DAEParsePrettyPrint + MExprFreeVars
  sem _exprStructureErrorMsg : Info -> ErrorSection
  sem _exprStructureErrorMsg =| info -> {
    errorDefault with
    msg = join [
      "Invalid expression structure, ",
      "unable to intepret this expression as part of a DAE"
    ],
    info = info
  }

  -- DAE Expression to DAE
  sem daeExprToDAE : Expr -> Res DAE
  sem daeExprToDAE =
  | t -> daeExprToDAEH {
    vars = mapEmpty nameCmp,
    depVarNameMap = mapEmpty (tupleCmp2 nameCmp subi),
    nameDepVarMap = mapEmpty nameCmp,
    bindings = [],
    ieqns = [],
    explicitDiffEqns = [],
    eqns = [],
    output = unit_
  } t

  sem daeExprToDAEH : DAE -> Expr -> Res DAE
  sem daeExprToDAEH dae =
  | TmLet r ->
    match daeExprToExpr dae r.body with (dae, body) in
    result.bind body
      (lam body.
        let dae = { dae with bindings = snoc dae.bindings (r.ident, body) } in
        daeExprToDAEH dae r.inexpr)
  | TmLam (r & {body = TmLam _}) ->
    let dae = { dae with vars = mapInsert r.ident r.tyParam dae.vars } in
    daeExprToDAEH dae r.body
  | TmLam r ->
    let dae = { dae with vars = mapInsert r.ident r.tyParam dae.vars } in
    daeExprBodyToDAEH dae r.body
  | t -> result.err (_exprStructureErrorMsg (infoTm t))

  sem daeExprBodyToDAEH : DAE -> Expr -> Res DAE
  sem daeExprBodyToDAEH dae =
  | TmLet r ->
    match daeExprToExpr dae r.body with (dae, body) in
    result.bind body
      (lam body.
        let dae = { dae with bindings = snoc dae.bindings (r.ident, body) } in
        daeExprBodyToDAEH dae r.inexpr)
  | TmRecord r ->
    switch
      map
        (lam l. mapLookup (stringToSid l) r.bindings)
        ["ieqns", "eqns", "output"]
    case [Some (TmRecord ieqns), Some (TmRecord eqns), Some output] then
      match map (lam r. record2tuple r.bindings) [ieqns, eqns] with
        [Some ieqns, Some eqns]
      then
        match result.mapAccumLM daeExprIEqnToIEqn dae ieqns with (dae, ieqns) in
        match result.mapAccumLM daeExprEqnToEqn dae eqns with (dae, eqns) in
        match daeExprToExpr dae output with (dae, output) in
        result.bind3 ieqns eqns output
          (lam ieqns. lam eqns. lam output. result.ok {
              dae with
              ieqns = ieqns,
              eqns = eqns,
              output = output
            })
      else result.err (_exprStructureErrorMsg r.info)
    case _ then result.err (_exprStructureErrorMsg r.info)
    end

  sem daeExprIEqnToIEqn : DAE -> Expr -> (DAE, Res IEqn)
  sem daeExprIEqnToIEqn dae =| t ->
    let nonExplicitErr = lam info. result.err {
      errorDefault with
      msg = "Non-explicit initial equation",
      info = info
    } in
    match daeExprEqnToEqn dae t with (dae, eqn) in
    (dae,
     result.bind eqn
       (lam eqn.
         switch eqn
         case (TmVar r, rhs) then
           if mapMem r.ident dae.nameDepVarMap then
             if setIsEmpty
                  (setIntersect
                     (setOfKeys dae.nameDepVarMap)
                     (freeVars rhs))
             then
               result.ok (r.ident, rhs)
             else
               nonExplicitErr (infoTm rhs)
           else
             let err = {
               errorDefault with
               msg = strJoin " " [
                 "Left-hand side",
                 nameGetStr r.ident,
                 "in initial equation is not a dependent variable"
               ],
               info = r.info
             } in
             result.err err
         case (lhs, _) then nonExplicitErr (infoTm lhs)
         end))

  sem daeExprEqnToEqn : DAE -> Expr -> (DAE, Res Eqn)
  sem daeExprEqnToEqn dae =| t ->
    match daeExprToExpr dae t with (dae, t) in
    (dae,
     result.bind t
       (lam t.
         match t with
           TmApp {
             lhs = TmApp {lhs = TmConst { val = CSubf _}, rhs = lhs},
             rhs = rhs
           }
         then result.ok (lhs, rhs)
         else result.err (_exprStructureErrorMsg (infoTm t))))

  sem daeExprToExpr : DAE -> Expr -> (DAE, Res Expr)
  sem daeExprToExpr dae =
  | t & TmVar r ->
    if mapMem r.ident dae.vars then
      let depVarNameMap = mapInsert (r.ident, 0) r.ident dae.depVarNameMap in
      let nameDepVarMap = mapInsert r.ident (r.ident, 0) dae.nameDepVarMap in
      let dae = {
        dae with
        depVarNameMap = depVarNameMap,
        nameDepVarMap = nameDepVarMap
      } in
      (dae, result.ok t)
    else
      (dae, result.ok t)
  | TmPrim (TmVar r) ->
    optionMapOrElse
      (lam.
        optionMapOrElse
          (lam.
            let err = {
              errorDefault with
              msg = join [nameGetStr r.ident, " is not a dependent variable"],
              info = r.info
            } in
            (dae, result.err err))
          (lam.
            let newident = nameSym (join [nameGetStr r.ident, "'"]) in
            let keys = [(r.ident, 0), (r.ident, 1)] in
            let vals = [r.ident, newident] in
            let f = lam m. lam k. lam v. mapInsert k v m in
            let depVarNameMap = foldl2 f dae.depVarNameMap keys vals in
            let nameDepVarMap = foldl2 f dae.nameDepVarMap vals keys in
            let dae = {
              dae with
              depVarNameMap = depVarNameMap,
              nameDepVarMap = nameDepVarMap
            } in
            (dae, result.ok (TmVar { r with ident = newident })))
          (mapLookup r.ident dae.vars))
      (lam ident. (dae, result.ok (TmVar { r with ident = ident })))
      (mapLookup (r.ident, 1) dae.depVarNameMap)
  | TmPrim t ->
    let err = {
      errorDefault with
      msg = join [
        "Invalid use of prim (').",
        "It can only be applied on dependent variables"
      ],
      info = infoTm t
    } in
    (dae, result.err err)
  | t -> smapAccumL_ResultM_Expr_Expr daeExprToExpr dae t
end

let _indent = "  "

lang DAEPrettyPrint =
  DAEAst + DAEParseDesugar + DAEParsePrettyPrint +
  MExprPrettyPrint + MExprFreeVars

  sem _defToString : (Expr -> String) -> (Name, Expr) -> String
  sem _defToString exprToString =
  | (name, expr) ->
    join [nameGetStr name, " := ", exprToString expr]

  sem _topToString : [String] -> String
  sem _topToString =| rows -> strJoin "\n\n" rows

  sem _sectionToString : String -> String -> [String] -> String
  sem _sectionToString indent name =| rows ->
    strJoin
      (join ["\n", indent, _indent])
      [join [name, ":"], strJoin (join ["\n", indent, _indent]) rows]

  sem _daeToString : (Expr -> String) -> DAE -> String
  sem _daeToString exprToString =| dae ->
    let dv = lam v.
      join ["d", int2string v.1, " ", nameGetStr v.0]
    in
    let def = _defToString exprToString in
    let sec = _sectionToString "" in
    _topToString [
      sec "vars"
        (map
           (lam var. join [nameGetStr var.0, ":", type2str var.1])
           (mapBindings dae.vars)),
      sec "varmap"
        (map
           (lam x.
             match x with (var, name2) in
             join [dv var, " -> ", nameGetStr name2])
           (sort
              (lam x. lam y. subi (x.0).1 (y.0).1)
              (mapBindings dae.depVarNameMap))),
      sec "varmap"
        (map
           (lam x.
             match x with (name1, var) in
             join [nameGetStr name1, " -> ", dv var])
           (sort
              (lam x. lam y. subi (x.1).1 (y.1).1)
              (mapBindings dae.nameDepVarMap))),
      sec "bindings" (map def dae.bindings),
      sec "init" (map def dae.ieqns),
      sec "explicit_diff_eqs"
        (mapi
           (lam i. lam eqn.
             join ["r[", int2string i, "] : d/dt ", def eqn])
           dae.explicitDiffEqns),
      sec "eqs"
        (mapi
           (lam i. lam eqn.
             join [
               "r[", int2string (addi i (length dae.explicitDiffEqns)), "] : ",
               exprToString eqn.0,
               " = ", exprToString eqn.1
             ])
           dae.eqns),
      sec "output" [exprToString dae.output]
    ]

  sem _exprToDAEExprString : Expr -> String
  sem _exprToDAEExprString =| expr ->
    optionMapOr
      (join ["[", expr2str expr, "]"]) daeExprToString (daeResugarExpr expr)

  sem daeToString : DAE -> String
  sem daeToString =| dae ->
    _daeToString _exprToDAEExprString dae

  sem daeStructureToString : DAE -> String
  sem daeStructureToString =| dae ->
    _daeToString (lam expr. setToString nameGetStr (freeVars expr)) dae

  sem daeJacobianToString : DAEJacobian -> String
  sem daeJacobianToString =| jac ->
    let nExplcit = (length jac.explicitDiffPartials) in
    let expr2str = _exprToDAEExprString in
    let def = _defToString expr2str in
    let sec = _sectionToString in
    _topToString [
      sec "" "bindings" (map def jac.bindings),
      sec "" "explicitDiffPartials"
        (mapi
           (lam i. lam x.
             match x with (name, jX) in
             sec _indent
               (join ["dr[", (int2string i), ",:]"])
               (cons
                  (join ["d/dt ", nameGetStr name, " := 1."])
                  (map def (mapBindings jX))))
           jac.explicitDiffPartials),
      sec "" "adResidual"
        (mapi
           (lam i. lam expr.
             join [
               "dr[", (int2string (addi i nExplcit)), ",:] ", expr2str expr
             ])
           jac.adResidual),
      sec "" "nameTangentNameMap"
        (map
           (lam x. join [nameGetStr x.0 , " -> ", nameGetStr x.1])
           (mapBindings jac.nameTangentNameMap)),
      sec "" "adPartials"
        (map
           (lam b. join [nameGetStr b.0, " -> ", seqToString int2string b.1])
           (mapBindings jac.adPartials)),
      sec "" "specPartials"
        (mapi
           (lam i. lam jX.
             sec _indent
               (join ["dr[", (int2string (addi i nExplcit)), ",:]"])
               (map def (mapBindings jX)))
           jac.specPartials)
    ]

end
