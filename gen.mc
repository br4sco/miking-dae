include "seq.mc"
include "option.mc"
include "these.mc"

include "./ast.mc"
include "./transform.mc"
include "./dae.mc"

lang DAEGen = DAE + DAEPrettyPrint + MExprPrettyPrint
  type ResidualState = {
    indexMap : Map Name (These Int Int),
    names : [These Name Name]
  }

  sem daeResidualStateToString : [These Name Name] -> String
  sem daeResidualStateToString =| names ->
    let xNames = theseCatHere names in
    let xpNames = map theseGetThere names in
    join [
      strJoin "\n"
        (mapi
           (lam i. lam x. join ["x[", int2string i, "] = ", nameGetStr x])
           xNames
        ),
      "\n\n",
      strJoin "\n"
        (mapi
           (lam i. lam x.
             join [
               "x'[", int2string i, "] = ",
               optionMapOr "_" nameGetStr x
             ])
           xpNames)
    ]

  sem daeResidualStateNames : DAE -> [These Name Name]
  sem daeResidualStateNames =| dae ->
    let varOffset =
      foldl
        (lam ofs. lam k.
          match k with (name, d) in
          mapUpdate
            name
            (optionMapOr (Some d) (lam i. Some (maxi d i)))
            ofs)
        (mapEmpty nameCmp)
        (mapKeys dae.depVarNameMap)
    in
    let names =
      map
        theseThis
        (mapValues
           (mapFilterWithKey
              (lam k. lam.
                match k with (name, n) in
                if eqi n 0 then true
                else lti n (mapFindExn name varOffset))
              dae.depVarNameMap))
    in
    map
      (lam x.
        theseBindThis x
          (lam x.
            optionMapOr
              (This x)
              (lam xp. These (x, xp))
              (daeDer dae x)))
      names

  sem daeResidualStateIndexMap : [These Name Name] -> Map Name (These Int Int)
  sem daeResidualStateIndexMap =| names ->
    let errMsg = lam name. join [
        "daeResidualStateIndexMap: Invalid residual state: ",
        nameGetStr name
      ]
    in
    let names =
      mapi (lam i. theseBiMap (lam name. (name, i)) (lam name. (name, i))) names
    in
    match thesePartition names with (xs, xps, xxps) in
    let fthis =
      lam acc. lam x.
        mapUpdate x.0
          (lam v.
            switch v
            case Some (That i) then Some (These (x.1, i))
            case None _ then Some (This x.1)
            case _ then error (errMsg x.0)
            end)
          acc
    in
    let fthat =
      lam acc. lam x.
        mapUpdate x.0
          (lam v.
            switch v
            case Some (This i) then Some (These (i, x.1))
            case None _ then Some (That x.1)
            case _ then error (errMsg x.0)
            end)
          acc
    in
    let indexMap = foldl fthis (mapEmpty nameCmp) xs in
    let indexMap = foldl fthat indexMap xps in
    foldl (lam acc. lam x. fthat (fthis acc x.0) x.1) indexMap xxps

  sem daeResidualState : DAE -> ResidualState
  sem daeResidualState =| dae ->
    let names = daeResidualStateNames dae in
    { names = names, indexMap = daeResidualStateIndexMap names }

  sem _Xi : Map Name (These Int Int) -> Name -> Option Int
  sem _Xi indexMap =| name ->
    optionBind (mapLookup name indexMap) theseGetHere

  sem _XiExn : Map Name (These Int Int) -> Name -> Int
  sem _XiExn indexMap =| name ->
    optionGetOrElse
      (lam. error (join ["_XiExn: ", nameGetStr name, " not in x"]))
      (_Xi indexMap name)

  sem _vvGet : Name -> Int -> Expr
  sem _vvGet vv =| i ->
    let tget = tensorLinearGetExn_ tyfloat_ in
    tget (nvar_ vv) (int_ i)

  sem _vvSet : Name -> Int -> Expr -> Expr
  sem _vvSet vv i =| v ->
    let tset = tensorLinearSetExn_ tyfloat_ in
    tset (nvar_ vv) (int_ i) v

  sem _vvSeti : Name -> Name -> Expr -> Expr
  sem _vvSeti vv i =| v ->
    let tset = tensorLinearSetExn_ tyfloat_ in
    tset (nvar_ vv) (nvar_ i) v

  sem _mmGet : Name -> Int -> Int -> Expr
  sem _mmGet mm i =| j ->
    let mget = tensorGetExn_ tyfloat_ in
    -- NOTE(oerikss, 2023-05-18): The Jacobian matrix is column-major in IDA
    mget (nvar_ mm) (seq_ [int_ j, int_ i])

  sem _mmSet : Name -> Int -> Int -> Expr -> Expr
  sem _mmSet mm i j =| v ->
    let mset = tensorSetExn_ tyfloat_ in
    -- NOTE(oerikss, 2023-05-18): The Jacobian matrix is column-major in IDA
    mset (nvar_ mm) (seq_ [int_ j, int_ i]) v

  sem _mmGetj : Name -> Int -> Name -> Expr
  sem _mmGetj mm i =| j ->
    let mget = tensorGetExn_ tyfloat_ in
    -- NOTE(oerikss, 2023-05-18): The Jacobian matrix is column-major in IDA
    mget (nvar_ mm) (seq_ [nvar_ j, int_ i])

  sem _mmSetj : Name -> Int -> Name -> Expr -> Expr
  sem _mmSetj mm i j =| v ->
    let mset = tensorSetExn_ tyfloat_ in
    -- NOTE(oerikss, 2023-05-18): The Jacobian matrix is column-major in IDA
    mset (nvar_ mm) (seq_ [nvar_ j, int_ i]) v

  sem _mmSetij : Name -> Name -> Name -> Expr -> Expr
  sem _mmSetij mm i j =| v ->
    let mset = tensorSetExn_ tyfloat_ in
    -- NOTE(oerikss, 2023-05-18): The Jacobian matrix is column-major in IDA
    mset (nvar_ mm) (seq_ [nvar_ j, nvar_ i]) v

  sem _mmGetij : Name -> Name -> Name -> Expr
  sem _mmGetij mm i =| j ->
    let mget = tensorGetExn_ tyfloat_ in
    -- NOTE(oerikss, 2023-05-18): The Jacobian matrix is column-major in IDA
    mget (nvar_ mm) (seq_ [nvar_ j, nvar_ i])

  sem _dvarsToResidualStateAccess
    : Name -> Name -> ResidualState -> Expr -> Expr

  sem _dvarsToResidualStateAccess x xp state =
  | expr & TmVar r ->
    optionMapOr expr
      (theseThese (_vvGet x) (_vvGet xp) (lam i. lam. _vvGet x i))
      (mapLookup r.ident state.indexMap)
  | expr -> smap_Expr_Expr (_dvarsToResidualStateAccess x xp state) expr

  sem _daeGenResidualStateBindings
    : Name -> Name -> ResidualState -> Expr

  sem _daeGenResidualStateBindings x xp =| state ->
    let bindings =
      foldli
        (lam acc. lam i.
          theseThese
            (lam name. snoc acc (nulet_ name (_vvGet x i)))
            (lam name. snoc acc (nulet_ name (_vvGet xp i)))
            (lam xName. lam xpName.
              concat acc
                [nulet_ xName (_vvGet x i), nulet_ xpName (_vvGet xp i)]))
        []
        state.names
    in
    bindall_ (snoc bindings unit_)

  sem _daeGenPrintWithComma : [Expr] -> Expr
  sem _daeGenPrintWithComma =| ts ->
    if null ts then unit_
    else
      foldl1
        semi_
        (snoc
           (map
              (lam t. semi_ (print_ t) (print_ (str_ ",")))
              (init ts))
           (semi_ (print_ (last ts)) (print_ (str_ "\n"))))

  sem daeGenVarIdVector : ResidualState -> Expr
  sem daeGenVarIdVector = | state ->
    seq_ (map (theseThese (lam. false_) (lam. true_) (lam. lam. true_)) state.names)

  sem daeGenVarsVectorExn : ResidualState -> [Name] -> Expr
  sem daeGenVarsVectorExn state =| names ->
    seq_
      ((map
          (lam name. utuple_ [
            str_ (nameGetStr name),
            int_ (mapFindExn name state.xxNameIndexMap)
          ]))
         names)

  type DaeGenerateInitialValuesCodeArg = {
    state : ResidualState,
    ieqns : [IEqn]
  }
  sem daeGenInitialFun : DaeGenerateInitialValuesCodeArg -> Expr
  sem daeGenInitialFun =
  | arg ->
    let inits =
      foldl
        (lam inits. lam eqn.
          match eqn with (name, expr) in
          mapUpdate
            name
            (lam exprs.
              switch exprs
              case Some exprs then Some (snoc exprs expr)
              case None _ then Some [expr]
              end)
            inits)
        (mapEmpty nameCmp)
        arg.ieqns
    in
    let _x = nameSym "x" in
    let _xp = nameSym "x'" in
    let inexpr =
      bindall_
        (map (ulet_ "")
           (snoc
              (mapFoldWithKey
                 (lam acc. lam name. lam exprs.
                   let expr =
                     switch exprs
                     case [expr] then expr
                     case [_] ++ _ then
                       -- TODO(oerikss, 2023-05-08): If the same dependent
                       -- variable has multiple initial values we should
                       -- probably give a warning.
                       divf_
                         (foldl1 addf_ exprs)
                         (float_ (int2float (length exprs)))
                     case _ then error "Impossible"
                     end
                   in
                   let setX = _vvSet _x in
                   let setXP = _vvSet _xp in
                   optionMapOr
                     acc
                     (theseThese
                        (lam i. cons (setX i expr) acc)
                        (lam i. cons (setXP i expr) acc)
                        (lam i. lam j. concat [setX i expr, setXP j expr] acc))
                     (mapLookup name arg.state.indexMap))
                 []
                 inits)
              unit_))
    in
    nulams_ [_x, _xp] inexpr

  type DaeGenerateResidualFunArg = {
    state : ResidualState,
    explicitDiffEqns : [(Name, Expr)],
    eqns : [Eqn]
  }
  sem daeGenResidualFun : DaeGenerateResidualFunArg -> Expr
  sem daeGenResidualFun =
  | arg ->
    let _x = nameSym "x" in
    let _xp = nameSym "x'" in
    let _r = nameSym "r" in
    let residual =
      map
        (lam eqn.
          switch eqn
          case
            (TmConst {val = CFloat r}, t)
          | (t, TmConst {val = CFloat r})
          then
            if eqf r.val 0. then t else subf_ eqn.0 eqn.1
          case (lhs, rhs) then subf_ lhs rhs
          end)
        arg.eqns
    in
    let explicitDiffResidual =
      map
        (lam eqn.
          match eqn with (name, expr) in
          let lhs = _vvGet _xp (_XiExn arg.state.indexMap name) in
          subf_ lhs expr)
        arg.explicitDiffEqns
    in
    let inexpr =
      bindall_
        (snoc
           (mapi
              (lam i. lam v. ulet_ "" (_vvSet _r i v))
              (concat explicitDiffResidual residual))
           unit_)
    in
    nulams_ [_x, _xp, _r]
      (bind_ (_daeGenResidualStateBindings _x _xp arg.state) inexpr)
  -- (_dvarsToResidualStateAccess _x _xp arg.state inexpr)

  sem daeGenHeaderFun : Expr -> Expr
  sem daeGenHeaderFun =| output ->
    let body =
      switch tyTm output
      case TyRecord r then
        match record2tuple r.fields with None _ then
          _daeGenPrintWithComma
            (map (lam k. str_ (sidToString k)) (mapKeys r.fields))
        else unit_
      case _ then unit_
      end
    in
    ulam_ "" body

  sem daeGenOutputFun : ResidualState -> Expr -> Res Expr
  sem daeGenOutputFun state =| output ->
    let ok = result.ok in
    let warnWithUnit = lam ty.
      let warn = result.warn {
        errorDefault with
        msg = strJoin "\n" [
          "Cannot print output expressions of type:",
          type2str ty
        ],
        info = infoTm output
      } in
      result.withAnnotations warn (ok unit_)
    in
    let _x = nameSym "x" in
    let _xp = nameSym "x'" in
    let output = _dvarsToResidualStateAccess _x _xp state output in
    let body =
      let ty = tyTm output in
      match ty
        with TyFloat _ then result.ok (print_ (float2string_ output))
      else
        let _r  = nameSym "r" in
        let output = nulet_ _r output in
        switch ty
        case TyRecord r then
          let isFloatTy = lam ty. match ty with TyFloat _ then true else false in
          match record2tuple r.fields with Some tys then
            if forAll isFloatTy tys then
              let inexpr =
                _daeGenPrintWithComma
                  (create (length tys)
                     (lam i. float2string_ (tupleproj_ i (nvar_ _r))))
              in
              ok (bind_ output inexpr)
            else warnWithUnit ty
          else
            if mapAll isFloatTy r.fields then
              let keys = mapKeys r.fields in
              let inexpr =
                _daeGenPrintWithComma
                  (map
                     (lam k.
                       float2string_ (recordproj_ (sidToString k) (nvar_ _r)))
                     keys)
              in
              ok (bind_ output inexpr)
            else warnWithUnit ty
        case ty then warnWithUnit ty
        end
    in
    result.map (nulams_ [_x, _xp]) body

  sem daeGenRuntime : Options -> Map String Name -> DAE -> Res Expr
  sem daeGenRuntime options runtimeNames =| dae ->
    let logDebug = lam msg.
      logMsg logLevel.debug (lam. join ["daeGenRuntime: ", msg ()])
    in
    let state = daeResidualState dae in
    logDebug
      (lam. strJoin "\n"
            ["Residual state:", daeResidualStateToString state.names]);
    let jac =
      daeJacobian
        (roundfi
           (mulf
              options.jacSpecThreshold
              (int2float (succ (length state.names)))))
        dae
    in
    logDebug
      (lam. strJoin "\n" ["Jacobian:", daeJacobianToString jac]);
    let varIds = daeGenVarIdVector state in
    let resf = daeGenResidualFun {
      state = state,
      explicitDiffEqns = dae.explicitDiffEqns,
      eqns = dae.eqns
    } in
    let jacf = daeGenJacFun {
      state = state,
      jac = jac
    } in
    -- let jacf = ulam_ "" never_ in
    let initf = daeGenInitialFun {
      state = state,
      ieqns = dae.ieqns
    } in
    let headerf = daeGenHeaderFun dae.output in
    result.bind (daeGenOutputFun state dae.output)
      (lam outputf.
        result.ok
          (bindall_
             (snoc
                (map (lam b. nulet_ b.0 b.1) dae.bindings)
                (appSeq_ (nvar_ (mapFindExn "daeRuntimeRun" runtimeNames)) [
                  (bool_ options.debugRuntime),
                  (bool_ options.numericJac),
                  varIds,
                  initf,
                  resf,
                  jacf,
                  headerf,
                  outputf
                ]))))

  type DaeGenJacFunArg = {
    state : ResidualState,
    jac : DAEJacobian
  }
  sem daeGenJacFun : DaeGenJacFunArg -> Expr
  sem daeGenJacFun =| arg ->
    let _Xi = _XiExn arg.state.indexMap in
    let _x = nameSym "x" in
    let _xp = nameSym "x'" in
    let _dx = nameSym "δx" in
    let _dxp = nameSym "δx'" in
    let _c = nameSym "c" in
    let _m = nameSym "m" in
    let _f = nameSym "f" in
    let _fs = nameSym "fs" in
    let _j = nameSym "j" in
    let _i = nameSym "i" in
    -- Generate code for partial derivatives of explicit differential equations.
    let explicitDiffPartials =
      mapi
        (lam i. lam eqn.
          match eqn with (name, partials) in
          mapFoldWithKey
            (lam acc. lam name. lam expr. cons (_mmSet _m i (_Xi name) expr) acc)
            [_mmSet _m i (_Xi name) (nvar_ _c)]
            partials)
        arg.jac.explicitDiffPartials
    in
    let iOffset = length arg.jac.explicitDiffPartials in
    -- Generate code for computing partials via vanilla AD.
    let calcAdPartials =
      if setIsEmpty (setOfKeys arg.jac.adPartials) then
        -- No partials via vanilla AD requested.
        [unit_]
      else
        let tangentNames =
          let f = lam name. mapFindExn name arg.jac.nameTangentNameMap in
          map (theseBiMap f f) arg.state.names
        in
        let tangentState = {
          names = tangentNames,
          indexMap = daeResidualStateIndexMap tangentNames
        } in
        let body =
          nulam_ _i
            (bindall_ [
              nulet_ _fs
                (bind_
                   (nulet_ _i (addi_ (nvar_ _i) (int_ iOffset)))
                   (seq_
                      (map
                         (lam expr.
                           ulam_ ""
                             (_mmSetij _m _i _j
                                (addf_ (_mmGetij _m _i _j) expr)))
                         arg.jac.adResidual))),
              app_ (get_ (nvar_ _fs) (nvar_ _i)) unit_
            ])
        in
        let calc = lam v. lam tangent. lam idxs.
          if null idxs then unit_
          else
            bindall_
              (map
                 (lam idx.
                   match idx with (j, is) in
                   bindall_
                     (map (ulet_ "") [
                       _vvSet v j tangent,
                       bind_
                         (nulet_ _f (appf1_ (nvar_ _f) (int_ j)))
                         (iter_
                            (nvar_ _f)
                            (seq_ (map (lam i. int_ i) is))),
                       _vvSet v j (float_ 0.)
                     ]))
                 idxs)
        in
        let idxs =
          map
            (lam b.
              let f = lam j. (j, b.1) in
              theseBiMap f f (mapFindExn b.0 arg.state.indexMap))
            (mapBindings arg.jac.adPartials)
        in
        [
          nulet_ _f
            (nulam_ _j
               (bind_
                  (_daeGenResidualStateBindings _dx _dxp tangentState)
                  body)),
          calc _dx (float_ 1.) (theseCatHere idxs),
          calc _dxp (nvar_ _c) (theseCatThat idxs)
        ]
    in
    -- Generate specialized code for computing partial derivatives.
    let specPartials =
      mapi
        (lam i. lam partials.
          let i = addi i iOffset in
          mapValues
            (mapIntersectWith
               (lam expr. lam idx.
                 theseThese
                   (lam j. _mmSet _m i j expr)
                   (lam j.
                     _mmSet _m i j
                       (addf_ (_mmGet _m i j) (mulf_ (nvar_ _c) expr)))
                   (lam j. lam. _mmSet _m i j expr)
                   idx)
               partials
               arg.state.indexMap))
        arg.jac.specPartials
    in
    let inexpr =
      bindall_
        (join [
          map (uncurry nulet_) arg.jac.bindings,
          map
            (ulet_ "")
            (join (concat explicitDiffPartials specPartials)),
          calcAdPartials,
          [unit_]
        ])
    in
    nulams_ [_x, _dx, _c, _xp, _dxp, _m]
      (bind_ (_daeGenResidualStateBindings _x _xp arg.state) inexpr)

end

lang TestLang =
  DAEParseAnalysis + DAEExpr + DAEStructuralAnalysis + DAEPrettyPrint +
  DAEParseDesugar + DAEGen
end

mexpr

use TestLang in

let _parse = lam prog.
  let prog = daeParseExn "internal" prog in
  logMsg logLevel.debug
    (lam. strJoin "\n" ["Input program:", daeProgToString prog]);
  let e = typeCheck (adSymbolize (daeDesugarProg prog)) in
  consumeWarnErrsExn (daeExprToDAE e)
in

let _transform = lam dae.
  let dae =
    consumeWarnErrsExn
      (result.bind (daeStructuralAnalysis defaultOptions dae)
         (lam analysis.
           let dae = transIndexReduceNaive dae analysis in
           result.ok dae))
  in
  let dae = transElimAliases (daePEval dae) in
  dae
in

-------------------
-- Test Pendulum --
-------------------

logSetLogLevel logLevel.debug;

let dae = _parse "
  let mul = lam x. lam y. x*y end
  let pow2 = lam x. mul x x end
  variables
  x, vx, y, vy, h : Float
  init
  x = 1.;
  y = 0.;
  h = 0.
  equations
  vx = x';
  vy = y';
  vx' = mul x h;
  vy' = mul y h - 1.;
  pow2 x + pow2 y = pow2 1.
  output
  {x, y}
  "
in

let dae = _transform dae in

logMsg logLevel.debug
  (lam. strJoin "\n" ["DAE:", daeToString dae]);

let state = daeResidualState dae in
logMsg logLevel.debug
  (lam. strJoin "\n" ["Residual state:", daeResidualStateToString state.names]);

let resf = daeGenResidualFun {
  state = state,
  explicitDiffEqns = dae.explicitDiffEqns,
  eqns = dae.eqns
} in

logMsg logLevel.debug
  (lam. strJoin "\n" ["Residual function:", expr2str resf]);

let jac = daeJacobian 3 dae in

logMsg logLevel.debug
  (lam. strJoin "\n" ["Jacobian:", daeJacobianToString jac]);

let jacf = daeGenJacFun {
  state = state,
  jac = jac
} in

logMsg logLevel.debug
  (lam. strJoin "\n" ["Jacobian function:", expr2str jacf]);

()
