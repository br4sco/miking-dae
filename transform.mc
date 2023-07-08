include "tuple.mc"
include "log.mc"
include "peval/peval.mc"

include "mexpr/constant-fold.mc"
include "mexpr/free-vars.mc"
include "mexpr/utils.mc"

include "../miking-pead/ad.mc"

include "./dae-arg.mc"
include "./ast_gen.mc"
include "./ast.mc"
include "./pprint.mc"
include "./parse.mc"
include "./desugar.mc"
include "./daecore-structure.mc"
include "./error-print.mc"
include "./dae.mc"

lang DAEStructuralAnalysis =
  DAEAst + DAEExpr +
  MExprSubstitute + MExprFreeVars + MExprPEval + MExprConstantFold + AD


  type DAEAnalysis = {
    varOffset : [(Name, Int)],
    eqnsOffset : [Int]
  }

  sem daeStructuralAnalysis : Options -> DAE -> Res DAEAnalysis
  sem daeStructuralAnalysis options =
  | dae ->
  (if options.debugCompilation then logSetLogLevel logLevel.debug else ());
    let logDebug =
      lam msg. logMsg logLevel.debug (lam. join ["daeStructuralAnalysis: ", msg ()])
    in
    -- Enumerate dependent variables
    let varEnum = mapKeys dae.vars in
    let varEnumMap =
      mapFromSeq nameCmp (mapi (lam i. lam name. (name, i)) varEnum)
    in
    let vm = {
      depVarNameMap = dae.depVarNameMap,
      nameDepVarMap = dae.nameDepVarMap
    } in
    -- Find equation structure
    let eqnsStructure =
      map
        (lam vars. {
          variables =
          map
            (lam x.
              match x with (name, ord) in
              (mapFindExn name varEnumMap, ord))
            vars,
          inputs = []
        })
        (map setToSeq (daeDepVarsEqns dae))
    in
    -- Perform structural analysis
    result.bind (structuralAnalysis eqnsStructure)
      (lam structure.

        logDebug
          (lam. strJoin "\n" [
            "d enumaration:",
            seqToString nameGetStr varEnum
          ]);

        -- logDebug
        --   (lam. strJoin "\n" [
        --     "Sigma Matrix:",
        --     matToString int2string structure.sigma
        --   ]);

        logDebug
          (lam. strJoin "\n" [
            "Variable offset vector:",
            vecToString int2string structure.d
          ]);

        logDebug
          (lam. strJoin "\n" [
            "Equation offset vector:",
            vecToString int2string structure.c
          ]);

        result.ok {
          varOffset = zipWith
                        (lam name. lam d. (name, d)) varEnum (vecToSeq structure.d),
          eqnsOffset = vecToSeq structure.c
        })

  sem _newDepVarName : Int -> Name -> Name
  sem _newDepVarName n =| name ->
    nameSym (concat (nameGetStr name) (create n (lam. '\'')))

  sem transOrderReduce : [(Name, Int)] -> DAE -> DAE
  sem transOrderReduce varOffset =
  | dae ->
    let dae = foldl
                (lam dae. lam ofs.
                  match ofs with (name, d) in
                  foldl
                    (lam dae. lam i.
                      if mapMem (name, i) dae.depVarNameMap then dae
                      else
                        let newname = _newDepVarName i name in
                        {
                          dae with
                          depVarNameMap =
                          mapInsert (name, i) newname dae.depVarNameMap,
                          nameDepVarMap =
                          mapInsert newname (name, i) dae.nameDepVarMap
                        })
                    dae
                    (create d succ))
                dae
                varOffset
    in
    let explicitDiffEqns =
      foldl
        (lam eqns. lam v.
          match v with (name, d) in
          let name = lam i. mapFindExn (name, i) dae.depVarNameMap in
          concat eqns
            (create (pred d)
               (lam i. (name i, nvar_ (name (succ i))))))
        []
        (filter (lam v. and (neqi v.1 0) (neqi v.1 1)) varOffset)
    in
    { dae with explicitDiffEqns = explicitDiffEqns }

  sem transIndexReduceNaive : DAE -> DAEAnalysis -> DAE
  sem transIndexReduceNaive dae =| analysis ->
    let b = peadAstBuilder (NoInfo ()) in
    let dae = transOrderReduce analysis.varOffset dae in
    -- Max derivative order
    let cmax = maxOrElse (lam. error "impossible") subi analysis.eqnsOffset in
    -- Generate new names for differentiated bindings
    let bindingNames = (unzip dae.bindings).0 in
    let bnms =
      create (succ cmax)
        (lam c.
          if eqi c 0 then
            mapFromSeq nameCmp (map (lam name. (name, name)) bindingNames)
          else
            mapFromSeq
              nameCmp
              (map
                 (lam name. (name, _newDepVarName c name))
                 bindingNames))
    in
    -- We associate derivatives of free variables with the name of their
    -- derivative.
    let adCtxs : [ADCtx] =
      map
        (lam bnm.
          mapFoldWithKey
            (lam ctx. lam name1. lam name2. adCtxEnvInsert name1 (nvar_ name2) ctx)
            (adCtxEmpty ())
            bnm)
        bnms
    in
    -- AD transform the bindings.
    let bindings =
      join
        (mapi
           (lam c. lam bnm.
             map
               (lam b.
                 match b with (name, expr) in
                 (mapFindExn name bnm, substituteIdentifiers bnm (ad c expr)))
               dae.bindings)
           bnms)
    in
    -- Extend the AD context with AD transformations of dependent variables.
    let adCtxs =
      mapi
        (lam c. lam ctx.
          mapFoldWithKey
            (lam ctx. lam name. lam.
              switch daeAD dae c name
              case Some [_] then ctx
              case Some names then
                let expr = b.taylorcoef (map nvar_ names) in
                adCtxEnvInsert name expr ctx
              case None _ then ctx
              end)
            ctx
            dae.nameDepVarMap)
        adCtxs
    in
    -- AD transform equations according to the equation offset vector
    let eqns =
      zipWith
        (lam c. lam eqn.
          if eqi c 0 then eqn
          else
            let adExpr = adExpr (get adCtxs c) c in
            let f = lam expr. b.taylorcoefproj (adExpr expr) c in
            match eqn with (lhs, rhs) in
            (f lhs, f rhs))
        analysis.eqnsOffset
        dae.eqns
    in
    let dae = {
      dae with
      vars = dae.vars,
      bindings = bindings,
      eqns = eqns
    } in
    -- Remove unused bindings
    daeElimDeadCode dae

  sem transElimAliases : DAE -> DAE
  sem transElimAliases =| dae ->
    let daeDer = daeDer dae in
    let daeAntiDer = daeAntiDer dae in
    let substitute = lam aliases. lam name1. lam name2.
      optionMapOrElse
        (lam. mapInsert name1 name2 aliases)
        (lam name2. mapInsert name1 name2 aliases)
        (mapLookup name2 aliases)
    in
    match
      foldl
        (lam acc. lam eqn.
          match acc with (eqns, aliases) in
          switch eqn
          case (TmVar {ident = name1}, TmVar {ident = name2}) then
            switch (daeDer name1, daeDer name2)
            case (None _, None _) then
              recursive let recur = lam aliases. lam x.
                match (daeAntiDer x.0, daeAntiDer x.1) with
                  (Some name1, Some name2)
                then
                  recur (substitute aliases name1 name2) (name1, name2)
                else aliases
              in
              let aliases = substitute aliases name1 name2 in
              (eqns, recur aliases (name1, name2))
            case _ then (snoc eqns eqn, aliases)
            end
          case _ then (snoc eqns eqn, aliases)
          end)
        ([], mapEmpty nameCmp)
        dae.eqns
      with (eqns, aliases)
    in
    let substituteIdentifiers = substituteIdentifiers aliases in
    let ieqns =
      map
        (lam eqn.
          match eqn with (name, rhs) in
          (optionGetOr name (mapLookup name aliases),
           substituteIdentifiers rhs))
        dae.ieqns
    in
    let explicitDiffEqns =
      map
        (lam eqn.
          match eqn with (name, rhs) in
          (optionGetOr name (mapLookup name aliases),
           substituteIdentifiers rhs))
        dae.explicitDiffEqns
    in
    let eqns =
      map
        (lam eqn.
          match eqn with (lhs, rhs) in
          (substituteIdentifiers lhs, substituteIdentifiers rhs))
        eqns
    in
    let output = substituteIdentifiers dae.output in
    let dae = {
      dae with
      ieqns = ieqns,
      explicitDiffEqns = explicitDiffEqns,
      eqns = eqns,
      output = output
    } in
    daeUpdateVarMap dae
end

lang TestLang =
  DAEParseAnalysis + DAEExpr + DAEStructuralAnalysis + DAEPrettyPrint +
  DAEParseDesugar
end

mexpr

use TestLang in

let _parseFile = lam filename.
  daeParseExn filename (readFile filename)
in

let _daeProgToDAE = lam prog.
  consumeWarnErrsExn
    (daeExprToDAE (typeCheck (adSymbolize (daeDesugarProg prog))))
in

let _defnNameToString = lam defn.
  (nameGetStr defn.0, sort cmpString (map nameGetStr defn.1))
in
let _daeStructureNameToString = lam dae.
  let eqnNameToString = lam eqn.
    (sort cmpString (map nameGetStr eqn.0),
     sort cmpString (map nameGetStr eqn.1))
  in
  {
    vars = sort cmpString (map nameGetStr dae.vars),
    bindings = map _defnNameToString dae.bindings,
    explicitDiffEqns = map _defnNameToString dae.explicitDiffEqns,
    ieqns = map _defnNameToString dae.ieqns,
    eqns = map eqnNameToString dae.eqns,
    output = map nameGetStr dae.output
  }
in

let _daeStructure = lam dae. _daeStructureNameToString (daeStructure dae) in

let _jacStructureNameToString = lam jac.
  let explicitDiffPartials =
    map
      (lam p. (nameGetStr p.0, map _defnNameToString p.1))
      jac.explicitDiffPartials
  in
  let specPartials =
    map
      (lam p. sort (lam x. lam y. cmpString x.0 y.0 ) (map _defnNameToString p))
      jac.specPartials
  in
  {
    bindings = map _defnNameToString jac.bindings,
    explicitDiffPartials = explicitDiffPartials,
    adResidual = map (map nameGetStr) jac.adResidual,
    specPartials = specPartials
  }
in

let _jacStructure =
  lam jac. _jacStructureNameToString (daeJacobianStructure jac)
in

let _analysis = lam dae.
  consumeWarnErrsExn
    (daeStructuralAnalysis defaultOptions dae)
in

let _indexReduce = lam analysis. lam dae.
  let dae = daePEval dae in
  daePEval (transIndexReduceNaive dae analysis)
in

-------------------------
-- Harmonic Oscillator --
-------------------------

utest
  let prog = _parseFile "examples/harmonic.dae" in

  logSetLogLevel logLevel.error;

  logMsg logLevel.debug
    (lam. strJoin "\n" ["Input program:", daeProgToString prog]);

  logSetLogLevel logLevel.error;

  let dae = _daeProgToDAE prog in

  -- Structure of input DAE
  let _s = _daeStructure dae in
  utest _s.vars with ["x", "vx"] in
  utest _s.bindings with [] in
  utest _s.explicitDiffEqns with [] in
  utest _s.eqns with [
    (["x'"], ["vx"]),
    (["x", "vx", "vx'"], [])
  ] in

  -- Structure of index-reduced DAE
  let dae = daePEval dae in
  let analysis = _analysis dae in
  utest analysis.eqnsOffset with [0, 0] in
  let dae = _indexReduce analysis dae in
  let _s = _daeStructure dae in
  utest _s.vars with ["x", "vx"] in
  utest _s.bindings with [] in
  utest _s.explicitDiffEqns with [] in
  utest _s.eqns with [
    (["x'"], ["vx"]),
    (["x", "vx", "vx'"], [])
  ] in

  -- Specialized partial derivatives of the Jacobian
  let _s = _jacStructure (daeJacobian 3 dae) in
  utest _s.bindings with [] in
  utest _s.explicitDiffPartials with [] in
  utest _s.adResidual with [] in
  utest _s.specPartials with [
    [
      ("vx", []),
      ("x'", [])
    ],
    [
      ("x", []),
      ("vx", []),
      ("vx'", [])
    ]
  ] in

  () with ()
in

---------------------
-- Planar Pendulum --
---------------------

utest
  let prog = _parseFile "examples/pendulum.dae" in

  logSetLogLevel logLevel.error;
  logMsg logLevel.debug
    (lam. strJoin "\n" ["Input program:", daeProgToString prog]);
  logSetLogLevel logLevel.error;

  let dae = _daeProgToDAE prog in

  -- Structure of input DAE
  let _s = _daeStructure dae in
  utest _s.vars with ["h", "x", "y", "vx", "vy"] in
  utest _s.bindings with [
    ("mul", []),
    ("pow2", ["mul"])
  ] in
  utest _s.explicitDiffEqns with [] in
  utest _s.eqns with [
    (["vx"], ["x'"]),
    (["vy"], ["y'"]),
    (["vx'"], ["h", "x", "mul"]),
    (["vy'"], ["h", "y", "mul"]),
    (["x", "y", "pow2"], ["pow2"])
  ] in

  -- Structure of index-reduced DAE
  let dae = daePEval dae in
  let analysis = _analysis dae in
  utest analysis.eqnsOffset with [1, 1, 0, 0, 2] in
  let dae = _indexReduce analysis dae in
  logSetLogLevel logLevel.error;
  logMsg logLevel.debug
    (lam. strJoin "\n" ["Index-reduced DAE:", daeToString dae]);
  logSetLogLevel logLevel.error;
  let _s = _daeStructure dae in
  utest _s.vars with ["h", "x", "y", "vx", "vy"] in
  utest _s.bindings with [] in
  utest _s.explicitDiffEqns with [
    ("x", ["x'"]),
    ("y", ["y'"])
  ] in
  utest _s.eqns with [
    (["vx'"], ["x''"]),                        -- f'
    (["vy'"], ["y''"]),                        -- f'
    (["vx'"], ["h", "x"]),                     -- f
    (["vy'"], ["h", "y"]),                     -- f
    (["x", "y", "x'", "y'", "x''", "y''"], []) -- f''
  ] in

  -- Specialized partial derivatives of the Jacobian
  let _s = _jacStructure (daeJacobian 3 dae) in
  utest _s.bindings with [] in
  utest _s.explicitDiffPartials with [
    ("x", [("x'", [])]),
    ("y", [("y'", [])])
  ] in
  utest _s.adResidual with [] in
  utest _s.specPartials with [
    [
      ("vx'", []),
      ("x''", [])
    ],
    [
      ("vy'", []),
      ("y''", [])
    ],
    [
      ("h", ["x"]),
      ("x", ["h"]),
      ("vx'", [])
    ],
    [
      ("h", ["y"]),
      ("y", ["h"]),
      ("vy'", [])
    ],
    [
      ("x", ["x''"]),
      ("y", ["y''"]),
      ("x'", ["x'"]),
      ("y'", ["y'"]),
      ("x''", ["x"]),
      ("y''", ["y"])
    ]
  ] in

  () with ()
in

-----------------
-- 3D-Pendulum --
-----------------

utest
  let prog = _parseFile "examples/pendulum-3d.dae" in

  logSetLogLevel logLevel.error;
  logMsg logLevel.debug
    (lam. strJoin "\n" ["Input program:", daeProgToString prog]);
  logSetLogLevel logLevel.error;

  let dae = _daeProgToDAE prog in

  -- Structure of input DAE
  let _s = _daeStructure dae in
  utest _s.vars with [
    "_x_1",
    "_y_1",
    "_z_1",
    "_Fm1_1",
    "_Fm2_1",
    "_Fm3_1",
    "_Tm1_1",
    "_Tm2_1",
    "_Tm3_1",
    "_qm1_1",
    "_qm2_1",
    "_qm3_1",
    "_qm4_1",
    "_vm1_1",
    "_vm2_1",
    "_vm3_1",
    "_Fr11_1",
    "_Fr12_1",
    "_Fr13_1",
    "_Fta1_1",
    "_Fta2_1",
    "_Fta3_1",
    "_Tr11_1",
    "_Tr12_1",
    "_Tr13_1",
    "_Tr21_1",
    "_Tr22_1",
    "_Tr23_1",
    "_omm1_1",
    "_omm2_1",
    "_omm3_1",
    "_qra1_1",
    "_qra2_1",
    "_qra3_1",
    "_qra4_1",
    "_xfa1_1",
    "_xfa2_1",
    "_xfa3_1",
    "_omr21_1",
    "_omr22_1",
    "_omr23_1"
  ] in
  utest _s.bindings with [] in
  utest _s.explicitDiffEqns with [] in
  utest get _s.eqns 0 with
    (["_qra1_1'"],
     ["_qra2_1", "_qra3_1", "_qra4_1", "_omr21_1", "_omr22_1", "_omr23_1"])
  in
  utest get _s.eqns 1 with
    (["_qra2_1'"],
     ["_qra1_1", "_qra3_1", "_qra4_1", "_omr21_1", "_omr22_1", "_omr23_1"])
  in
  utest get _s.eqns 2 with
    (["_qra3_1'"],
     ["_qra1_1", "_qra2_1", "_qra4_1", "_omr21_1", "_omr22_1", "_omr23_1"])
  in
  utest get _s.eqns 3 with
    (["_qra4_1'"],
     ["_qra1_1", "_qra2_1", "_qra3_1", "_omr21_1", "_omr22_1", "_omr23_1"])
  in
  utest get _s.eqns 4 with
    (["_Tr21_1"],
     ["_Fr12_1", "_Fr13_1", "_qra1_1", "_qra2_1", "_qra3_1", "_qra4_1"])
  in
  utest get _s.eqns 5 with
    (["_Tr22_1"],
     ["_Fr11_1", "_Fr13_1", "_qra1_1", "_qra2_1", "_qra3_1", "_qra4_1"])
  in
  utest get _s.eqns 6 with
    (["_Tr23_1"],
     ["_Fr11_1", "_Fr12_1", "_qra1_1", "_qra2_1", "_qra3_1", "_qra4_1"])
  in
  utest get _s.eqns 7 with (["_x_1'"], ["_vm1_1"]) in
  utest get _s.eqns 8 with (["_y_1'"], ["_vm2_1"]) in
  utest get _s.eqns 9 with (["_z_1'"], ["_vm3_1"]) in
  utest get _s.eqns 10 with (["_Fm1_1"], ["_vm1_1'"]) in
  utest get _s.eqns 11 with (["_Fm2_1"], ["_vm2_1'"]) in
  utest get _s.eqns 12 with (["_Fm3_1"], ["_vm3_1'"]) in
  utest get _s.eqns 13 with
    (["_qm1_1'"],
     ["_qm2_1", "_qm3_1", "_qm4_1", "_omm1_1", "_omm2_1", "_omm3_1"])
  in
  utest get _s.eqns 14 with
    (["_qm2_1'"],
     ["_qm1_1", "_qm3_1", "_qm4_1", "_omm1_1", "_omm2_1", "_omm3_1"])
  in
  utest get _s.eqns 15 with
    (["_qm3_1'"],
     ["_qm1_1", "_qm2_1", "_qm4_1", "_omm1_1", "_omm2_1", "_omm3_1"])
  in
  utest get _s.eqns 16 with
    (["_qm4_1'"],
     ["_qm1_1", "_qm2_1", "_qm3_1", "_omm1_1", "_omm2_1", "_omm3_1"])
  in
  utest get _s.eqns 17 with
    (["_Tm1_1"],
     [
       "_qm1_1",
       "_qm2_1",
       "_qm3_1",
       "_qm4_1",
       "_omm1_1",
       "_omm2_1",
       "_omm3_1",
       "_omm1_1'",
       "_omm2_1'",
       "_omm3_1'"
     ])
  in
  utest get _s.eqns 18 with
    (["_Tm2_1"],
     [
       "_qm1_1",
       "_qm2_1",
       "_qm3_1",
       "_qm4_1",
       "_omm1_1",
       "_omm2_1",
       "_omm3_1",
       "_omm1_1'",
       "_omm2_1'",
       "_omm3_1'"
     ])
  in
  utest get _s.eqns 19 with
    (["_Tm3_1"],
     [
       "_qm1_1",
       "_qm2_1",
       "_qm3_1",
       "_qm4_1",
       "_omm1_1",
       "_omm2_1",
       "_omm3_1",
       "_omm1_1'",
       "_omm2_1'",
       "_omm3_1'"
     ])
  in
  utest get _s.eqns 20 with (["_x_1"], ["_xfa1_1"]) in
  utest get _s.eqns 21 with (["_y_1"], ["_xfa2_1"]) in
  utest get _s.eqns 22 with (["_z_1"], ["_xfa3_1"]) in
  utest get _s.eqns 23 with
    (["_qra1_1", "_qra2_1", "_qra3_1", "_qra4_1"], ["_xfa1_1"])
  in
  utest get _s.eqns 24 with
    (["_qra1_1", "_qra2_1", "_qra3_1", "_qra4_1"], ["_xfa2_1"])
  in
  utest get _s.eqns 25 with
    (["_qra1_1", "_qra2_1", "_qra3_1", "_qra4_1"], ["_xfa3_1"])
  in
  utest get _s.eqns 26 with (["_Fta1_1"], ["_Fr11_1"]) in
  utest get _s.eqns 27 with (["_Fta2_1"], ["_Fr12_1"]) in
  utest get _s.eqns 28 with (["_Fta3_1"], ["_Fr13_1"]) in
  utest get _s.eqns 29 with ([], ["_Fm1_1", "_Fr11_1"]) in
  utest get _s.eqns 30 with ([], ["_Fm2_1", "_Fr12_1"]) in
  utest get _s.eqns 31 with ([], ["_Fm3_1", "_Fr13_1"]) in
  utest get _s.eqns 32 with (["_omr21_1"], ["_omm1_1"]) in
  utest get _s.eqns 33 with (["_omr22_1"], ["_omm2_1"]) in
  utest get _s.eqns 34 with (["_omr23_1"], ["_omm3_1"]) in
  utest get _s.eqns 35 with (["_Tm1_1"], ["_Tr21_1"]) in
  utest get _s.eqns 36 with (["_Tm2_1"], ["_Tr22_1"]) in
  utest get _s.eqns 37 with (["_Tm3_1"], ["_Tr23_1"]) in
  utest get _s.eqns 38 with (["_Tr11_1"], []) in
  utest get _s.eqns 39 with (["_Tr12_1"], []) in
  utest get _s.eqns 40 with (["_Tr13_1"], []) in

  -- Structure of index-reduced DAE
  let dae = daePEval dae in
  let analysis = _analysis dae in
  utest analysis.eqnsOffset with
    [1,1,1,1,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,2,2,2,2,2,2,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0]
  in
  let dae = _indexReduce analysis dae in
  logSetLogLevel logLevel.error;
  logMsg logLevel.debug
    (lam. strJoin "\n" ["Index-reduced DAE:", daeToString dae]);
  logSetLogLevel logLevel.error;
  let _s = _daeStructure dae in
  utest _s.vars with [
    "_x_1",
    "_y_1",
    "_z_1",
    "_Fm1_1",
    "_Fm2_1",
    "_Fm3_1",
    "_Tm1_1",
    "_Tm2_1",
    "_Tm3_1",
    "_qm1_1",
    "_qm2_1",
    "_qm3_1",
    "_qm4_1",
    "_vm1_1",
    "_vm2_1",
    "_vm3_1",
    "_Fr11_1",
    "_Fr12_1",
    "_Fr13_1",
    "_Fta1_1",
    "_Fta2_1",
    "_Fta3_1",
    "_Tr11_1",
    "_Tr12_1",
    "_Tr13_1",
    "_Tr21_1",
    "_Tr22_1",
    "_Tr23_1",
    "_omm1_1",
    "_omm2_1",
    "_omm3_1",
    "_qra1_1",
    "_qra2_1",
    "_qra3_1",
    "_qra4_1",
    "_xfa1_1",
    "_xfa2_1",
    "_xfa3_1",
    "_omr21_1",
    "_omr22_1",
    "_omr23_1"
  ] in
  utest _s.bindings with [] in
  utest _s.explicitDiffEqns with [
    ("_xfa3_1", ["_xfa3_1'"]),
    ("_xfa2_1", ["_xfa2_1'"]),
    ("_xfa1_1", ["_xfa1_1'"]),
    ("_qra4_1", ["_qra4_1'"]),
    ("_qra3_1", ["_qra3_1'"]),
    ("_qra2_1", ["_qra2_1'"]),
    ("_qra1_1", ["_qra1_1'"]),
    ("_z_1", ["_z_1'"]),
    ("_y_1", ["_y_1'"]),
    ("_x_1", ["_x_1'"])
  ] in
  utest get _s.eqns 0 with        -- f'
    (["_qra1_1''"],
     [
       "_qra2_1",
       "_qra3_1",
       "_qra4_1",
       "_omr21_1",
       "_omr22_1",
       "_omr23_1",
       "_qra2_1'",
       "_qra3_1'",
       "_qra4_1'",
       "_omr21_1'",
       "_omr22_1'",
       "_omr23_1'"
     ])
  in
  utest get _s.eqns 1 with        -- f'
    (["_qra2_1''"],
     [
       "_qra1_1",
       "_qra3_1",
       "_qra4_1",
       "_omr21_1",
       "_omr22_1",
       "_omr23_1",
       "_qra1_1'",
       "_qra3_1'",
       "_qra4_1'",
       "_omr21_1'",
       "_omr22_1'",
       "_omr23_1'"
     ])
  in
  utest get _s.eqns 2 with        -- f'
    (["_qra3_1''"],
     [
       "_qra1_1",
       "_qra2_1",
       "_qra4_1",
       "_omr21_1",
       "_omr22_1",
       "_omr23_1",
       "_qra1_1'",
       "_qra2_1'",
       "_qra4_1'",
       "_omr21_1'",
       "_omr22_1'",
       "_omr23_1'"
     ])
  in
  utest get _s.eqns 3 with        -- f'
    (["_qra4_1''"],
     [
       "_qra1_1",
       "_qra2_1",
       "_qra3_1",
       "_omr21_1",
       "_omr22_1",
       "_omr23_1",
       "_qra1_1'",
       "_qra2_1'",
       "_qra3_1'",
       "_omr21_1'",
       "_omr22_1'",
       "_omr23_1'"
     ])
  in
  utest get _s.eqns 4 with        -- f
    (["_Tr21_1"],
     ["_Fr12_1", "_Fr13_1", "_qra1_1", "_qra2_1", "_qra3_1", "_qra4_1"])
  in
  utest get _s.eqns 5 with        -- f
    (["_Tr22_1"],
     ["_Fr11_1", "_Fr13_1", "_qra1_1", "_qra2_1", "_qra3_1", "_qra4_1"])
  in
  utest get _s.eqns 6 with        -- f
    (["_Tr23_1"],
     ["_Fr11_1", "_Fr12_1", "_qra1_1", "_qra2_1", "_qra3_1", "_qra4_1"])
  in
  utest get _s.eqns 7 with (["_x_1''"], ["_vm1_1'"]) in  -- f'
  utest get _s.eqns 8 with (["_y_1''"], ["_vm2_1'"]) in  -- f'
  utest get _s.eqns 9 with (["_z_1''"], ["_vm3_1'"]) in  -- f'
  utest get _s.eqns 10 with (["_Fm1_1"], ["_vm1_1'"]) in -- f
  utest get _s.eqns 11 with (["_Fm2_1"], ["_vm2_1'"]) in -- f
  utest get _s.eqns 12 with (["_Fm3_1"], ["_vm3_1'"]) in -- f
  utest get _s.eqns 13 with       -- f
    (["_qm1_1'"],
     ["_qm2_1", "_qm3_1", "_qm4_1", "_omm1_1", "_omm2_1", "_omm3_1"])
  in
  utest get _s.eqns 14 with       -- f
    (["_qm2_1'"],
     ["_qm1_1", "_qm3_1", "_qm4_1", "_omm1_1", "_omm2_1", "_omm3_1"])
  in
  utest get _s.eqns 15 with       -- f
    (["_qm3_1'"],
     ["_qm1_1", "_qm2_1", "_qm4_1", "_omm1_1", "_omm2_1", "_omm3_1"])
  in
  utest get _s.eqns 16 with       -- f
    (["_qm4_1'"],
     ["_qm1_1", "_qm2_1", "_qm3_1", "_omm1_1", "_omm2_1", "_omm3_1"])
  in
  utest get _s.eqns 17 with       -- f
    (["_Tm1_1"],
     [
       "_qm1_1",
       "_qm2_1",
       "_qm3_1",
       "_qm4_1",
       "_omm1_1",
       "_omm2_1",
       "_omm3_1",
       "_omm1_1'",
       "_omm2_1'",
       "_omm3_1'"
     ])
  in
  utest get _s.eqns 18 with       -- f
    (["_Tm2_1"],
     [
       "_qm1_1",
       "_qm2_1",
       "_qm3_1",
       "_qm4_1",
       "_omm1_1",
       "_omm2_1",
       "_omm3_1",
       "_omm1_1'",
       "_omm2_1'",
       "_omm3_1'"
     ])
  in
  utest get _s.eqns 19 with       -- f
    (["_Tm3_1"],
     [
       "_qm1_1",
       "_qm2_1",
       "_qm3_1",
       "_qm4_1",
       "_omm1_1",
       "_omm2_1",
       "_omm3_1",
       "_omm1_1'",
       "_omm2_1'",
       "_omm3_1'"
     ])
  in
  utest get _s.eqns 20 with (["_x_1''"], ["_xfa1_1''"]) in -- f''
  utest get _s.eqns 21 with (["_y_1''"], ["_xfa2_1''"]) in -- f''
  utest get _s.eqns 22 with (["_z_1''"], ["_xfa3_1''"]) in -- f''
  utest get _s.eqns 23 with                                -- f''
    ([
      "_qra1_1",
      "_qra2_1",
      "_qra3_1",
      "_qra4_1",
      "_qra1_1'",
      "_qra2_1'",
      "_qra3_1'",
      "_qra4_1'",
      "_qra1_1''",
      "_qra2_1''",
      "_qra3_1''",
      "_qra4_1''"
    ],
     ["_xfa1_1''"])
  in
  utest get _s.eqns 24 with                                -- f''
    ([
      "_qra1_1",
      "_qra2_1",
      "_qra3_1",
      "_qra4_1",
      "_qra1_1'",
      "_qra2_1'",
      "_qra3_1'",
      "_qra4_1'",
      "_qra1_1''",
      "_qra2_1''",
      "_qra3_1''",
      "_qra4_1''"
    ],
     ["_xfa2_1''"])
  in
  utest get _s.eqns 25 with                                -- f''
    ([
      "_qra1_1",
      "_qra2_1",
      "_qra3_1",
      "_qra4_1",
      "_qra1_1'",
      "_qra2_1'",
      "_qra3_1'",
      "_qra4_1'",
      "_qra1_1''",
      "_qra2_1''",
      "_qra3_1''",
      "_qra4_1''"
    ],
     ["_xfa3_1''"])
  in
  utest get _s.eqns 26 with (["_Fta1_1"], ["_Fr11_1"]) in    -- f
  utest get _s.eqns 27 with (["_Fta2_1"], ["_Fr12_1"]) in    -- f
  utest get _s.eqns 28 with (["_Fta3_1"], ["_Fr13_1"]) in    -- f
  utest get _s.eqns 29 with ([], ["_Fm1_1", "_Fr11_1"]) in   -- f
  utest get _s.eqns 30 with ([], ["_Fm2_1", "_Fr12_1"]) in   -- f
  utest get _s.eqns 31 with ([], ["_Fm3_1", "_Fr13_1"]) in   -- f
  utest get _s.eqns 32 with (["_omr21_1'"], ["_omm1_1'"]) in -- f'
  utest get _s.eqns 33 with (["_omr22_1'"], ["_omm2_1'"]) in -- f'
  utest get _s.eqns 34 with (["_omr23_1'"], ["_omm3_1'"]) in -- f'
  utest get _s.eqns 35 with (["_Tm1_1"], ["_Tr21_1"]) in     -- f
  utest get _s.eqns 36 with (["_Tm2_1"], ["_Tr22_1"]) in     -- f
  utest get _s.eqns 37 with (["_Tm3_1"], ["_Tr23_1"]) in     -- f
  utest get _s.eqns 38 with (["_Tr11_1"], []) in             -- f
  utest get _s.eqns 39 with (["_Tr12_1"], []) in             -- f
  utest get _s.eqns 40 with (["_Tr13_1"], []) in             -- f

  -- Specialized partial derivatives of the Jacobian
  let _s = _jacStructure (daeJacobian 40 dae) in
  utest _s.bindings with [] in
  utest _s.explicitDiffPartials with [
    ("_xfa3_1", [("_xfa3_1'", [])]),
    ("_xfa2_1", [("_xfa2_1'", [])]),
    ("_xfa1_1", [("_xfa1_1'", [])]),
    ("_qra4_1", [("_qra4_1'", [])]),
    ("_qra3_1", [("_qra3_1'", [])]),
    ("_qra2_1", [("_qra2_1'", [])]),
    ("_qra1_1", [("_qra1_1'", [])]),
    ("_z_1", [("_z_1'", [])]),
    ("_y_1", [("_y_1'", [])]),
    ("_x_1", [("_x_1'", [])])
  ] in
  utest _s.adResidual with [] in

  -- _qra1_1'' = 0.5*-_qra2_1*_omr21_1' + 0.5*-_qra2_1'*_omr21_1 +
  --             0.5*-_qra3_1*_omr22_1' + 0.5*-_qra3_1'*_omr22_1 +
  --             0.5*-_qra4_1*_omr23_1'+0.5*-_qra4_1'*_omr23_1
  utest get _s.specPartials 0 with [
    ("_qra2_1", ["_omr21_1'"]),
    ("_qra3_1", ["_omr22_1'"]),
    ("_qra4_1", ["_omr23_1'"]),
    ("_omr21_1", ["_qra2_1'"]),
    ("_omr22_1", ["_qra3_1'"]),
    ("_omr23_1", ["_qra4_1'"]),
    ("_qra2_1'", ["_omr21_1"]),
    ("_qra3_1'", ["_omr22_1"]),
    ("_qra4_1'", ["_omr23_1"]),
    ("_omr21_1'", ["_qra2_1"]),
    ("_omr22_1'", ["_qra3_1"]),
    ("_omr23_1'", ["_qra4_1"]),
    ("_qra1_1''", [])
  ] in

  -- _qra2_1'' = 0.5*_qra1_1*_omr21_1' + 0.5*_qra1_1'*_omr21_1 +
  --             0.5*_qra4_1*_omr22_1' + 0.5*_qra4_1'*_omr22_1 +
  --             0.5*-_qra3_1*_omr23_1' + 0.5*-_qra3_1'*_omr23_1
  utest get _s.specPartials 1 with [
    ("_qra1_1", ["_omr21_1'"]),
    ("_qra3_1", ["_omr23_1'"]),
    ("_qra4_1", ["_omr22_1'"]),
    ("_omr21_1", ["_qra1_1'"]),
    ("_omr22_1", ["_qra4_1'"]),
    ("_omr23_1", ["_qra3_1'"]),
    ("_qra1_1'", ["_omr21_1"]),
    ("_qra3_1'", ["_omr23_1"]),
    ("_qra4_1'", ["_omr22_1"]),
    ("_omr21_1'", ["_qra1_1"]),
    ("_omr22_1'", ["_qra4_1"]),
    ("_omr23_1'", ["_qra3_1"]),
    ("_qra2_1''", [])
  ] in

  -- _qra3_1'' = 0.5*-_qra4_1*_omr21_1' + 0.5*-_qra4_1'*_omr21_1 +
  --             0.5*_qra1_1*_omr22_1' + 0.5*_qra1_1'*_omr22_1 +
  --             0.5*_qra2_1*_omr23_1' + 0.5*_qra2_1'*_omr23_1
  utest get _s.specPartials 2 with [
    ("_qra1_1", ["_omr22_1'"]),
    ("_qra2_1", ["_omr23_1'"]),
    ("_qra4_1", ["_omr21_1'"]),
    ("_omr21_1", ["_qra4_1'"]),
    ("_omr22_1", ["_qra1_1'"]),
    ("_omr23_1", ["_qra2_1'"]),
    ("_qra1_1'", ["_omr22_1"]),
    ("_qra2_1'", ["_omr23_1"]),
    ("_qra4_1'", ["_omr21_1"]),
    ("_omr21_1'", ["_qra4_1"]),
    ("_omr22_1'", ["_qra1_1"]),
    ("_omr23_1'", ["_qra2_1"]),
    ("_qra3_1''", [])
  ] in

  -- _qra4_1'' = 0.5*_qra3_1*_omr21_1' + 0.5*_qra3_1'*_omr21_1 +
  --             0.5*-_qra2_1*_omr22_1' + 0.5*-_qra2_1'*_omr22_1 +
  --             0.5*_qra1_1*_omr23_1' + 0.5*_qra1_1'*_omr23_1
  utest get _s.specPartials 3 with [
    ("_qra1_1", ["_omr23_1'"]),
    ("_qra2_1", ["_omr22_1'"]),
    ("_qra3_1", ["_omr21_1'"]),
    ("_omr21_1", ["_qra3_1'"]),
    ("_omr22_1", ["_qra2_1'"]),
    ("_omr23_1", ["_qra1_1'"]),
    ("_qra1_1'", ["_omr23_1"]),
    ("_qra2_1'", ["_omr22_1"]),
    ("_qra3_1'", ["_omr21_1"]),
    ("_omr21_1'", ["_qra3_1"]),
    ("_omr22_1'", ["_qra2_1"]),
    ("_omr23_1'", ["_qra1_1"]),
    ("_qra4_1''", [])
  ] in

  -- _Tr21_1 = -2.5*(_qra4_1*_qra2_1 + -_qra3_1*_qra1_1 + _qra2_1*_qra4_1 +
  --                 _qra1_1*-_qra3_1)*_Fr12_1 +
  --           (0.--2.5*(_qra3_1*_qra2_1 +
  --            _qra4_1*_qra1_1+_qra1_1*_qra4_1+_qra2_1*_qra3_1))*_Fr13_1
  utest get _s.specPartials 4 with [
    ("_Fr12_1", ["_qra1_1", "_qra2_1", "_qra3_1", "_qra4_1"]),
    ("_Fr13_1", ["_qra1_1", "_qra2_1", "_qra3_1", "_qra4_1"]),
    ("_Tr21_1", []),
    ("_qra1_1", ["_Fr12_1", "_Fr13_1", "_qra3_1", "_qra4_1"]),
    ("_qra2_1", ["_Fr12_1", "_Fr13_1", "_qra3_1", "_qra4_1"]),
    ("_qra3_1", ["_Fr12_1", "_Fr13_1", "_qra1_1", "_qra2_1"]),
    ("_qra4_1", ["_Fr12_1", "_Fr13_1", "_qra1_1", "_qra2_1"])
  ]
  in

  -- _Tr22_1 = (0.--2.5*(_qra4_1*_qra2_1 + -_qra3_1*_qra1_1 + _qra2_1*_qra4_1 +
  --            _qra1_1*-_qra3_1))*_Fr11_1 +
  --           -2.5*(_qra2_1*_qra2_1 + _qra1_1*_qra1_1 + -_qra4_1*_qra4_1 +
  --                 _qra3_1*-_qra3_1)*_Fr13_1
  utest get _s.specPartials 5 with [
    ("_Fr11_1", ["_qra1_1", "_qra2_1", "_qra3_1", "_qra4_1"]),
    ("_Fr13_1", ["_qra1_1", "_qra2_1", "_qra3_1", "_qra4_1"]),
    ("_Tr22_1", []),
    ("_qra1_1", ["_Fr11_1", "_Fr13_1", "_qra1_1", "_qra3_1"]),
    ("_qra2_1", ["_Fr11_1", "_Fr13_1", "_qra2_1", "_qra4_1"]),
    ("_qra3_1", ["_Fr11_1", "_Fr13_1", "_qra1_1", "_qra3_1"]),
    ("_qra4_1", ["_Fr11_1", "_Fr13_1", "_qra2_1", "_qra4_1"])
  ]
  in

  -- _Tr23_1 = -2.5*(_qra3_1*_qra2_1 + _qra4_1*_qra1_1 + _qra1_1*_qra4_1 +
  --                 _qra2_1*_qra3_1)*_Fr11_1 +
  --           (0.--2.5*(_qra2_1*_qra2_1 + _qra1_1*_qra1_1 + -_qra4_1*_qra4_1 +
  --            _qra3_1*-_qra3_1))*_Fr12_1
  utest get _s.specPartials 6 with [
    ("_Fr11_1", ["_qra1_1", "_qra2_1", "_qra3_1", "_qra4_1"]),
    ("_Fr12_1", ["_qra1_1", "_qra2_1", "_qra3_1", "_qra4_1"]),
    ("_Tr23_1", []),
    ("_qra1_1", ["_Fr11_1", "_Fr12_1", "_qra1_1", "_qra4_1"]),
    ("_qra2_1", ["_Fr11_1", "_Fr12_1", "_qra2_1", "_qra3_1"]),
    ("_qra3_1", ["_Fr11_1", "_Fr12_1", "_qra2_1", "_qra3_1"]),
    ("_qra4_1", ["_Fr11_1", "_Fr12_1", "_qra1_1", "_qra4_1"])
  ]
  in

  -- _x_1'' = _vm1_1'
  utest get _s.specPartials 7 with [
    ("_x_1''", []),
    ("_vm1_1'", [])
  ] in

  -- _y_1'' = _vm2_1'
  utest get _s.specPartials 8 with [
    ("_y_1''", []),
    ("_vm2_1'", [])
  ] in

  -- _z_1'' = _vm3_1'
  utest get _s.specPartials 9 with [
    ("_z_1''", []),
    ("_vm3_1'", [])
  ] in

  -- _Fm1_1 = _vm1_1'
  utest get _s.specPartials 10 with [
    ("_Fm1_1", []),
    ("_vm1_1'", [])
  ] in

  -- _Fm2_1 = _vm2_1'
  utest get _s.specPartials 11 with [
    ("_Fm2_1", []),
    ("_vm2_1'", [])
  ] in

  -- _Fm3_1 = _vm3_1'
  utest get _s.specPartials 12 with [
    ("_Fm3_1", []),
    ("_vm3_1'", [])
  ] in


  -- _Tm1_1 =
  --   (0.166666666667*(_qm2_1*_qm2_1 + _qm1_1*_qm1_1 + -_qm4_1*_qm4_1 +
  --    _qm3_1*-_qm3_1)*(_qm2_1*_qm2_1 + _qm1_1*_qm1_1 + -_qm4_1*_qm4_1 +
  --    _qm3_1*-_qm3_1) +
  --     2.16666666667*(_qm2_1*_qm3_1 + _qm1_1*-_qm4_1 + -_qm4_1*_qm1_1 +
  --                    _qm3_1*_qm2_1)*
  --     (_qm2_1*_qm3_1 + _qm1_1*-_qm4_1 + -_qm4_1*_qm1_1 + _qm3_1*_qm2_1) +
  --     2.16666666667*(_qm2_1*_qm4_1 + _qm1_1*_qm3_1 + _qm4_1*_qm2_1 +
  --                    _qm3_1*_qm1_1)*
  --     (_qm2_1*_qm4_1 + _qm1_1*_qm3_1 + _qm4_1*_qm2_1 + _qm3_1*_qm1_1))*
  --     _omm1_1' +
  --
  --   (0.166666666667*(_qm2_1*_qm2_1 + _qm1_1*_qm1_1 + -_qm4_1*_qm4_1 + _qm3_1*-_qm3_1)*(_qm3_1*_qm2_1 + _qm4_1*_qm1_1 + _qm1_1*_qm4_1 + _qm2_1*_qm3_1) + 2.16666666667*(_qm2_1*_qm3_1 + _qm1_1*-_qm4_1 + -_qm4_1*_qm1_1 + _qm3_1*_qm2_1)*(_qm3_1*_qm3_1 + _qm4_1*-_qm4_1 + _qm1_1*_qm1_1 + -_qm2_1*_qm2_1) + 2.16666666667*(_qm2_1*_qm4_1 + _qm1_1*_qm3_1 + _qm4_1*_qm2_1 + _qm3_1*_qm1_1)*(_qm3_1*_qm4_1 + _qm4_1*_qm3_1 + _qm1_1*-_qm2_1 + -_qm2_1*_qm1_1))*_omm2_1' +
  --
  --   (0.166666666667*(_qm2_1*_qm2_1 + _qm1_1*_qm1_1 + -_qm4_1*_qm4_1 + _qm3_1*-_qm3_1)*(_qm4_1*_qm2_1 + -_qm3_1*_qm1_1 + _qm2_1*_qm4_1 + _qm1_1*-_qm3_1) + 2.16666666667*(_qm2_1*_qm3_1 + _qm1_1*-_qm4_1 + -_qm4_1*_qm1_1 + _qm3_1*_qm2_1)*(_qm4_1*_qm3_1 + _qm3_1*_qm4_1 + _qm2_1*_qm1_1 + _qm1_1*_qm2_1) + 2.16666666667*(_qm2_1*_qm4_1 + _qm1_1*_qm3_1 + _qm4_1*_qm2_1 + _qm3_1*_qm1_1)*(_qm4_1*_qm4_1 + -_qm3_1*_qm3_1 + _qm2_1*-_qm2_1 + _qm1_1*_qm1_1))*_omm3_1' +
  --
  --   (-_omm3_1*(0.166666666667*(_qm3_1*_qm2_1 + _qm4_1*_qm1_1 + _qm1_1*_qm4_1 + _qm2_1*_qm3_1)*(_qm2_1*_qm2_1 + _qm1_1*_qm1_1 + -_qm4_1*_qm4_1 + _qm3_1*-_qm3_1) + 2.16666666667*(_qm3_1*_qm3_1 + _qm4_1*-_qm4_1 + _qm1_1*_qm1_1 + -_qm2_1*_qm2_1)*(_qm2_1*_qm3_1 + _qm1_1*-_qm4_1 + -_qm4_1*_qm1_1 + _qm3_1*_qm2_1) + 2.16666666667*(_qm3_1*_qm4_1 + _qm4_1*_qm3_1 + _qm1_1*-_qm2_1 + -_qm2_1*_qm1_1)*(_qm2_1*_qm4_1 + _qm1_1*_qm3_1 + _qm4_1*_qm2_1 + _qm3_1*_qm1_1)) + _omm2_1*(0.166666666667*(_qm4_1*_qm2_1 + -_qm3_1*_qm1_1 + _qm2_1*_qm4_1 + _qm1_1*-_qm3_1)*(_qm2_1*_qm2_1 + _qm1_1*_qm1_1 + -_qm4_1*_qm4_1 + _qm3_1*-_qm3_1) + 2.16666666667*(_qm4_1*_qm3_1 + _qm3_1*_qm4_1 + _qm2_1*_qm1_1 + _qm1_1*_qm2_1)*(_qm2_1*_qm3_1 + _qm1_1*-_qm4_1 + -_qm4_1*_qm1_1 + _qm3_1*_qm2_1) + 2.16666666667*(_qm4_1*_qm4_1 + -_qm3_1*_qm3_1 + _qm2_1*-_qm2_1 + _qm1_1*_qm1_1)*(_qm2_1*_qm4_1 + _qm1_1*_qm3_1 + _qm4_1*_qm2_1 + _qm3_1*_qm1_1)))*_omm1_1 +
  --
  --   (-_omm3_1*(0.166666666667*(_qm3_1*_qm2_1 + _qm4_1*_qm1_1 + _qm1_1*_qm4_1 + _qm2_1*_qm3_1)*(_qm3_1*_qm2_1 + _qm4_1*_qm1_1 + _qm1_1*_qm4_1 + _qm2_1*_qm3_1) + 2.16666666667*(_qm3_1*_qm3_1 + _qm4_1*-_qm4_1 + _qm1_1*_qm1_1 + -_qm2_1*_qm2_1)*(_qm3_1*_qm3_1 + _qm4_1*-_qm4_1 + _qm1_1*_qm1_1 + -_qm2_1*_qm2_1) + 2.16666666667*(_qm3_1*_qm4_1 + _qm4_1*_qm3_1 + _qm1_1*-_qm2_1 + -_qm2_1*_qm1_1)*(_qm3_1*_qm4_1 + _qm4_1*_qm3_1 + _qm1_1*-_qm2_1 + -_qm2_1*_qm1_1)) + _omm2_1*(0.166666666667*(_qm4_1*_qm2_1 + -_qm3_1*_qm1_1 + _qm2_1*_qm4_1 + _qm1_1*-_qm3_1)*(_qm3_1*_qm2_1 + _qm4_1*_qm1_1 + _qm1_1*_qm4_1 + _qm2_1*_qm3_1) + 2.16666666667*(_qm4_1*_qm3_1 + _qm3_1*_qm4_1 + _qm2_1*_qm1_1 + _qm1_1*_qm2_1)*(_qm3_1*_qm3_1 + _qm4_1*-_qm4_1 + _qm1_1*_qm1_1 + -_qm2_1*_qm2_1) + 2.16666666667*(_qm4_1*_qm4_1 + -_qm3_1*_qm3_1 + _qm2_1*-_qm2_1 + _qm1_1*_qm1_1)*(_qm3_1*_qm4_1 + _qm4_1*_qm3_1 + _qm1_1*-_qm2_1 + -_qm2_1*_qm1_1)))*_omm2_1 +
  --
  --   (-_omm3_1*(0.166666666667*(_qm3_1*_qm2_1 + _qm4_1*_qm1_1 + _qm1_1*_qm4_1 + _qm2_1*_qm3_1)*(_qm4_1*_qm2_1 + -_qm3_1*_qm1_1 + _qm2_1*_qm4_1 + _qm1_1*-_qm3_1) + 2.16666666667*(_qm3_1*_qm3_1 + _qm4_1*-_qm4_1 + _qm1_1*_qm1_1 + -_qm2_1*_qm2_1)*(_qm4_1*_qm3_1 + _qm3_1*_qm4_1 + _qm2_1*_qm1_1 + _qm1_1*_qm2_1) + 2.16666666667*(_qm3_1*_qm4_1 + _qm4_1*_qm3_1 + _qm1_1*-_qm2_1 + -_qm2_1*_qm1_1)*(_qm4_1*_qm4_1 + -_qm3_1*_qm3_1 + _qm2_1*-_qm2_1 + _qm1_1*_qm1_1)) + _omm2_1*(0.166666666667*(_qm4_1*_qm2_1 + -_qm3_1*_qm1_1 + _qm2_1*_qm4_1 + _qm1_1*-_qm3_1)*(_qm4_1*_qm2_1 + -_qm3_1*_qm1_1 + _qm2_1*_qm4_1 + _qm1_1*-_qm3_1) + 2.16666666667*(_qm4_1*_qm3_1 + _qm3_1*_qm4_1 + _qm2_1*_qm1_1 + _qm1_1*_qm2_1)*(_qm4_1*_qm3_1 + _qm3_1*_qm4_1 + _qm2_1*_qm1_1 + _qm1_1*_qm2_1) + 2.16666666667*(_qm4_1*_qm4_1 + -_qm3_1*_qm3_1 + _qm2_1*-_qm2_1 + _qm1_1*_qm1_1)*(_qm4_1*_qm4_1 + -_qm3_1*_qm3_1 + _qm2_1*-_qm2_1 + _qm1_1*_qm1_1)))*_omm3_1
  utest get _s.specPartials 17 with
    let qms = [
      "_qm1_1",
      "_qm2_1",
      "_qm3_1",
      "_qm4_1"
    ] in
    let omms = [
      "_omm1_1",
      "_omm2_1",
      "_omm3_1"
    ] in
    let ommps = [
      "_omm1_1'",
      "_omm2_1'",
      "_omm3_1'"
    ]
    in
    [
      ("_Tm1_1", []),
      ("_qm1_1", join [qms, omms, ommps]),
      ("_qm2_1", join [qms, omms, ommps]),
      ("_qm3_1", join [qms, omms, ommps]),
      ("_qm4_1", join [qms, omms, ommps]),
      ("_omm1_1", concat qms ["_omm2_1", "_omm3_1"]),
      ("_omm2_1", concat qms omms),
      ("_omm3_1", concat qms omms),
      ("_omm1_1'", qms),
      ("_omm2_1'", qms),
      ("_omm3_1'", qms)
    ]
  in

  -- _Tm2_1 =
  --   (0.166666666667*(_qm3_1*_qm2_1 + _qm4_1*_qm1_1 + _qm1_1*_qm4_1 + _qm2_1*_qm3_1)*(_qm2_1*_qm2_1 + _qm1_1*_qm1_1 + -_qm4_1*_qm4_1 + _qm3_1*-_qm3_1) + 2.16666666667*(_qm3_1*_qm3_1 + _qm4_1*-_qm4_1 + _qm1_1*_qm1_1 + -_qm2_1*_qm2_1)*(_qm2_1*_qm3_1 + _qm1_1*-_qm4_1 + -_qm4_1*_qm1_1 + _qm3_1*_qm2_1) + 2.16666666667*(_qm3_1*_qm4_1 + _qm4_1*_qm3_1 + _qm1_1*-_qm2_1 + -_qm2_1*_qm1_1)*(_qm2_1*_qm4_1 + _qm1_1*_qm3_1 + _qm4_1*_qm2_1 + _qm3_1*_qm1_1))*_omm1_1' +
  --
  --   (0.166666666667*(_qm3_1*_qm2_1 + _qm4_1*_qm1_1 + _qm1_1*_qm4_1 + _qm2_1*_qm3_1)*(_qm3_1*_qm2_1 + _qm4_1*_qm1_1 + _qm1_1*_qm4_1 + _qm2_1*_qm3_1) + 2.16666666667*(_qm3_1*_qm3_1 + _qm4_1*-_qm4_1 + _qm1_1*_qm1_1 + -_qm2_1*_qm2_1)*(_qm3_1*_qm3_1 + _qm4_1*-_qm4_1 + _qm1_1*_qm1_1 + -_qm2_1*_qm2_1) + 2.16666666667*(_qm3_1*_qm4_1 + _qm4_1*_qm3_1 + _qm1_1*-_qm2_1 + -_qm2_1*_qm1_1)*(_qm3_1*_qm4_1 + _qm4_1*_qm3_1 + _qm1_1*-_qm2_1 + -_qm2_1*_qm1_1))*_omm2_1' +
  --
  --   (0.166666666667*(_qm3_1*_qm2_1 + _qm4_1*_qm1_1 + _qm1_1*_qm4_1 + _qm2_1*_qm3_1)*(_qm4_1*_qm2_1 + -_qm3_1*_qm1_1 + _qm2_1*_qm4_1 + _qm1_1*-_qm3_1) + 2.16666666667*(_qm3_1*_qm3_1 + _qm4_1*-_qm4_1 + _qm1_1*_qm1_1 + -_qm2_1*_qm2_1)*(_qm4_1*_qm3_1 + _qm3_1*_qm4_1 + _qm2_1*_qm1_1 + _qm1_1*_qm2_1) + 2.16666666667*(_qm3_1*_qm4_1 + _qm4_1*_qm3_1 + _qm1_1*-_qm2_1 + -_qm2_1*_qm1_1)*(_qm4_1*_qm4_1 + -_qm3_1*_qm3_1 + _qm2_1*-_qm2_1 + _qm1_1*_qm1_1))*_omm3_1' +
  --
  --   (_omm3_1*(0.166666666667*(_qm2_1*_qm2_1 + _qm1_1*_qm1_1 + -_qm4_1*_qm4_1 + _qm3_1*-_qm3_1)*(_qm2_1*_qm2_1 + _qm1_1*_qm1_1 + -_qm4_1*_qm4_1 + _qm3_1*-_qm3_1) + 2.16666666667*(_qm2_1*_qm3_1 + _qm1_1*-_qm4_1 + -_qm4_1*_qm1_1 + _qm3_1*_qm2_1)*(_qm2_1*_qm3_1 + _qm1_1*-_qm4_1 + -_qm4_1*_qm1_1 + _qm3_1*_qm2_1) + 2.16666666667*(_qm2_1*_qm4_1 + _qm1_1*_qm3_1 + _qm4_1*_qm2_1 + _qm3_1*_qm1_1)*(_qm2_1*_qm4_1 + _qm1_1*_qm3_1 + _qm4_1*_qm2_1 + _qm3_1*_qm1_1)) + -_omm1_1*(0.166666666667*(_qm4_1*_qm2_1 + -_qm3_1*_qm1_1 + _qm2_1*_qm4_1 + _qm1_1*-_qm3_1)*(_qm2_1*_qm2_1 + _qm1_1*_qm1_1 + -_qm4_1*_qm4_1 + _qm3_1*-_qm3_1) + 2.16666666667*(_qm4_1*_qm3_1 + _qm3_1*_qm4_1 + _qm2_1*_qm1_1 + _qm1_1*_qm2_1)*(_qm2_1*_qm3_1 + _qm1_1*-_qm4_1 + -_qm4_1*_qm1_1 + _qm3_1*_qm2_1) + 2.16666666667*(_qm4_1*_qm4_1 + -_qm3_1*_qm3_1 + _qm2_1*-_qm2_1 + _qm1_1*_qm1_1)*(_qm2_1*_qm4_1 + _qm1_1*_qm3_1 + _qm4_1*_qm2_1 + _qm3_1*_qm1_1)))*_omm1_1 +
  --
  --   (_omm3_1*(0.166666666667*(_qm2_1*_qm2_1 + _qm1_1*_qm1_1 + -_qm4_1*_qm4_1 + _qm3_1*-_qm3_1)*(_qm3_1*_qm2_1 + _qm4_1*_qm1_1 + _qm1_1*_qm4_1 + _qm2_1*_qm3_1) + 2.16666666667*(_qm2_1*_qm3_1 + _qm1_1*-_qm4_1 + -_qm4_1*_qm1_1 + _qm3_1*_qm2_1)*(_qm3_1*_qm3_1 + _qm4_1*-_qm4_1 + _qm1_1*_qm1_1 + -_qm2_1*_qm2_1) + 2.16666666667*(_qm2_1*_qm4_1 + _qm1_1*_qm3_1 + _qm4_1*_qm2_1 + _qm3_1*_qm1_1)*(_qm3_1*_qm4_1 + _qm4_1*_qm3_1 + _qm1_1*-_qm2_1 + -_qm2_1*_qm1_1)) + -_omm1_1*(0.166666666667*(_qm4_1*_qm2_1 + -_qm3_1*_qm1_1 + _qm2_1*_qm4_1 + _qm1_1*-_qm3_1)*(_qm3_1*_qm2_1 + _qm4_1*_qm1_1 + _qm1_1*_qm4_1 + _qm2_1*_qm3_1) + 2.16666666667*(_qm4_1*_qm3_1 + _qm3_1*_qm4_1 + _qm2_1*_qm1_1 + _qm1_1*_qm2_1)*(_qm3_1*_qm3_1 + _qm4_1*-_qm4_1 + _qm1_1*_qm1_1 + -_qm2_1*_qm2_1) + 2.16666666667*(_qm4_1*_qm4_1 + -_qm3_1*_qm3_1 + _qm2_1*-_qm2_1 + _qm1_1*_qm1_1)*(_qm3_1*_qm4_1 + _qm4_1*_qm3_1 + _qm1_1*-_qm2_1 + -_qm2_1*_qm1_1)))*_omm2_1 +
  --
  --   (_omm3_1*(0.166666666667*(_qm2_1*_qm2_1 + _qm1_1*_qm1_1 + -_qm4_1*_qm4_1 + _qm3_1*-_qm3_1)*(_qm4_1*_qm2_1 + -_qm3_1*_qm1_1 + _qm2_1*_qm4_1 + _qm1_1*-_qm3_1) + 2.16666666667*(_qm2_1*_qm3_1 + _qm1_1*-_qm4_1 + -_qm4_1*_qm1_1 + _qm3_1*_qm2_1)*(_qm4_1*_qm3_1 + _qm3_1*_qm4_1 + _qm2_1*_qm1_1 + _qm1_1*_qm2_1) + 2.16666666667*(_qm2_1*_qm4_1 + _qm1_1*_qm3_1 + _qm4_1*_qm2_1 + _qm3_1*_qm1_1)*(_qm4_1*_qm4_1 + -_qm3_1*_qm3_1 + _qm2_1*-_qm2_1 + _qm1_1*_qm1_1)) + -_omm1_1*(0.166666666667*(_qm4_1*_qm2_1 + -_qm3_1*_qm1_1 + _qm2_1*_qm4_1 + _qm1_1*-_qm3_1)*(_qm4_1*_qm2_1 + -_qm3_1*_qm1_1 + _qm2_1*_qm4_1 + _qm1_1*-_qm3_1) + 2.16666666667*(_qm4_1*_qm3_1 + _qm3_1*_qm4_1 + _qm2_1*_qm1_1 + _qm1_1*_qm2_1)*(_qm4_1*_qm3_1 + _qm3_1*_qm4_1 + _qm2_1*_qm1_1 + _qm1_1*_qm2_1) + 2.16666666667*(_qm4_1*_qm4_1 + -_qm3_1*_qm3_1 + _qm2_1*-_qm2_1 + _qm1_1*_qm1_1)*(_qm4_1*_qm4_1 + -_qm3_1*_qm3_1 + _qm2_1*-_qm2_1 + _qm1_1*_qm1_1)))*_omm3_1
  utest get _s.specPartials 18 with
    let qms = [
      "_qm1_1",
      "_qm2_1",
      "_qm3_1",
      "_qm4_1"
    ] in
    let omms = [
      "_omm1_1",
      "_omm2_1",
      "_omm3_1"
    ] in
    let ommps = [
      "_omm1_1'",
      "_omm2_1'",
      "_omm3_1'"
    ]
    in
    [
      ("_Tm2_1", []),
      ("_qm1_1", join [qms, omms, ommps]),
      ("_qm2_1", join [qms, omms, ommps]),
      ("_qm3_1", join [qms, omms, ommps]),
      ("_qm4_1", join [qms, omms, ommps]),
      ("_omm1_1", concat qms omms),
      ("_omm2_1", concat qms ["_omm1_1", "_omm3_1"]),
      ("_omm3_1", concat qms omms),
      ("_omm1_1'", qms),
      ("_omm2_1'", qms),
      ("_omm3_1'", qms)
    ]
  in

  -- _Tm3_1 =
  --   (0.166666666667*(_qm4_1*_qm2_1 + -_qm3_1*_qm1_1 + _qm2_1*_qm4_1 + _qm1_1*-_qm3_1)*(_qm2_1*_qm2_1 + _qm1_1*_qm1_1 + -_qm4_1*_qm4_1 + _qm3_1*-_qm3_1) + 2.16666666667*(_qm4_1*_qm3_1 + _qm3_1*_qm4_1 + _qm2_1*_qm1_1 + _qm1_1*_qm2_1)*(_qm2_1*_qm3_1 + _qm1_1*-_qm4_1 + -_qm4_1*_qm1_1 + _qm3_1*_qm2_1) + 2.16666666667*(_qm4_1*_qm4_1 + -_qm3_1*_qm3_1 + _qm2_1*-_qm2_1 + _qm1_1*_qm1_1)*(_qm2_1*_qm4_1 + _qm1_1*_qm3_1 + _qm4_1*_qm2_1 + _qm3_1*_qm1_1))*_omm1_1' +
  --
  --   (0.166666666667*(_qm4_1*_qm2_1 + -_qm3_1*_qm1_1 + _qm2_1*_qm4_1 + _qm1_1*-_qm3_1)*(_qm3_1*_qm2_1 + _qm4_1*_qm1_1 + _qm1_1*_qm4_1 + _qm2_1*_qm3_1) + 2.16666666667*(_qm4_1*_qm3_1 + _qm3_1*_qm4_1 + _qm2_1*_qm1_1 + _qm1_1*_qm2_1)*(_qm3_1*_qm3_1 + _qm4_1*-_qm4_1 + _qm1_1*_qm1_1 + -_qm2_1*_qm2_1) + 2.16666666667*(_qm4_1*_qm4_1 + -_qm3_1*_qm3_1 + _qm2_1*-_qm2_1 + _qm1_1*_qm1_1)*(_qm3_1*_qm4_1 + _qm4_1*_qm3_1 + _qm1_1*-_qm2_1 + -_qm2_1*_qm1_1))*_omm2_1' +
  --
  --   (0.166666666667*(_qm4_1*_qm2_1 + -_qm3_1*_qm1_1 + _qm2_1*_qm4_1 + _qm1_1*-_qm3_1)*(_qm4_1*_qm2_1 + -_qm3_1*_qm1_1 + _qm2_1*_qm4_1 + _qm1_1*-_qm3_1) + 2.16666666667*(_qm4_1*_qm3_1 + _qm3_1*_qm4_1 + _qm2_1*_qm1_1 + _qm1_1*_qm2_1)*(_qm4_1*_qm3_1 + _qm3_1*_qm4_1 + _qm2_1*_qm1_1 + _qm1_1*_qm2_1) + 2.16666666667*(_qm4_1*_qm4_1 + -_qm3_1*_qm3_1 + _qm2_1*-_qm2_1 + _qm1_1*_qm1_1)*(_qm4_1*_qm4_1 + -_qm3_1*_qm3_1 + _qm2_1*-_qm2_1 + _qm1_1*_qm1_1))*_omm3_1' +
  --
  --   (-_omm2_1*(0.166666666667*(_qm2_1*_qm2_1 + _qm1_1*_qm1_1 + -_qm4_1*_qm4_1 + _qm3_1*-_qm3_1)*(_qm2_1*_qm2_1 + _qm1_1*_qm1_1 + -_qm4_1*_qm4_1 + _qm3_1*-_qm3_1) + 2.16666666667*(_qm2_1*_qm3_1 + _qm1_1*-_qm4_1 + -_qm4_1*_qm1_1 + _qm3_1*_qm2_1)*(_qm2_1*_qm3_1 + _qm1_1*-_qm4_1 + -_qm4_1*_qm1_1 + _qm3_1*_qm2_1) + 2.16666666667*(_qm2_1*_qm4_1 + _qm1_1*_qm3_1 + _qm4_1*_qm2_1 + _qm3_1*_qm1_1)*(_qm2_1*_qm4_1 + _qm1_1*_qm3_1 + _qm4_1*_qm2_1 + _qm3_1*_qm1_1)) + _omm1_1*(0.166666666667*(_qm3_1*_qm2_1 + _qm4_1*_qm1_1 + _qm1_1*_qm4_1 + _qm2_1*_qm3_1)*(_qm2_1*_qm2_1 + _qm1_1*_qm1_1 + -_qm4_1*_qm4_1 + _qm3_1*-_qm3_1) + 2.16666666667*(_qm3_1*_qm3_1 + _qm4_1*-_qm4_1 + _qm1_1*_qm1_1 + -_qm2_1*_qm2_1)*(_qm2_1*_qm3_1 + _qm1_1*-_qm4_1 + -_qm4_1*_qm1_1 + _qm3_1*_qm2_1) + 2.16666666667*(_qm3_1*_qm4_1 + _qm4_1*_qm3_1 + _qm1_1*-_qm2_1 + -_qm2_1*_qm1_1)*(_qm2_1*_qm4_1 + _qm1_1*_qm3_1 + _qm4_1*_qm2_1 + _qm3_1*_qm1_1)))*_omm1_1 +
  --
  --    (-_omm2_1*(0.166666666667*(_qm2_1*_qm2_1 + _qm1_1*_qm1_1 + -_qm4_1*_qm4_1 + _qm3_1*-_qm3_1)*(_qm3_1*_qm2_1 + _qm4_1*_qm1_1 + _qm1_1*_qm4_1 + _qm2_1*_qm3_1) + 2.16666666667*(_qm2_1*_qm3_1 + _qm1_1*-_qm4_1 + -_qm4_1*_qm1_1 + _qm3_1*_qm2_1)*(_qm3_1*_qm3_1 + _qm4_1*-_qm4_1 + _qm1_1*_qm1_1 + -_qm2_1*_qm2_1) + 2.16666666667*(_qm2_1*_qm4_1 + _qm1_1*_qm3_1 + _qm4_1*_qm2_1 + _qm3_1*_qm1_1)*(_qm3_1*_qm4_1 + _qm4_1*_qm3_1 + _qm1_1*-_qm2_1 + -_qm2_1*_qm1_1)) + _omm1_1*(0.166666666667*(_qm3_1*_qm2_1 + _qm4_1*_qm1_1 + _qm1_1*_qm4_1 + _qm2_1*_qm3_1)*(_qm3_1*_qm2_1 + _qm4_1*_qm1_1 + _qm1_1*_qm4_1 + _qm2_1*_qm3_1) + 2.16666666667*(_qm3_1*_qm3_1 + _qm4_1*-_qm4_1 + _qm1_1*_qm1_1 + -_qm2_1*_qm2_1)*(_qm3_1*_qm3_1 + _qm4_1*-_qm4_1 + _qm1_1*_qm1_1 + -_qm2_1*_qm2_1) + 2.16666666667*(_qm3_1*_qm4_1 + _qm4_1*_qm3_1 + _qm1_1*-_qm2_1 + -_qm2_1*_qm1_1)*(_qm3_1*_qm4_1 + _qm4_1*_qm3_1 + _qm1_1*-_qm2_1 + -_qm2_1*_qm1_1)))*_omm2_1 +
  --
  --    (-_omm2_1*(0.166666666667*(_qm2_1*_qm2_1 + _qm1_1*_qm1_1 + -_qm4_1*_qm4_1 + _qm3_1*-_qm3_1)*(_qm4_1*_qm2_1 + -_qm3_1*_qm1_1 + _qm2_1*_qm4_1 + _qm1_1*-_qm3_1) + 2.16666666667*(_qm2_1*_qm3_1 + _qm1_1*-_qm4_1 + -_qm4_1*_qm1_1 + _qm3_1*_qm2_1)*(_qm4_1*_qm3_1 + _qm3_1*_qm4_1 + _qm2_1*_qm1_1 + _qm1_1*_qm2_1) + 2.16666666667*(_qm2_1*_qm4_1 + _qm1_1*_qm3_1 + _qm4_1*_qm2_1 + _qm3_1*_qm1_1)*(_qm4_1*_qm4_1 + -_qm3_1*_qm3_1 + _qm2_1*-_qm2_1 + _qm1_1*_qm1_1)) + _omm1_1*(0.166666666667*(_qm3_1*_qm2_1 + _qm4_1*_qm1_1 + _qm1_1*_qm4_1 + _qm2_1*_qm3_1)*(_qm4_1*_qm2_1 + -_qm3_1*_qm1_1 + _qm2_1*_qm4_1 + _qm1_1*-_qm3_1) + 2.16666666667*(_qm3_1*_qm3_1 + _qm4_1*-_qm4_1 + _qm1_1*_qm1_1 + -_qm2_1*_qm2_1)*(_qm4_1*_qm3_1 + _qm3_1*_qm4_1 + _qm2_1*_qm1_1 + _qm1_1*_qm2_1) + 2.16666666667*(_qm3_1*_qm4_1 + _qm4_1*_qm3_1 + _qm1_1*-_qm2_1 + -_qm2_1*_qm1_1)*(_qm4_1*_qm4_1 + -_qm3_1*_qm3_1 + _qm2_1*-_qm2_1 + _qm1_1*_qm1_1)))*_omm3_1
  utest get _s.specPartials 19 with
    let qms = [
      "_qm1_1",
      "_qm2_1",
      "_qm3_1",
      "_qm4_1"
    ] in
    let omms = [
      "_omm1_1",
      "_omm2_1",
      "_omm3_1"
    ] in
    let ommps = [
      "_omm1_1'",
      "_omm2_1'",
      "_omm3_1'"
    ]
    in
    [
      ("_Tm3_1", []),
      ("_qm1_1", join [qms, omms, ommps]),
      ("_qm2_1", join [qms, omms, ommps]),
      ("_qm3_1", join [qms, omms, ommps]),
      ("_qm4_1", join [qms, omms, ommps]),
      ("_omm1_1", concat qms omms),
      ("_omm2_1", concat qms omms),
      ("_omm3_1", concat qms ["_omm1_1", "_omm2_1"]),
      ("_omm1_1'", qms),
      ("_omm2_1'", qms),
      ("_omm3_1'", qms)
    ]
  in

  -- _x_1'' = _xfa1_1''
  utest get _s.specPartials 20 with [
    ("_x_1''", []),
    ("_xfa1_1''", [])
  ] in

  -- _y_1'' = _xfa2_1''
  utest get _s.specPartials 21 with [
    ("_y_1''", []),
    ("_xfa2_1''", [])
  ] in

  -- _z_1'' = _xfa3_1''
  utest get _s.specPartials 22 with [
    ("_z_1''", []),
    ("_xfa3_1''", [])
  ] in

  -- -2.5*(_qra2_1*_qra2_1'' + 2.*_qra2_1'*_qra2_1' + _qra2_1''*_qra2_1 +
  --       _qra1_1*_qra1_1'' + 2.*_qra1_1'*_qra1_1' + _qra1_1''*_qra1_1 +
  --       -_qra4_1*_qra4_1'' + 2.*-_qra4_1'*_qra4_1' + -_qra4_1''*_qra4_1 +
  --       _qra3_1*-_qra3_1'' + 2.*_qra3_1'*-_qra3_1' + _qra3_1''*-_qra3_1) =
  -- -_xfa1_1''
  utest get _s.specPartials 23 with [
    ("_qra1_1", ["_qra1_1''"]),
    ("_qra2_1", ["_qra2_1''"]),
    ("_qra3_1", ["_qra3_1''"]),
    ("_qra4_1", ["_qra4_1''"]),
    ("_qra1_1'", ["_qra1_1'"]),
    ("_qra2_1'", ["_qra2_1'"]),
    ("_qra3_1'", ["_qra3_1'"]),
    ("_qra4_1'", ["_qra4_1'"]),
    ("_qra1_1''", ["_qra1_1"]),
    ("_qra2_1''", ["_qra2_1"]),
    ("_qra3_1''", ["_qra3_1"]),
    ("_qra4_1''", ["_qra4_1"]),
    ("_xfa1_1''", [])
  ] in

  -- -2.5*(_qra3_1*_qra2_1''+2.*_qra3_1'*_qra2_1'+_qra3_1''*_qra2_1+
  --       _qra4_1*_qra1_1''+2.*_qra4_1'*_qra1_1'+_qra4_1''*_qra1_1+
  --       _qra1_1*_qra4_1''+2.*_qra1_1'*_qra4_1'+_qra1_1''*_qra4_1+
  --       _qra2_1*_qra3_1''+2.*_qra2_1'*_qra3_1'+_qra2_1''*_qra3_1) =
  -- -_xfa2_1''
  utest get _s.specPartials 24 with [
    ("_qra1_1", ["_qra4_1''"]),
    ("_qra2_1", ["_qra3_1''"]),
    ("_qra3_1", ["_qra2_1''"]),
    ("_qra4_1", ["_qra1_1''"]),
    ("_qra1_1'", ["_qra4_1'"]),
    ("_qra2_1'", ["_qra3_1'"]),
    ("_qra3_1'", ["_qra2_1'"]),
    ("_qra4_1'", ["_qra1_1'"]),
    ("_qra1_1''", ["_qra4_1"]),
    ("_qra2_1''", ["_qra3_1"]),
    ("_qra3_1''", ["_qra2_1"]),
    ("_qra4_1''", ["_qra1_1"]),
    ("_xfa2_1''", [])
  ] in

  -- -2.5*(_qra4_1*_qra2_1''+2.*_qra4_1'*_qra2_1'+_qra4_1''*_qra2_1+
  --       -_qra3_1*_qra1_1''+2.*-_qra3_1'*_qra1_1'+-_qra3_1''*_qra1_1+
  --       _qra2_1*_qra4_1''+2.*_qra2_1'*_qra4_1'+_qra2_1''*_qra4_1+
  --       _qra1_1*-_qra3_1''+2.*_qra1_1'*-_qra3_1'+_qra1_1''*-_qra3_1) =
  --  -_xfa3_1''
  utest get _s.specPartials 25 with [
    ("_qra1_1", ["_qra3_1''"]),
    ("_qra2_1", ["_qra4_1''"]),
    ("_qra3_1", ["_qra1_1''"]),
    ("_qra4_1", ["_qra2_1''"]),
    ("_qra1_1'", ["_qra3_1'"]),
    ("_qra2_1'", ["_qra4_1'"]),
    ("_qra3_1'", ["_qra1_1'"]),
    ("_qra4_1'", ["_qra2_1'"]),
    ("_qra1_1''", ["_qra3_1"]),
    ("_qra2_1''", ["_qra4_1"]),
    ("_qra3_1''", ["_qra1_1"]),
    ("_qra4_1''", ["_qra2_1"]),
    ("_xfa3_1''", [])
  ] in

  --  _Fta1_1 = 0.-_Fr11_1
  utest get _s.specPartials 26 with [
    ("_Fr11_1", []),
    ("_Fta1_1", [])
  ] in

  -- _Fta2_1 = 0.-_Fr12_1
  utest get _s.specPartials 27 with [
    ("_Fr12_1", []),
    ("_Fta2_1", [])
  ] in

  -- _Fta3_1 = 0.-_Fr13_1
  utest get _s.specPartials 28 with [
    ("_Fr13_1", []),
    ("_Fta3_1", [])
  ] in

  -- 0. = _Fr11_1-_Fm1_1
  utest get _s.specPartials 29 with [
    ("_Fm1_1", []),
    ("_Fr11_1", [])
  ] in

  -- 0. = _Fr12_1-_Fm2_1
  utest get _s.specPartials 30 with [
    ("_Fm2_1", []),
    ("_Fr12_1", [])
  ] in

  -- 9.81 = _Fr13_1-_Fm3_1
  utest get _s.specPartials 31 with [
    ("_Fm3_1", []),
    ("_Fr13_1", [])
  ] in

  -- _omr21_1' = _omm1_1'
  utest get _s.specPartials 32 with [
    ("_omm1_1'", []),
    ("_omr21_1'", [])
  ] in

  -- _omr22_1' = _omm2_1'
  utest get _s.specPartials 33 with [
    ("_omm2_1'", []),
    ("_omr22_1'", [])
  ] in

  -- _omr23_1' = _omm3_1'
  utest get _s.specPartials 34 with [
    ("_omm3_1'", []),
    ("_omr23_1'", [])
  ] in

  -- _Tm1_1 = 0.-_Tr21_1
  utest get _s.specPartials 35 with [
    ("_Tm1_1", []),
    ("_Tr21_1", [])
  ] in

  -- _Tm2_1 = 0.-_Tr22_1
  utest get _s.specPartials 36 with [
    ("_Tm2_1", []),
    ("_Tr22_1", [])

  ] in

  -- _Tm3_1 = 0.-_Tr23_1
  utest get _s.specPartials 37 with [
    ("_Tm3_1", []),
    ("_Tr23_1", [])
  ] in

  -- _Tr11_1 = 0.
  utest get _s.specPartials 38 with [("_Tr11_1", [])] in

  -- _Tr12_1 = 0.
  utest get _s.specPartials 39 with [("_Tr12_1", [])] in

  -- _Tr13_1 = 0.
  utest get _s.specPartials 40 with [("_Tr13_1", [])] in

  () with ()
in

----

let _parse = lam prog. daeParseExn "internal" prog in

let prog = _parse "
  let mul = lam x. lam y. x*y end
  let pow2 = lam x. mul x x end
  variables
  x, vx, y, vy, h : Float
  init
  x = 1.;
  vy' = 0. - 1.
  equations
  vx = x';
  vy = y';
  vx' = mul x h;
  vy' = mul y h - 1.;
  pow2 x + pow2 y = pow2 1.
  output
  {x, vx, vx'}
  "
in

-- let prog = _parse "
--   variables
--   x, vx : Float
--   init
--   x = 1.
--   equations
--   vx = x';
--   vx' + vx + x = 0.
--   output
--   {x, vx, vx'}
--   "
-- in

logMsg logLevel.debug
  (lam. strJoin "\n" ["Input program:", daeProgToString prog]);

let dae =
  consumeWarnErrsExn
    (daeExprToDAE (typeCheck (adSymbolize (daeDesugarProg prog))))
in

logMsg logLevel.debug
  (lam. strJoin "\n" [
    "DAE structure before index-reduction:",
    daeStructureToString dae
  ]);

let dae =
  consumeWarnErrsExn
    (result.bind (daeStructuralAnalysis defaultOptions dae)
       (lam analysis.
         let dae = transIndexReduceNaive dae analysis in
         result.ok dae))
in

logMsg logLevel.debug
  (lam. strJoin "\n" [
    "DAE structure after index-reduction:",
    daeStructureToString dae
  ]);

let dae = transElimAliases (daePEval dae) in

logMsg logLevel.debug
  (lam. strJoin "\n" [
    "DAE after PEval and alias elimination:",
    daeToString dae
  ]);


logSetLogLevel logLevel.error;

let jac = daeJacobian 3 dae in

logMsg logLevel.debug (lam. strJoin "\n" ["Jacobian:", daeJacobianToString jac]);

()
