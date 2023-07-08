include "sys.mc"
include "option.mc"
include "log.mc"

include "mexpr/ast-builder.mc"
include "mexpr/utils.mc"
include "mexpr/symbolize.mc"
include "mexpr/cse.mc"

include "./dae.mc"
include "./desugar.mc"
include "./dae-arg.mc"

let peadaeNameSpace = "PEADAE"

lang DAECompile =
  DAE + MExprConstantFold + MExprFindSym + MExprSubstitute + BootParser + CSE


  sem daeSrcPathExn : () -> String
  sem daeSrcPathExn =| () ->
    optionGetOrElse
      (lam. error
            (strJoin " " [
              peadaeNameSpace,
              "is unset. Please set it to point to the root of PEADAE source"
            ]))
      (sysGetEnv peadaeNameSpace)

  sem daeCompile : Options -> TmDAERec -> Expr
  sem daeCompile options =| daer ->
    let logDebug = lam head. lam msg.
      logDebug (lam. strJoin "\n" [join ["daeCompile:", msg, ":"], msg])
    in
    match typeCheck (adBuiltinSymsToConsts (adSymbolize (TmDAE daer)))
      with TmDAE daer
    then
      -- Setup runtime
      let runtime =
        parseMCoreFile {
          defaultBootParserParseMCoreFileArg with
          eliminateDeadCode = true,
          allowFree = true
        } (join [daeSrcPathExn (), "/runtime.mc"])
      in
      let runtime = symbolize runtime in
      let runtimeNames = [
        "daeRuntimeRun", "sin", "cos", "exp", "pow", "sqrt", "onehot"
      ] in
      let runtimeNames =
        foldl2
          (lam runtimeNames. lam str. lam name.
            mapUpdate str (lam. name) runtimeNames)
          (mapEmpty cmpString)
          runtimeNames
          (findNamesOfStrings runtimeNames runtime)
      in
      -- Compile DAE
      let daer = daeAnnotDVars daer in
      logDebug "analysis"
        (lam.
          strJoin " " ["number of equations:", int2string (length daer.eqns)]);
      let analysis = daeStructuralAnalysis daer in
      logDebug "analysis"
        (lam. strJoin " " [
          "max equation offset",
          int2string
            (maxOrElse (lam. error "impossible") subi analysis.eqnsOffset)
        ]);
      let daer = daeIndexReduce analysis.eqnsOffset daer in
      let state = daeFirstOrderState analysis.varOffset in
      let isdiffvars = daeIsDiffVars state in
      let daer = daeOrderReduce state (nameSym "y") (nameSym "yp") daer in
      let ts = [
        daeGenInitExpr state daer,
        daeGenResExpr daer,
        daeGenOutExpr daer
      ]
      in
      match
        if options.disablePeval then ts
        else map (lam t. foldLets (peval t)) ts
        with [iexpr, rexpr, oexpr]
      in
      match
        if options.constantFold then
          (constantfold iexpr, constantfold rexpr, constantfold oexpr)
        else (iexpr, rexpr, oexpr)
        with (iexpr, rexpr, oexpr)
      in
      match
        if options.cse then
          (cse iexpr, cse rexpr, cse oexpr)
        else (iexpr, rexpr, oexpr)
        with (iexpr, rexpr, oexpr)
      in
      match
        if options.numericJac then (ulam_ "" never_, ulam_ "" never_) else
          (daeGenMixedJacY options.jacSpecThreshold daer,
           daeGenMixedJacYp options.jacSpecThreshold daer)
        with (jacY, jacYp)
      in
      -- Generate runtime
      let t =
        (appSeq_ (nvar_ (mapFindExn "daeRuntimeRun" runtimeNames)) [
          (bool_ options.debugRuntime),
          (bool_ options.numericJac),
          seq_ (map bool_ isdiffvars),
          iexpr,
          rexpr,
          jacY,
          jacYp,
          oexpr
        ])
      in
      let t = adBuiltinConstsToSyms t in
      let t =
        substituteIdentifiers
          (mapFromSeq
             nameCmp
             (map
                (lam x. (x.1, mapFindExn x.0 runtimeNames))
                adBuiltinSymbols))
          t
      in
      bind_ runtime t
    else error "impossible"
end

lang TestLang = DAEParseAnalysis + DAEParseDesugar + DAECompile end

mexpr

use TestLang in

let _parse = lam prog.
  let prog = daeParseExn "internal" prog in
  logMsg logLevel.debug
    (lam. strJoin "\n" ["Input program:", daeProgToString prog]);
  let daer = daeDesugarProg prog in
  match typeCheck (adSymbolize (TmDAE daer)) with TmDAE daer then
    daer
  else error "impossible"
in

-------------------
-- Test Pendulum --
-------------------

logSetLogLevel logLevel.error;

let dae = _parse "
  let mul = lam x. lam y. x*y end
  let pow2 = lam x. mul x x end
  variables
  x, y, h : Float
  init
  x = 1.;
  x' = 2.;
  y'' = 0. - 1.
  equations
  x'' = mul x h;
  y'' = mul y h - 1.;
  pow2 x + pow2 y = pow2 1.
  output
  {x, x', x''}
  "
in

let t = daeCompile defaultOptions dae in

logMsg logLevel.debug
  (lam. strJoin "\n" ["Output program:", expr2str t]);

utest typeCheck (adSymbolize t); true with true in

()
