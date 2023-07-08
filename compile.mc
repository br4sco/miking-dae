include "sys.mc"
include "option.mc"
include "log.mc"

include "mexpr/ast-builder.mc"
include "mexpr/utils.mc"
include "mexpr/symbolize.mc"

include "./ast.mc"
include "./parse.mc"
include "./transform.mc"
include "./gen.mc"
include "./error-print.mc"
include "./dae-arg.mc"
include "./dae.mc"

let peadaeNameSpace = "PEADAE"

lang DAECompile =
  DAEAst + DAEStructuralAnalysis + DAEGen + DAEPrettyPrint +
  MExprFindSym + MExprSym + BootParser

  sem daeSrcPathExn : () -> String
  sem daeSrcPathExn =| () ->
    optionGetOrElse
      (lam. error
            (strJoin " " [
              peadaeNameSpace,
              "is unset. Please set it to point to the root of PEADAE source"
            ]))
      (sysGetEnv peadaeNameSpace)

  sem daeCompile : Options -> Expr -> Res Expr
  sem daeCompile options =| prog ->
    (if options.debugCompilation then logSetLogLevel logLevel.debug else ());
    let expr = typeCheck (adBuiltinSymsToConsts (adSymbolize prog)) in
    let logDebug = lam head. lam msg.
      logDebug (lam. strJoin "\n" [join ["daeCompile:", msg, ":"], msg])
    in
    let logDebugDAE = lam head. lam dae.
      logDebug head
        (lam.
          if options.disableDebugStructure then daeToString dae
          else daeStructureToString dae)
    in
    let expr = typeCheck (adBuiltinSymsToConsts (adSymbolize prog)) in
    let daePEval = daePEval options.constantFold in
    result.bind (daeExprToDAE expr)
      (lam dae.
        let dae =
          if options.cse then
            { dae with eqns = map (lam eqn. (cse eqn.0, cse eqn.1)) dae.eqns }
          else dae
        in
        let dae =
          if options.aliasElim then
            let dae = transElimAliases dae in
            logDebugDAE "DAE after alias elimination" dae;
            dae
          else dae
        in
        result.bind (daeStructuralAnalysis options dae)
          (lam analysis.
            logDebugDAE "DAE before index-reduction" dae;
            let dae = transIndexReduceNaive dae analysis in
            logDebugDAE "DAE after index-reduction" dae;
            let dae =
              if options.disablePeval then dae
              else
                let dae = daePEval dae in
                logDebugDAE "DAE after PEval" dae;
                dae
            in
            let dae =
              if options.aliasElim then
                let dae = transElimAliases dae in
                logDebugDAE "DAE structure after alias elimination" dae;
                dae
              else dae
            in
            let runtime =
              parseMCoreFile {
                defaultBootParserParseMCoreFileArg with
                eliminateDeadCode = true,
                allowFree = true
              } (join [daeSrcPathExn (), "/runtime.mc"])
            in
            let runtime = symbolize runtime in
            let runtimeNames = [
              "daeRuntimeRun", "sin", "cos", "exp", "pow", "sqrt"
            ] in
            let runtimeNames =
              foldl2
                (lam runtimeNames. lam str. lam name.
                  mapUpdate str (lam. name) runtimeNames)
                (mapEmpty cmpString)
                runtimeNames
                (findNamesOfStrings runtimeNames runtime)
            in
            result.bind (daeGenRuntime options runtimeNames dae)
              (lam expr.
                let expr = adBuiltinConstsToSyms expr in
                let expr =
                  substituteIdentifiers
                    (mapFromSeq
                       nameCmp
                       (map
                          (lam x. (x.1, mapFindExn x.0 runtimeNames))
                          adBuiltinSymbols))
                    expr
                in
                result.ok (bind_ runtime expr))))

  sem daeCompileExn : Options -> Expr -> Expr
  sem daeCompileExn options =| dae ->
    consumeWarnErrsExn (daeCompile options dae)
end



lang TestLang =
  DAEParseAnalysis +
  DAEParseDesugar +
  DAEExpr +
  DAEStructuralAnalysis +
  DAEPrettyPrint +
  DAECompile
end

mexpr

use TestLang in

let _parse = lam prog.
  let prog = daeParseExn "internal" prog in

  logMsg logLevel.debug
    (lam. strJoin "\n" ["Input program:", daeProgToString prog]);

  let e = typeCheck (adSymbolize (daeDesugarProg prog)) in

  logMsg logLevel.debug
    (lam. strJoin "\n" ["MExpr program:", expr2str e]);

  e
in

-------------------
-- Test Pendulum --
-------------------

logSetLogLevel logLevel.error;

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

let expr = daeCompileExn defaultOptions dae in

logMsg logLevel.debug
  (lam. strJoin "\n" ["Output program:", expr2str expr]);

utest typeCheck (adSymbolize expr); true with true in

()
