include "mexpr/symbolize.mc"
include "mexpr/type-check.mc"

include "./ast.mc"
include "./elaborate.mc"
include "./lib/elaboration.mc"
include "./dae/ast.mc"
include "./dae/compile.mc"

lang EOOCoreLang =
  EOOCoreAst + DAEAst + EOOCoreElaborate + MExprSym + MExprTypeCheck + MExprCmp

  type EOOCoreCompileOptions = {
    debug : Bool
  }

  sem eooCoreCompileOptionsDefault : () -> EOOCoreCompileOptions
  sem eooCoreCompileOptionsDefault =| () -> {
    debug = false
  }

  sem eooCoreCompile : EOOCoreCompileOptions -> Expr -> Expr
  sem eooCoreCompile options =| ast ->
    -- Symbolize
    let symEnv = symEnvAddBuiltinTypes symEnvDefault (eooCoreBuiltinTypes ()) in
    let ast = symbolizeExpr symEnv ast in

    -- Type-Check
    let tcEnv =
      typecheckEnvAddBuiltinTypes typcheckEnvDefault (eooCoreBuiltinTypes ())
    in
    let ast = typeCheckExpr tcEnv ast in

    -- Flatten
    let flatEOO = eooFlatten ast in

    -- Eloborate Model Graphs
    let flatEOO = eooElaborateGraphs flatEOO in

    -- Compile DAE
    let ast =
      use DAECompile in
      let daer = {
        bindings = flatEOO.bindings,
        vars = flatEOO.vars,
        ieqns = flatEOO.ieqns,
        eqns = flatEOO.eqns,
        out = flatEOO.out,
        info = NoInfo ()
      } in
      daeCompile
        {
          daeDefaultOptions with
          debug = options.debug
        }
        daer
    in
    ast
end
