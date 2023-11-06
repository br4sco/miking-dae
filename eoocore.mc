include "arg.mc"

include "mexpr/symbolize.mc"
include "mexpr/type-check.mc"
include "mexpr/boot-parser.mc"

include "./ast.mc"
include "./elaborate.mc"
include "./lib/elaboration.mc"
include "./dae/ast.mc"
include "./dae/compile.mc"

type Options = {
  debug : Bool
}

let eoocoreDefaultOptions = {
  debug = false
}

let argConfig = [
  ([("--debug", "", "")],
   "Print debug information during compilation. ",
   lam p. { p.options with debug = true })
]

let usage = lam prog. join [
  "Usage: ", prog, " FILE [OPTION]\n\n",
  "Options:\n",
  argHelpOptions argConfig,
  "\n"
]

lang EOOCoreLang =
  EOOCoreAst + DAEAst + EOOCoreElaborate +
  MExprSym + MExprPrettyPrint + MExprTypeCheck + MExprCmp + BootParser
end

lang TyAnnotFull = MExprPrettyPrint + TyAnnot + HtmlAnnotator
end

mexpr

use EOOCoreLang in

switch argParse eoocoreDefaultOptions argConfig
case ParseOK r then
  -- Print menu if not exactly one file argument
  if neqi (length r.strings) 1 then
    print (usage (get argv 0));
    exit 1
  else
    let file = head r.strings in

    -- Parse
    let ast = parseMCoreFile {
      _defaultBootParserParseMCoreFileArg () with
      keywords = _daeAstKeywords,
      allowFree = true,
      builtin = join [builtin, daeBuiltin (), eooCoreBuiltin ()]
    } file in

    -- Symbolize
    let symEnv = {
      symEnvEmpty with
      tyConEnv =
        mapUnion
          symEnvEmpty.tyConEnv
          (mapFromSeq cmpString
             (map (lam t. (t.0, nameNoSym t.0)) (eooCoreBuiltinTypes ())))
    } in
    let ast = symbolizeExpr symEnv ast in

    -- Type-Check
    let tcEnv = {
      _tcEnvEmpty with
      tyConEnv =
        mapUnion
          _tcEnvEmpty.tyConEnv
          (mapFromSeq nameCmp
             (map
                (lam t.
                  (nameNoSym t.0, (0, map nameSym t.1, tyvariant_ [])))
                (eooCoreBuiltinTypes ())))
    } in
    let ast = typeCheckExpr tcEnv ast in

    -- Flatten
    let eoo = flatten ast in

    -- Eloborate Edges
    let eoo = flatEOOElaborate eoo in

    -- Compile DAE
    let ast =
      use DAECompile in
      let daer = {
        bindings = eoo.bindings,
        vars = eoo.vars,
        ieqns = eoo.ieqns,
        eqns = eoo.eqns,
        out = eoo.out,
        info = NoInfo ()
      } in
      daeCompile
        {
          daeDefaultOptions with
          debug = r.options.debug
        }
        daer
    in

    -- Output MCore program to stdout
    printLn (concat "mexpr\n" (expr2str ast));

    ()
case result then
  argPrintError result;
  exit 1
end
