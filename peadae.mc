include "arg.mc"

include "./error-print.mc"
include "./dae-arg.mc"
include "./parse.mc"
include "./desugar.mc"
include "./compile.mc"

lang PEADAE =
  DAEParseDesugar +
  DAEParseAnalysis +
  DAEExpr +
  DAEStructuralAnalysis +
  DAEPrettyPrint +
  DAECompile
end

mexpr

use PEADAE in

switch argParse defaultOptions argConfig
case ParseOK r then
  -- Print menu if not exactly one file argument
  if neqi (length r.strings) 1 then
    print (usage (get argv 0));
    exit 1
  else
    let res =
      let filename = head r.strings in
      let prog = parseDAEParseExn filename (readFile filename) in
      result.bind (daeProgWellFormed prog)
        (lam prog.
          result.bind (daeCompile r.options (daeDesugarProg prog))
            (lam expr.
              printLn (strJoin "\n" ["mexpr", expr2str expr]);
              result.ok ()))
    in
    consumeWarnErrsExn res
case result then
  argPrintError result;
  exit 1
end
