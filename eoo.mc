include "arg.mc"

include "./ast_gen.mc"
include "./desugar.mc"
include "./eoocore.mc"

type Options = {
  debugBackend : Bool,
  debugDesugaring : Bool
}

let eooDefaultOptions = {
  debugBackend = false,
  debugDesugaring = false
}

let eooArgConfig = [
  ([("--debug-backend", "", "")],
   "Print debug information during in the compiler backend. ",
   lam p. { p.options with debugBackend = true }),
  ([("--debug-desugaring", "", "")],
   "Print the program after desugaring to std error. ",
   lam p. { p.options with debugDesugaring = true })
]

let usage = lam prog. join [
  "Usage: ", prog, " FILE [OPTION]\n\n",
  "Options:\n",
  argHelpOptions argConfig,
  "\n"
]

lang EOOLangMain =
  EOOCoreLang + EOODesugar + ConstTransformer + MExprPrettyPrint

  sem eooBuiltin : () -> [(String, Const)]
  sem eooBuiltin =| () ->
    concat (daeBuiltin ()) [
      -- integer operations
      ("addi", CAddi ()),
      ("subi", CSubi ()),
      ("muli", CMuli ()),
      ("divi", CDivi ()),
      ("modi", CModi ()),
      ("negi", CNegi ()),
      ("lti", CLti ()),
      ("leqi", CLeqi ()),
      ("gti", CGti ()),
      ("geqi", CGeqi ()),
      ("eqi", CEqi ()),
      ("neqi", CNeqi ()),
      ("slli", CSlli ()),
      ("srli", CSrli ()),
      ("srai", CSrai ()),
      -- float operations
      ("add", CAddf ()),
      ("sub", CSubf ()),
      ("mul", CMulf ()),
      ("div", CDivf ()),
      ("neg", CNegf ()),
      ("lt", CLtf ()),
      ("leq", CLeqf ()),
      ("gt", CGtf ()),
      ("geq", CGeqf ()),
      ("eq", CEqf ()),
      ("neq", CNeqf ()),
      ("floor", CFloorfi ()),
      ("ceil", CCeilfi ()),
      ("round", CRoundfi ()),
      -- Sequences
      ("create", CCreate ()),
      ("length", CLength ()),
      ("concat", CConcat ()),
      ("get", CGet ()),
      ("set", CSet ()),
      ("cons", CCons ()),
      ("snoc", CSnoc ()),
      ("splitAt", CSplitAt ()),
      ("reverse", CReverse ()),
      ("head", CHead ()),
      ("tail", CTail ()),
      ("null", CNull ()),
      ("map", CMap ()),
      ("mapi", CMapi ()),
      ("foldl", CFoldl ()),
      ("foldr", CFoldr ()),
      ("subsequence", CSubsequence ()),
      -- symbol operations
      ("gensym", CGensym ()),
      ("eqsym", CEqsym ()),
      -- Model operations
      ("genVar", CGenDynVarf ())
    ]
end

mexpr

use EOOLangMain in

switch argParse eooDefaultOptions eooArgConfig
case ParseOK r then
  -- Print menu if not exactly one file argument
  if neqi (length r.strings) 1 then
    print (usage (get argv 0));
    exit 1
  else
    let file = head r.strings in

    -- Parse
    let eooAST = parseEOOExn file (readFile file) in

    -- Desugar
    let eoocoreAST = constTransform (eooBuiltin ()) (eooDesugar eooAST) in
    (if (r.options.debugDesugaring) then
        printError (expr2str eoocoreAST); flushStderr () else ());

    -- Compile EOO program
    let mexprAST =
      eooCoreCompile
        { eooCoreCompileOptionsDefault () with debug = r.options.debugBackend }
        eoocoreAST
    in

    -- Output MCore program to stdout
    printLn (concat "mexpr\n" (expr2str mexprAST));

    ()
case result then
  argPrintError result;
  exit 1
end
