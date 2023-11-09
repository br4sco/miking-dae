include "arg.mc"

include "mexpr/boot-parser.mc"

include "./compile.mc"

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

lang EOOCoreLangMain =
  EOOCoreLang + MExprPrettyPrint + BootParser
end

mexpr

use EOOCoreLangMain in

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

    -- Compile EOO program
    let ast =
      eooCoreCompile
        { eooCoreCompileOptionsDefault () with debug = r.options.debug }
        ast
    in

    -- Output MCore program to stdout
    printLn (concat "mexpr\n" (expr2str ast));

    ()
case result then
  argPrintError result;
  exit 1
end
