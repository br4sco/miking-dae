include "arg.mc"

type Options = {
  debugCompilation : Bool,
  debugRuntime : Bool,
  disableDebugStructure : Bool,
  disablePeval : Bool,
  constantFold : Bool,
  cse : Bool,
  aliasElim : Bool,
  numericJac : Bool,
  jacSpecThreshold : Float,
  jacSpecThresholdAbsolute : Option Int,
  outputOnlyLast : Bool
}

let defaultOptions = {
  debugCompilation = false,
  debugRuntime = false,
  disableDebugStructure = false,
  disablePeval = false,
  constantFold = false,
  cse = false,
  aliasElim = false,
  numericJac = false,
  jacSpecThreshold = 1.,
  jacSpecThresholdAbsolute = None (),
  outputOnlyLast = true
}

let argConfig = [
  ([("--debug-compilation", "", "")],
   "Print debug information during compilation. ",
   lam p. { p.options with debugCompilation = true }),
  ([("--debug-runtime", "", "")],
   "Print debug information during runtime. ",
   lam p. { p.options with debugRuntime = true }),
  ([("--disable-peval", "", "")],
   "Disable partial evaluation. ",
   lam p. { p.options with disablePeval = true }),
  ([("--constant-fold", "", "")],
   "Do constant folding after partial evaluation. ",
   lam p. { p.options with constantFold = true }),
  ([("--cse", "", "")],
   "Perform common subexpression elimination. ",
   lam p. { p.options with cse = true }),
  ([("--alias-elim", "", "")],
   "Eliminate alias equations. ",
   lam p. { p.options with aliasElim = true }),
  ([("--numeric-jac", "", "")],
   "Numerically approximate Jacobian. ",
   lam p. { p.options with numericJac = true }),
  ([("--jac-spec-threshold", " ", "<value>")],
   "Abstract interval that constrols how many partial derivatives in the Jacobian should be specialized, where 0 means none and 1 means all. More specialized partial derivatives results in more code. For a DAE with n equations this and --jac-spec-threshold phi, this is equivalent to --jac-spec-threshold-absolute (floor phi*n)",
   lam p. { p.options with jacSpecThreshold = argToFloatInterval p 0. 1. }),
  ([("--jac-spec-absolute", " ", "<value>")],
   "The maximum number non-zero elements a partial derivative in the Jacobian must in order to be specialized. Overrides --jac-spec-threshold. ",
   lam p. { p.options with jacSpecThresholdAbsolute = Some (argToInt p) }),
  ([("--output-only-last", "", "")],
   "Only output the last time-step to standard out. ",
   lam p. { p.options with outputOnlyLast = true })
]

let usage = lam prog. join [
  "Usage: ", prog, " FILE [OPTION]\n\n",
  "Options:\n",
  argHelpOptions argConfig,
  "\n"
]
