include "mexpr/symbolize.mc"
include "mexpr/eq.mc"
include "mexpr/pprint.mc"
include "mexpr/type-check.mc"
include "mexpr/ast-builder.mc"
include "mexpr/ast-result.mc"
include "mexpr/keyword-maker.mc"
include "mexpr/boot-parser.mc"

include "map.mc"
include "result.mc"
include "error.mc"

include "./ast_gen.mc"
include "../miking-pead/ad.mc"

type Res a = Result ErrorSection ErrorSection a

let _daeAstKeywords = ["prim"]

lang DAEAst = DAEParseAst + AstResult +
  MExprSym + MExprEq + MExprPrettyPrint + MExprTypeCheck + AD +
  BootParser + KeywordMaker

  syn Expr =
  -- e' in MExpr
  | TmPrim Expr
  -- -- x(t):T in MExpr
  -- | TmVarDecl {
  --   ty : Type,
  --   info : Info
  -- }

  -- Accessors
  sem infoTm =
  | TmPrim t -> infoTm t
  -- | TmVarDecl r -> r.info

  sem tyTm =
  | TmPrim t -> tyTm t
  -- | TmVarDecl r -> r.ty

  sem withInfo info =
  | TmPrim t -> TmPrim (withInfo info t)
  -- | TmVarDecl r -> TmVarDecl { r with info = info }

  sem withType ty =
  | TmPrim t -> TmPrim (withType ty t)
  -- | TmVarDecl r -> TmVarDecl { r with ty = ty }

  -- Shallow maps/folds
  sem smapAccumL_Expr_Expr f acc =
  | TmPrim t ->
    match f acc t with (acc, t) in
    (acc, TmPrim t)

  -- KeywordMaker
  sem isKeyword =
  | TmPrim _ -> true
  -- | TmVarDecl _ -> true

  sem matchKeywordString (info : Info) =
  | "prim" ->
    Some (1, lam lst. TmPrim (get lst 0))
  -- | "var" ->
  --   Some (0, lam lst. TmVarDecl { info = info, ty = TyUnknown { info = info }})

  -- Eq
  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmPrim r ->
    match lhs with TmPrim l then eqExprH env free l r
    else None ()
  -- | TmLet {ident = rident, body = TmVarDecl _, inexpr = rinexpr} ->
  --   match lhs with
  --     TmLet {ident = lident, body = TmVarDecl _, inexpr = linexpr}
  --   then
  --     let varEnv = biInsert (lident, rident) env.varEnv in
  --     eqExprH {env with varEnv = varEnv} free linexpr rinexpr
  --   else None ()

  -- PrettyPrint
  sem isAtomic =
  | TmPrim _ -> false
  -- | TmVarDecl _ -> true

  sem pprintCode (indent : Int) (env : PprintEnv) =
  | TmPrim t ->
    match pprintCode indent env t with (env, arg) in
    (env, join ["prim", pprintNewline indent, arg])
  -- | TmVarDecl _ ->
  --   (env, join ["var"])

  -- Type Check
  sem typeCheckExpr env =
  | TmPrim t -> TmPrim (typeCheckExpr env t)
  -- | TmLet (r & {body = TmVarDecl _}) ->
  --   switch r.tyAnnot
  --   case TyUnknown _ then
  --     let msg = join [
  --       "* Encountered an untyped dependent variable: ",
  --       nameGetStr r.ident, "\n",
  --       "* When type checking the expression\n"
  --     ] in
  --     errorSingle [r.info] msg
  --   case ty then
  --     let inexpr = typeCheckExpr (_insertVar r.ident ty env) r.inexpr in
  --     TmLet {
  --       r with
  --       body = withType ty r.body,
  --       tyBody = ty,
  --       inexpr = inexpr,
  --       ty = tyTm inexpr
  --     }
  --   end

  -- Parse
  sem parseDAEExprExn : String -> Expr
  sem parseDAEExprExn =| str ->
    let t = parseMExprString {
      keywords = _daeAstKeywords,
      allowFree = true,
      builtin = concat builtin (adBuiltin ())
    } str
    in makeKeywords t

  sem parseDAEFileExn : String -> Expr
  sem parseDAEFileExn =| file ->
    let t =
      parseMCoreFile
        { _defaultBootParserParseMCoreFileArg () with
          keywords = _daeAstKeywords,
          allowFree = true,
          builtin = concat builtin (adBuiltin ())
        } file
    in makeKeywords t

  -- Monadic Shallow Maps/Folds

  -- sem smapAccumLM_Expr_Expr
  --   : all w1. all e1. all w2. all e2. all a.
  --     (a -> Expr -> (Result w1 e1 a, Result w2 e2 Expr))
  --      -> a
  --        -> Expr
  --          -> (Result w1 e1 a, Result w2 e2 Expr)

  -- sem smapAccumLM_Expr_Expr f acc =
  -- | t ->
  --   let inner = lam acc. lam t.
  --     match acc with (annotAcc, annotExpr, acc) in
  --     match f acc t with (resAcc, resExpr) in
  --     let acct =
  --       switch (result.consume resAcc, result.consume resExpr)
  --       case ((_, Right acc), (_, Right t)) then (acc, t)
  --       case ((_, Right acc), _) then (acc, t)
  --       case (_, _) then (acc, t)
  --       end
  --     in
  --     match acct with (acc, t) in
  --     ((result.withAnnotations resAcc annotAcc,
  --       result.withAnnotations resExpr annotExpr,
  --       acc),
  --      t)
  --   in
  --   match smapAccumL_Expr_Expr inner (result.ok (), result.ok (), acc) t
  --     with ((annotAcc, annotExpr, acc), t)
  --   in
  --   (result.withAnnotations annotAcc (result.ok acc),
  --    result.withAnnotations annotExpr (result.ok t))

  -- sem smapM_A_B smapAccumL f =| x ->
  --   let inner = lam annot. lam here.
  --     let res = f here in
  --     let here = match result.consume res with (_, Right x) then x else here in
  --     (result.withAnnotations res annot, here) in
  --   match #frozen"smapAccumL" inner (result.ok ()) x
  --     with (annot, res)
  --   in
  --   result.withAnnotations annot (result.ok res)

  sem smapM_DAEExpr_DAEExpr
    : all a. all b. (DAEExpr -> Result a b DAEExpr) -> DAEExpr -> Result a b DAEExpr
  sem smapM_DAEExpr_DAEExpr f =
  | expr ->
    let inner = lam annot. lam here.
      let res = f here in
      let here = match result.consume res with (_, Right x) then x else here in
      (result.withAnnotations res annot, here) in
    match smapAccumL_DAEExpr_DAEExpr inner (result.ok ()) expr
      with (annot, res)
    in
    result.withAnnotations annot (result.ok res)

  sem smapM_DAEProg_DAETop
    : all a. all b. (DAETop -> Result a b DAETop) -> DAEProg -> Result a b DAEProg
  sem smapM_DAEProg_DAETop f =
  | prog ->
    let inner = lam annot. lam here.
      let res = f here in
      let here = match result.consume res with (_, Right x) then x else here in
      (result.withAnnotations res annot, here) in
    match smapAccumL_DAEProg_DAETop inner (result.ok ()) prog
      with (annot, res)
    in
    result.withAnnotations annot (result.ok res)

  sem smapM_DAETop_DAEExpr
    : all a. all b. (DAEExpr -> Result a b DAEExpr) -> DAETop -> Result a b DAETop
  sem smapM_DAETop_DAEExpr f =
  | top ->
    let inner = lam annot. lam here.
      let res = f here in
      let here = match result.consume res with (_, Right x) then x else here in
      (result.withAnnotations res annot, here) in
    match smapAccumL_DAETop_DAEExpr inner (result.ok ()) top
      with (annot, res)
    in
    result.withAnnotations annot (result.ok res)

  sem smapM_DAEProg_DAEVar
    : all a. all b. (DAEVar -> Result a b DAEVar) -> DAEProg -> Result a b DAEProg
  sem smapM_DAEProg_DAEVar f =
  | prog ->
    let inner = lam annot. lam here.
      let res = f here in
      let here = match result.consume res with (_, Right x) then x else here in
      (result.withAnnotations res annot, here) in
    match smapAccumL_DAEProg_DAEVar inner (result.ok ()) prog
      with (annot, res)
    in
    result.withAnnotations annot (result.ok res)

  sem smapM_DAEVar_DAEExpr
    : all a. all b. (DAEExpr -> Result a b DAEExpr) -> DAEVar -> Result a b DAEVar
  sem smapM_DAEVar_DAEExpr f =
  | expr ->
    let inner = lam annot. lam here.
      let res = f here in
      let here = match result.consume res with (_, Right x) then x else here in
      (result.withAnnotations res annot, here) in
    match smapAccumL_DAEVar_DAEExpr inner (result.ok ()) expr
      with (annot, res)
    in
    result.withAnnotations annot (result.ok res)

  sem smapM_DAEProg_DAEEqn
    : all a. all b.
      (DAEEqn -> Result a b DAEEqn) -> DAEProg -> Result a b DAEProg
  sem smapM_DAEProg_DAEEqn f =
  | prog ->
    let inner = lam annot. lam here.
      let res = f here in
      let here = match result.consume res with (_, Right x) then x else here in
      (result.withAnnotations res annot, here) in
    match smapAccumL_DAEProg_DAEEqn inner (result.ok ()) prog
      with (annot, res)
    in
    result.withAnnotations annot (result.ok res)

  sem smapM_DAEEqn_DAEEqn
    : all a. all b.
      (DAEEqn -> Result a b DAEEqn) -> DAEEqn -> Result a b DAEEqn
  sem smapM_DAEEqn_DAEEqn f =
  | equation ->
    let inner = lam annot. lam here.
      let res = f here in
      let here = match result.consume res with (_, Right x) then x else here in
      (result.withAnnotations res annot, here) in
    match smapAccumL_DAEEqn_DAEEqn inner (result.ok ()) equation
      with (annot, res)
    in
    result.withAnnotations annot (result.ok res)

  sem smapM_DAEEqn_DAEExpr
    : all a. all b.
      (DAEExpr -> Result a b DAEExpr) -> DAEEqn -> Result a b DAEEqn
  sem smapM_DAEEqn_DAEExpr f =
  | equation ->
    let inner = lam annot. lam here.
      let res = f here in
      let here = match result.consume res with (_, Right x) then x else here in
      (result.withAnnotations res annot, here) in
    match smapAccumL_DAEEqn_DAEExpr inner (result.ok ()) equation
      with (annot, res)
    in
    result.withAnnotations annot (result.ok res)

  sem smapM_DAEProg_DAEProg
    : all a. all b. (DAEProg -> Result a b DAEProg) -> DAEProg -> Result a b DAEProg
  sem smapM_DAEProg_DAEProg f =
  | prog ->
    let inner = lam annot. lam here.
      let res = f here in
      let here = match result.consume res with (_, Right x) then x else here in
      (result.withAnnotations res annot, here) in
    match smapAccumL_DAEProg_DAEProg inner (result.ok ()) prog
      with (annot, res)
    in
    result.withAnnotations annot (result.ok res)
end

mexpr

()
