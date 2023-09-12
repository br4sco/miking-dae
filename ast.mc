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
include "tuple.mc"

include "./ast_gen.mc"
include "./built-in.mc"
include "../miking-pead/ad.mc"

type Res a = Result ErrorSection ErrorSection a

let _daeAstKeywords = ["prim"]

let dvarCmp = tupleCmp2 nameCmp subi

lang DAEAst = DAEParseAst + AstResult +
  MExprSym + MExprEq + MExprPrettyPrint + MExprTypeCheck + AD +
  BootParser + KeywordMaker

  type TmDVarRec = {
    ident : Name,
    order : Int,
    ty : Type,
    info : Info
  }

  type TmDAERec = {
    bindings : Expr,
    vars : [(Name, Type)],
    ieqns : [Expr],
    eqns : [Expr],
    out : Expr,
    info : Info
  }

  syn Expr =
  -- x^(n) in MExpr
  | TmDVar TmDVarRec
  | TmDAE TmDAERec

  sem tmVarRecToTmDVarRec
    : TmDVarRec -> ({ident : Name, ty : Type, info : Info, frozen : Bool}, Int)
  sem tmVarRecToTmDVarRec =
  | r -> ({ ident = r.ident, ty = r.ty, info = r.info, frozen = false }, r.order)

  sem tmDVarRecToTmVarRec
    : Int -> {ident : Name, ty : Type, info : Info, frozen : Bool} -> TmDVarRec
  sem tmDVarRecToTmVarRec order =| r ->
    { ident = r.ident, ty = r.ty, info = r.info, order = order }

  sem _tmDAERecToTm : TmDAERec -> Expr
  sem _tmDAERecToTm =| r ->
    bind_ r.bindings
      (nlams_ r.vars
         (urecord_ [
           ("ieqns", utuple_ r.ieqns),
           ("eqns", utuple_ r.eqns),
           ("out", r.out)
         ]))

  sem _tmToTmDAERec : Expr -> TmDAERec
  sem _tmToTmDAERec =
  | t -> _tmToTmDAERecH {
    bindings = bind_ t unit_,
    vars = [],
    ieqns = [],
    eqns = [],
    out = unit_,
    info = infoTm t
  } t

  sem _tmToTmDAERecErrMsg msg =| t ->
    errorSingle [infoTm t]
      (strJoin "\n" [join ["daeExprToDAERec: ", msg, ":"], expr2str t])

  sem _tmToTmDAERecH : TmDAERec -> Expr -> TmDAERec
  sem _tmToTmDAERecH dae =
  | TmLet r -> _tmToTmDAERecH dae r.inexpr
  | TmRecLets r -> _tmToTmDAERecH dae r.inexpr
  | TmLam (r & {body = TmLam _}) ->
    let dae = { dae with vars = snoc dae.vars (r.ident, r.tyParam) } in
    _tmToTmDAERecH dae r.body
  | TmLam (lamr & {body = t & TmRecord r}) ->
    let dae = { dae with vars = snoc dae.vars (lamr.ident, lamr.tyParam) } in
    switch
      map
        (lam l. mapLookup (stringToSid l) r.bindings)
        ["ieqns", "eqns", "out"]
    case [Some (TmRecord ieqns), Some (TmRecord eqns), Some out] then
      match
        if mapIsEmpty ieqns.bindings then [Some [], record2tuple eqns.bindings]
        else
          map (lam r. record2tuple r.bindings) [ieqns, eqns] with
        [Some ieqns, Some eqns]
      then
        {
          dae with
          ieqns = ieqns,
          eqns = eqns,
          out = out
        }
      else _tmToTmDAERecErrMsg "Malformed record" t
    case _ then _tmToTmDAERecErrMsg "Malformed lambda body" t
    end
  | t -> _tmToTmDAERecErrMsg "Malformed expression" t

  -- Accessors
  sem infoTm =
  | TmDVar r -> r.info
  | TmDAE r -> r.info

  sem tyTm =
  | TmDVar r -> r.ty
  | TmDAE r -> tyTm r.out

  sem withInfo info =
  | TmDVar r -> TmDVar { r with info = info }
  | TmDAE r -> TmDAE { r with info = info }

  sem withType ty =
  | TmDVar r -> TmDVar { r with ty = ty }
  | TmDAE r -> TmDAE { r with out = withType ty r.out }

  -- Shallow map/fold
  sem smapAccumL_Expr_Expr f acc =
  | TmDAE r ->
    match f acc r.bindings with (acc, bindings) in
    match mapAccumL f acc r.ieqns with (acc, ieqns) in
    match mapAccumL f acc r.eqns with (acc, eqns) in
    match f acc r.out with (acc, out) in
    (acc,
     TmDAE {
       r with bindings = bindings, ieqns = ieqns, eqns = eqns, out = out
     })

  -- Dependet variables
  sem dvarsExpr : Set (Name, Int) -> Expr -> Set (Name, Int)
  sem dvarsExpr dvars =
  | TmDVar r -> setInsert (r.ident, r.order) dvars
  | t -> sfold_Expr_Expr dvarsExpr dvars t

  sem dvars : Expr -> Set (Name, Int)
  sem dvars =| t -> dvarsExpr (setEmpty (tupleCmp2 nameCmp subi)) t

  -- AD
  sem adExprH ctx n =
  | TmDVar r ->
    let b = peadAstBuilder r.info in
    b.taylorcoef
      (create (succ n) (lam i. TmDVar { r with order = addi r.order i }))

  -- PEval
  sem pevalBindThis =
  | TmDVar _ -> false

  sem pevalEval ctx k =
  | TmDVar r -> k (TmDVar r)

  -- KeywordMaker
  sem isKeyword =
  | TmDVar _ -> true

  sem matchKeywordString (info : Info) =
  | "prim" ->
    Some (2, lam lst.
      match lst with [TmConst { val = CInt { val = order }}, TmVar r] then
        TmDVar (tmDVarRecToTmVarRec order r)
      else errorSingle [info] "Invalid application use prim")


  -- Eq
  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmDVar r ->
    match lhs with TmDVar l then
      match (tmVarRecToTmDVarRec l, tmVarRecToTmDVarRec r)
        with
        ((lvarr, lorder),  (rvarr, rorder))
      in
      if eqi lorder rorder then
        eqExprH env free (TmVar lvarr) (TmVar rvarr)
      else None ()
    else None ()
  | TmDAE r ->
    match lhs with TmDAE l then
      eqExprH env free (_tmDAERecToTm l) (_tmDAERecToTm r)
    else None ()

  -- Cmp
  sem cmpExprH =
  | (TmDVar l, TmDVar r) ->
    match (tmVarRecToTmDVarRec l, tmVarRecToTmDVarRec r)
      with ((l, lorder), (r, rorder))
    in
    if eqi lorder rorder then cmpExprH (TmVar l, TmVar r)
    else subi lorder rorder
  | (TmDAE r, TmDAE l) -> cmpExprH (_tmDAERecToTm l, _tmDAERecToTm r)

  -- PrettyPrint
  sem isAtomic =
  | TmDVar _ -> false
  | TmDAE _ -> false

  sem pprintCode (indent : Int) (env : PprintEnv) =
  | TmDVar r ->
    match tmVarRecToTmDVarRec r with (varr, order) in
    match pprintCode indent env (TmVar varr) with (env, var) in
    (env, strJoin " " ["prim", int2string order, var])
  | TmDAE r -> pprintCode indent env (_tmDAERecToTm r)

  -- Sym
  sem symbolizeExpr (env : SymEnv) =
  | TmDVar r ->
    match tmVarRecToTmDVarRec r with (r, order) in
    match symbolizeExpr env (TmVar r) with TmVar r then
      TmDVar (tmDVarRecToTmVarRec order r)
    else error "Impossible"
  | TmDAE r -> TmDAE (_tmToTmDAERec (symbolizeExpr env (_tmDAERecToTm r)))

  sem daeSymbolize : Expr -> Expr
  sem daeSymbolize =| t ->
    adSymbolizeExpr { symEnvEmpty with varEnv = daeBuiltins } t

  -- Type Check
  sem typeCheckExpr env =
  | TmDVar r ->
    match tmVarRecToTmDVarRec r with (varr, order) in
    match typeCheckExpr env (TmVar varr) with TmVar varr then
      TmDVar (tmDVarRecToTmVarRec order varr)
    else error "impossible"
  | TmDAE r -> TmDAE (_tmToTmDAERec (typeCheckExpr env (_tmDAERecToTm r)))

  sem daeTypeCheck : Expr -> Expr
  sem daeTypeCheck =| t ->
    typeCheckExpr { _tcEnvEmpty with varEnv = daeTCVarEnv } t

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
