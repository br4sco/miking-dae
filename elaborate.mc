include "mexpr/ast-builder.mc"
include "mexpr/cmp.mc"
include "mexpr/pprint.mc"

include "./ast.mc"
include "./lib/elaboration.mc"
include "./dae/ast.mc"
include "./dae/ad.mc"

type EOOEdgeType
con EOOEdgeScalar : () -> EOOEdgeType
con EOOEdgeVector3 : () -> EOOEdgeType
con EOOEdgeSO3 : () -> EOOEdgeType
con EOOEdgeQuaternion : () -> EOOEdgeType

let eooEdgeTypeToString : EOOEdgeType -> String
  = lam e.
    switch e
    case EOOEdgeScalar _ then "ℝ"
    case EOOEdgeVector3 _ then "ℝ³"
    case EOOEdgeSO3 _ then "SO(3)"
    case EOOEdgeQuaternion _ then "Q"
    end

lang FlatEOO = Cmp + PrettyPrint
  type FlatEOORec = {
    bindings : Expr,
    vars : [(Name, Type)],
    ieqns : [Expr],
    eqns : [Expr],
    out : Expr,
    graphs : [{
      acrossType : EOOEdgeType,
      throughType : EOOEdgeType,
      graph : ElabGraph Symbol Expr Expr
    }]
  }

  sem flatEOOElabGraphEmpty =| () ->
    elabGraphEmpty (lam x. lam y. subi (sym2hash x) (sym2hash y)) cmpExpr cmpExpr

  sem flatEOOElabGraphToDot expr2str =| g ->
    elabGraphToDot
      (lam v. int2string (sym2hash v))
      (lam e. join ["(", expr2str e.0, ",", expr2str e.1, ")"])
      g.graph

  sem flatEOOToString : FlatEOORec -> String
  sem flatEOOToString =| eoo ->
    match
      mapAccumL
        (lam env. lam v.
          match pprintVarName env v.0 with (env, name) in
          (env, join [name, ":", type2str v.1]))
        pprintEnvEmpty eoo.vars
      with (env, vars)
    in
    let expr2str = exprToString env in
    strJoin "\n" [
      "bindings:",
      expr2str eoo.bindings,
      "\nvars:",
      strJoin ", " vars,
      "\nieqns:",
      strJoin ";\n" (map expr2str eoo.ieqns),
      "\neqns:",
      strJoin ";\n" (map expr2str eoo.eqns),
      "\nout:",
      expr2str eoo.out,
      "\ngraphs:",
      strJoin "\n"
        (map
           (lam g.
             strJoin "\n" [join [
               "edgeType: (",
               eooEdgeTypeToString g.acrossType,
               ",",
               eooEdgeTypeToString g.throughType,
               ")"
             ], flatEOOElabGraphToDot expr2str g])
           eoo.graphs)
    ]

  sem flatEOOElaborate : FlatEOORec -> FlatEOORec
  sem flatEOOElaborate =| eoo ->
    let rhs = lam eType. lam rhs.
      switch eType
      case EOOEdgeScalar _ then
        foldl
          (lam acc. lam t.
            match t with (c, t) in
            if eqi c 0 then acc
            else
              if eqi c 1 then addf_ acc t
              else
                if eqi c (negi 1) then subf_ acc t
                else error "expected only coefficients 0, 1 and -1")
          (float_ 0.)
          rhs
      case _ then error "unimplemented"
      end
    in
    foldl
      (lam eoo. lam g.
        let terms = (unzip g.graph.edges).1 in
        match elab terms g.graph with (circ, cut) in
        let circ = map (lam eqn. (eqn.0, rhs g.throughType eqn.1)) circ in
        let cut = map (lam eqn. (eqn.0, rhs g.acrossType eqn.1)) cut in
        let eqns =
          concat eoo.eqns (map (uncurry subf_) (concat circ cut))
        in
        { eoo with eqns = eqns })
      eoo eoo.graphs
end


lang EOOCoreElaborate = EOOCoreAst + FlatEOO + AD +
  -- Terms
  VarEval + AppEval + LamEval + LetEval + RecordEval + ConstEvalNoDefault +
  TypeEval + DataEval + SeqEval

  -- Constants
  + ArithIntEval + ShiftIntEval + ArithFloatEval + CmpIntEval + CmpFloatEval +
  SymbEval + CmpSymbEval + SeqOpEval + FloatIntConversionEval + CmpCharEval +
  IntCharConversionEval + FloatStringConversionEval

  -- Patterns
  + NamedPatEval + SeqTotPatEval + SeqEdgePatEval + RecordPatEval + DataPatEval +
  IntPatEval + CharPatEval + BoolPatEval + AndPatEval + OrPatEval + NotPatEval

  -- Pretty Printing
  + MExprPrettyPrint

  -- Dynamic Terms
  syn Expr =
  | TmDynamic Expr

  sem infoTm =
  | TmDynamic t -> infoTm t

  sem tyTm =
  | TmDynamic t -> tyTm t

  sem withInfo info =
  | TmDynamic t -> TmDynamic (withInfo info t)

  sem withType ty =
  | TmDynamic t -> TmDynamic (withType ty t)

  sem smapAccumL_Expr_Expr f acc =
  | TmDynamic t ->
    match smapAccumL_Expr_Expr f acc t with (acc, t) in (acc, TmDynamic t)

  sem flatten : Expr -> FlatEOORec
  sem flatten =| t ->
    let errorSingle = lam t.
      errorSingle [infoTm t] (strJoin "\n" ["Unable to flatten", expr2str t])
    in
    let symCmp = lam x. lam y. subi (sym2hash x) (sym2hash y) in
    let t = readback (eval (evalCtxEmpty ()) t) in
    recursive let gatherDynamicVariables = lam vars. lam t.
      match t with TmDVar r then
        mapInsert r.ident r.ty vars
      else sfold_Expr_Expr gatherDynamicVariables vars t
    in
    recursive let inner = lam acc. lam t.
      switch t
      case TmApp {
        lhs = TmApp {
          lhs = TmConst {val = CEqnf _},
          rhs = lhs},
        rhs = rhs}
      then
        { acc with eqns = cons (subf_ lhs rhs) acc.eqns }
      case TmApp {
        lhs = TmApp {
          lhs = TmConst { val = CIEqnf _},
          rhs = lhs},
        rhs = rhs}
      then
        { acc with ieqns = cons (subf_ lhs rhs) acc.ieqns }
      case TmApp {
        lhs = TmApp {
          lhs = TmApp {
            lhs = TmApp {
              lhs = TmApp {
                lhs = TmConst {val = CEdgeff _},
                rhs = TmConst {val = CSymb {val = dom}}},
              rhs = TmConst {val = CSymb {val = n1}}},
            rhs = TmConst {val = CSymb {val = n2}}},
          rhs = across},
        rhs = through}
      then
        let e = ((n1, n2), (through, across)) in
        let graphs =
          mapInsertWith
            (lam g1. lam g2. {
              g1 with graph = {
                g1.graph with edges = concat g1.graph.edges g2.graph.edges
              }})
            dom
            ({
              acrossType = EOOEdgeScalar (),
              throughType = EOOEdgeScalar (),
              graph = { elabGraphEmpty symCmp cmpExpr cmpExpr with edges = [e] }
            })
            acc.graphs
        in
        { acc with graphs = graphs }
      case t then errorSingle t
      end
    in
    match t with TmRecord r then
      match
        (mapLookup (stringToSid "0") r.bindings,
         mapLookup (stringToSid "1") r.bindings)
        with
        (Some (TmSeq r), Some out)
      then
        let vars = mapBindings (gatherDynamicVariables (mapEmpty nameCmp) t) in
        let acc =
          foldl inner { ieqns = [], eqns = [], graphs = mapEmpty symCmp } r.tms
        in
        {
          bindings = unit_,
          vars = vars,
          ieqns = acc.ieqns,
          eqns = acc.eqns,
          out = out,
          graphs = mapValues acc.graphs
        }
      else errorSingle t
    else errorSingle t

  sem eval ctx =
  | TmDynamic t -> TmDynamic t
  | TmNever r ->  TmDynamic (TmNever r)
  | TmDVar r -> TmDynamic (TmDVar r)
  | TmMatch r ->
    let target = eval ctx r.target in
    match tryMatch ctx.env target r.pat with Some newEnv then
      eval { ctx with env = newEnv } r.thn
    else
      match target with TmDynamic target then
        TmDynamic (TmMatch { r with target = target })
      else eval ctx r.els

  sem apply ctx info =
  | (TmDynamic t, arg) -> TmDynamic (app_ t arg)

  sem delta info =
  | (CGenDynVarf _, [TmSeq r]) ->
    let t = TmDVar {
      ident = nameSym (_evalSeqOfCharsToString r.info r.tms),
      ty = tyfloat_,
      order = 0,
      info = info
    } in
    TmDynamic t
  | (const, args) ->
    if lti (length args) (constArity const) then
      -- Accumulate arguments if still not a complete application
      TmConstApp {const = const, args = args, info = info}
    else
      -- No available pattern so we treat this constant application as dynamic
      TmDynamic (TmConstApp { const = const, args = args, info = info })

  sem tryMatch (env : EvalEnv) (t : Expr) =| _ -> None ()

  sem readback : Expr -> Expr
  sem readback =
  | TmDynamic (TmConstApp (r & {
    const = CDotf _, args = [TmConst {val = CInt {val = n}}, t]
  })) ->
    if lti n 0 then error "Invalid application"
    else
      if eqi n 0 then readback t
      else
        readback (eval (evalCtxEmpty ()) (tupleproj_ n (ad n (readback t))))
  | TmDynamic t -> readback t
  | TmConstApp r -> appSeq_ (uconst_ r.const) (map readback r.args)
  | TmClos r ->
    let newident = nameSetNewSym r.ident in
    let newvar = TmVar {
      ident = newident,
      ty = tyunknown_,
      info = r.info,
      frozen = false
    } in
    let ctx = evalCtxEmpty () in
    let body =
      readback
        (eval
           { ctx with env = evalEnvInsert r.ident (TmDynamic newvar) (r.env ()) }
           r.body)
    in
    nulam_ newident body
  | TmMatch r ->
    match readbackPat (evalEnvEmpty ()) r.pat with (env, pat) in
    let ctx = evalCtxEmpty () in
    TmMatch {
      r with
      target = readback r.target,
      pat = pat,
      thn = readback (eval { ctx with env = env } r.thn),
      els = readback (eval ctx r.els)
    }
  | t -> smap_Expr_Expr readback t

  sem readbackPat : EvalEnv -> Pat -> (EvalEnv, Pat)
  sem readbackPat env =
  | PatNamed (r & {ident = PName name}) ->
    let newname = nameSetNewSym name in
    let newvar = TmVar {
      ident = newname,
      ty = r.ty,
      info = r.info,
      frozen = false
    } in
    (evalEnvInsert name (TmDynamic newvar) env,
     PatNamed { r with ident = PName newname })
  | p -> smapAccumL_Pat_Pat readbackPat env p
end
