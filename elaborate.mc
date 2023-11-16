/-

  This file contains the implementation of elaboration of EOO models. The
  elaboration consists of two steps, flattening and graph elaboration.

  - Flattening means transforming a EOO program to a seqence of equations and
    graphs thats encodes the topology of the model (i.e. which components are
    connected in the EOO model).

  - Graph elaboration means taking the model graphs and producing additional
    equations based on energy conservation laws. Together with the equations
    already present, these form a consistent DAE if the elaboration is
    successful.

-/

include "mexpr/ast-builder.mc"
include "mexpr/cmp.mc"
include "mexpr/pprint.mc"

include "./ast.mc"
include "./lib/elaboration.mc"  -- Algorithms and data structures related to the
                                -- graph elaboration lives here
include "./dae/ast.mc"
include "./dae/ad.mc"

-- The graph elaboration dependes type of the across and through quantity
-- associated with a particular edge.
type EOOQuantityType
con EOOQuantityScalar : () -> EOOQuantityType
con EOOQuantityVector3 : () -> EOOQuantityType
con EOOQuantitySO3 : () -> EOOQuantityType -- rotational matrices
con EOOQuantityQuaternion : () -> EOOQuantityType

let eooQuantityTypeToString : EOOQuantityType -> String
  = lam e.
    switch e
    case EOOQuantityScalar _ then "ℝ"
    case EOOQuantityVector3 _ then "ℝ³"
    case EOOQuantitySO3 _ then "SO(3)"
    case EOOQuantityQuaternion _ then "Q"
    end

-- Intermediate representation and operations on a flat EOO model
lang FlatEOO = Cmp + PrettyPrint

  -- An EOO model graph consists of an elaboration graph and the types of its
  -- across and through quantities.
  type EOOGraph = {
    acrossType : EOOQuantityType,
    throughType : EOOQuantityType,
    graph : ElabGraph Symbol Expr Expr
  }

  -- This datastructure encodes the representation of a flat EOO model
  type FlatEOO = {
    bindings : Expr,
    vars : [(Name, Type)],
    ieqns : [Expr],
    eqns : [Expr],
    out : Expr,
    graphs : [EOOGraph]
  }

  sem flatEOOElabGraphEmpty : () -> ElabGraph Symb Expr Expr
  sem flatEOOElabGraphEmpty =| () ->
    elabGraphEmpty (lam x. lam y. subi (sym2hash x) (sym2hash y)) cmpExpr cmpExpr

  sem flatEOOElabGraphEdgeLabelToString : (Expr, Expr) -> String
  sem flatEOOElabGraphEdgeLabelToString =| e ->
    join ["(", expr2str e.0, ",", expr2str e.1, ")"]

  sem flatEOOElabGraphToDot : ElabGraph Symbol Expr Expr -> String
  sem flatEOOElabGraphToDot =| g ->
    elabGraphToDot
      (lam v. int2string (sym2hash v))
      flatEOOElabGraphEdgeLabelToString
      g

  sem flatEOOToString : FlatEOO -> String
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
               "quantityType: (",
               eooQuantityTypeToString g.acrossType,
               ",",
               eooQuantityTypeToString g.throughType,
               ")"
             ], flatEOOElabGraphToDot g.graph])
           eoo.graphs)
    ]

  -- Elaborates all model graphs and augments the equations of the EOO with the
  -- resulting balance equations.
  sem eooElaborateGraphs : FlatEOO -> FlatEOO
  sem eooElaborateGraphs =| eoo ->
    let rhs = lam qType. lam rhs.
      switch qType
      case EOOQuantityScalar _ then
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
        logDebug
          (lam. strJoin "\n" [
            "IM Matrix Column labels",
            strJoin "," (map flatEOOElabGraphEdgeLabelToString terms)
          ]);
        match elab terms g.graph with (cut, circ, vertices) in
        logDebug
          (lam.
            strJoin "\n" [
              "Vertices:",
              strJoin "," (map (lam v. int2string (sym2hash v)) vertices),
              "F-Cutset Equations:",
              strJoin "\n" (map (elabEquationScalarToString expr2str) cut),
              "F-Circutset Equations:",
              strJoin "\n" (map (elabEquationScalarToString expr2str) circ)
            ]);
        let cut = map (lam eqn. (eqn.0, rhs g.throughType eqn.1)) cut in
        let circ = map (lam eqn. (eqn.0, rhs g.acrossType eqn.1)) circ in

        let eqns =
          concat eoo.eqns (map (uncurry subf_) (concat cut circ))
        in
        { eoo with eqns = eqns, graphs = [] })
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

  sem containsDynamic : Bool -> Expr -> Bool
  sem containsDynamic acc =
  | TmDynamic _ -> true
  | t -> sfold_Expr_Expr containsDynamic acc t

  -- Collects all dynamic variable names that appear in an
  -- expression. Internally calls `eooGatherDynVarsExpr` which is suitable for
  -- extension.
  sem eooGatherDynVars : Expr -> Map Name Type
  sem eooGatherDynVars =| t ->
    eooGatherDynVarsExpr (mapEmpty nameCmp) t

  sem eooGatherDynVarsExpr : Map Name Type -> Expr -> Map Name Type
  sem eooGatherDynVarsExpr vars =
  | TmDVar r -> mapInsert r.ident r.ty vars
  | t -> sfold_Expr_Expr eooGatherDynVarsExpr vars t

  type EqnsAndEdgesAcc =
    {ieqns : [Expr], eqns : [Expr], graphs : Map Symbol EOOGraph}

  -- Gathers an equation or an edge from a single expression. Returns `None ()`
  -- if the expression does not encode neither an equation nor an edge.
  sem eooGatherEqnsAndEdges : EqnsAndEdgesAcc -> Expr -> Option EqnsAndEdgesAcc
  sem eooGatherEqnsAndEdges acc =
  | TmApp {
    lhs = TmApp {
      lhs = TmConst {val = CEqnf _},
      rhs = lhs},
    rhs = rhs}
    -> Some { acc with eqns = cons (subf_ lhs rhs) acc.eqns }
  | TmApp {
    lhs = TmApp {
      lhs = TmConst { val = CIEqnf _},
      rhs = lhs},
    rhs = rhs}
    -> Some { acc with ieqns = cons (subf_ lhs rhs) acc.ieqns }
  | TmApp {
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
    ->
    let symCmp = lam x. lam y. subi (sym2hash x) (sym2hash y) in
    let e = ((n1, n2), (through, across)) in
    let graphs =
      mapInsertWith
        (lam g1. lam g2. {
          g1 with graph = {
            g1.graph with edges = concat g1.graph.edges g2.graph.edges
          }})
        dom
        ({
          acrossType = EOOQuantityScalar (),
          throughType = EOOQuantityScalar (),
          graph = { elabGraphEmpty symCmp cmpExpr cmpExpr with edges = [e] }
        })
        acc.graphs
    in
    Some { acc with graphs = graphs }
  | _ -> None ()

  -- Produces a flat EOO IR from an EOO program by partially evaluating the
  -- input expression. The input expression must be of type
  -- `([Equation], a)` and the partial evaluation must result in an expression
  -- `([e1, ..., en], e)`, where `e1` to `en` encodes either equations or edges.
  --
  -- Produces an error if the flattening is unsuccessful. This can occur, e.g.,
  -- if branch predicates involves dynamic variables.
  sem eooFlatten : Expr -> FlatEOO
  sem eooFlatten =| t ->
    let errorSingle = lam t.
      errorSingle [infoTm t] (strJoin "\n" ["Unable to flatten"])
    in
    let symCmp = lam x. lam y. subi (sym2hash x) (sym2hash y) in
    -- Partially evaluate the input expression
    let t = readback (eval (evalCtxEmpty ()) t) in
    -- We now expect the residual program to be on the form:
    -- `([e1, ..., en], e)`, where `e1` to `en` encodes either equations or
    -- edges.
    match t with TmRecord r then
      match
        (mapLookup (stringToSid "0") r.bindings,
         mapLookup (stringToSid "1") r.bindings)
        with
        (Some (TmSeq r), Some out)
      then
        let vars = mapBindings (eooGatherDynVars t) in
        let acc =
          foldl
            (lam acc. lam t.
              optionGetOrElse (lam. errorSingle t) (eooGatherEqnsAndEdges acc t))
            { ieqns = [], eqns = [], graphs = mapEmpty symCmp } r.tms
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

  -- The MExpr evaluator extended with dynamic terms that allows partial
  -- evaluation of a subset of pure MCore. The implementation uses the
  -- eval/readback approach of Benjamin Grégoire and Xavier Leroy. 2002. A
  -- compiled implementation of strong reduction
  -- (https://doi.org/10.1145/581478.581501). Notably the implementation does
  -- not allow recursion to guarantee termination.
  --
  -- Moreover, the partial evaluation is extended to perform symbolic
  -- differentation of applications of the `CDotf` constant by partially
  -- evaluating a forward mode AD source-code transformation.
  --
  -- Note that this partial evaluator does not preserve sharing to simplify the
  -- structural analysis needed by the DAE compilation. In typical EOO program
  -- the `CDotf` constant is applied directly on dynamic variables and therefore
  -- expression swell that results from symbolic differentation in this manner
  -- is not major concern.
  sem eval ctx =
  | TmDynamic t -> TmDynamic t
  | TmNever r ->  TmDynamic (TmNever r)
  | TmDVar r -> TmDynamic (TmDVar r)
  | TmMatch r ->
    let target = eval ctx r.target in
    match tryMatch ctx.env target r.pat with Some newEnv then
      eval { ctx with env = newEnv } r.thn
    else
      if containsDynamic false r.target then
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
  -- NOTE(oerikss, 2023-11-11): We place the symbolic differentation here to not
  -- make eval and readback mutally recursive.
  | TmDynamic (TmConstApp (r & {
    const = CDotf _, args = [TmConst {val = CInt {val = n}}, t]
  })) ->
    if lti n 0 then errorSingle [r.info] "Invalid application"
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
  -- NOTE(oerikss, 2023-11-11): Here we take a conservative approach and
  -- consider all bound variables in a dynamic match dynamic. A more granular
  -- implementation can treat a subset of the matched expressions as static and
  -- update the environment accordingly.
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
