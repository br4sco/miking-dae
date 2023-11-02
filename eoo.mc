/-
  This File implements a Equation-Based Object-Oriented intermediate language and
  its transformation to a DAE program.
 -/

include "elaboration.mc"
include "ast.mc"


type EOOGraphKind
con EOOGraphScalar : () -> EOOGraphKind
con EOOGraphPlanarMech : () -> EOOGraphKind

lang EOOBase = DAEAst
  type EOORec a = {
    bindings : Expr,
    vars : [(Name, Type)],
    eqns : [Expr],
    graphs : [(EOOGraphKind, ElabGraph a (Name, Int) (Name, Int))],
    info : Info
  }

  sem symbolizeEOO : all a. SymEnv -> EOORec a -> EOORec a
  sem symbolizeEOO env =| r ->
    let bindings = symbolizeExpr env r.bindings in
    let newenv = addTopNames env bindings in
    match
      mapAccumL
        (lam env. lam v.
          match v with (id, ty) in
          match symbolizeTyAnnot env ty with (tyVarEnv, ty) in
          match setSymbol env.varEnv id with (varEnv, id) in
          ({ env with varEnv = varEnv }, (id, ty)))
        env r.vars
      with (varenv, vars)
    in
    let varsSet = setOfSeq nameCmp (unzip vars).0 in
    let isDependentVar = lam ident. setMem ident varsSet in
    let graphs =
      map
        (lam g.
          let dg = elabGraphGetDigraph g in
          let es =
            map
              (lam e.
                match e with (v1, v2, ((xa, na), (xt, nt))) in
                let getSymbol = lam kind. lam ident.
                  getSymbol {
                    kind = (concat kind " variable"),
                    info = [r.info],
                    allowFree = env.allowFree
                  } env.varEnv ident
                in
                let xa = getSymbol "across" xa in
                let xt = getSymbol "through" xt in
                if and (isDependentVar xa) (isDependentVar xt) then
                  (v1, v2, ((xa, na), (xt, nt)))
                else
                  errorSingle [r.info]
                    "Only dependent variables may occur as across or through variables")
              (digraphEdges dg)
          in
          let dg = digraphAddEdges (digraphRemoveEdges dg) es in
          (g.0, elabGraphSetDigraph g.1 dg))
        r.graphs
    in
    {
      r with
      bindings = bindings,
      vars = vars,
      eqns = map (symbolizeExpr env) r.eqns,
      graphs = graphs
    }

  sem typeCheckEOO : TCEnv -> EOORec -> EOORec
  sem typeCheckEOO env =| r ->
    let bindings = typeCheckExpr env r.bindings in
    let env = addTopTypes env bindings in
    match
      mapAccumL
        (lam env. lam v.
          match v with (id, ty) in
          let ty = resolveType r.info env.tyConEnv ty in
          (_insertVar id ty env, (id, ty)))
        env
        r.vars
      with (env, vars)
    in
    -- TODO(oerikss, 2023-08-31): Make sure residuals are scalars
    let eqns = map (typeCheckExpr env) r.eqns in
    {
      r with
      bindings = bindings,
      vars = vars,
      eqns = eqns
    }
end

lang EOOElab = EOOBase

end
