include "name.mc"

include "mexpr/ast-builder.mc"

include "./ast_gen.mc"
include "./ast.mc"

let _tmLet = lam i. lam n. lam ty. lam b. lam ie. use MExprAst in TmLet {
  ident = n,
  tyAnnot = ty,
  tyBody = tyunknown_,
  body = b,
  inexpr = ie,
  ty = tyunknown_,
  info = i
}

let _tmLam = lam i. tmLam i tyunknown_

let _tmApp = lam i. tmApp i tyunknown_

let _tmApp2 = lam i. lam f. lam a. lam b. _tmApp i (_tmApp i f a) b

let _tmApps = lam i. foldl1 (_tmApp i)

let _tmEqn = lam info. lam c. lam l. lam r.
  use MExprAst in withInfo info (seq_ [_tmApps info [(uconst_ c), l, r]])

lang EOODesugar = EOOCoreAst + EOOAst
  sem eooDesugar : EOOProg -> Expr
  sem eooDesugar =
  | ProgEOOProg r ->
    foldr bind_
      (foldl eooDSModelHead
         (tmTuple r.info tyunknown_
            [eooDSModelEquation r.eqnkw r.eqn, eooDSExpr r.output])
         (reverse r.heads))
      (map eooDSTop r.tops)

  sem eooDSModelHead : Expr -> EOOModelHead -> Expr
  sem eooDSModelHead body =
  | VarEOOModelHead r ->
    foldl
      (lam body. lam n.
        let rhs =
          _tmApp n.i
            (withInfo r.info (uconst_ (CGenDynVarf ())))
            (withInfo r.info (str_ (nameGetStr n.v)))
        in
        _tmLet n.i n.v (eooDSType r.ty) rhs body)
      body r.ns
  | NodeEOOModelHead r ->
    foldl
      (lam body. lam n.
        let rhs =
          _tmApp n.i
            (withInfo r.info (uconst_ (CGensym ())))
            (withInfo r.info (unit_))
        in
        _tmLet n.i n.v tyunknown_ rhs body)
      body r.ns
  | DefEOOModelHead r -> eooDSDefBinding body r.binding
  | ModelEOOModelHead r -> eooDSModelBinding body r.binding

  sem eooDSModelEquation : Info -> EOOExpr -> Expr
  sem eooDSModelEquation info =| eqn ->
    let ident = nameSym "eqn" in
    _tmLet info ident (tyseq_ tyequation_)
      (eooDSExpr eqn) (withInfo info (nvar_ ident))

  sem eooDSTop : EOOTop -> Expr
  sem eooDSTop =
  | DefEOOTop r -> eooDSDefBinding (unit_) r.binding
  | ModelEOOTop r -> eooDSModelBinding (unit_) r.binding
  | TypeEOOTop r -> TmType {
    ident = r.n.v,
    params = map (lam p. p.v) r.params,
    tyIdent = eooDSType r.rhs,
    inexpr = unit_,
    ty = tyunknown_,
    info = r.info
  }

  sem eooDSParamF : (Info -> Name -> Expr -> Expr) -> Expr -> EOOParam -> Expr
  sem eooDSParamF f body =
  | NameEOOParam r ->
    f r.info r.n.v body
  | IgnoreEOOParam r ->
    f r.info (nameNoSym "") body
  | PatEOOParam r ->
    switch r.pat
    case WildEOOPat _ then
      f r.info (nameNoSym "") body
    case BindEOOPat pr then
      f r.info pr.n.v body
    case pat then
      let ident = nameSym "x" in
      f r.info ident
        (TmMatch {
          target = withInfo r.info (nvar_ ident),
          pat = eooDSPat pat,
          thn = body,
          els = withInfo r.info never_,
          ty = tyunknown_,
          info = r.info
        })
    end

  sem eooDSDefBinding : Expr -> EOODefBinding -> Expr
  sem eooDSDefBinding body =
  | SimpleEOODefBinding r ->
    let rhs =
      foldl (eooDSParamF (lam i. lam n. _tmLam i n tyunknown_))
        (eooDSExpr r.rhs) (reverse r.params)
    in
    if optionMapOr true (lam n2. nameEq r.n.v n2.v) r.n2 then
      _tmLet r.info r.n.v (optionMapOr tyunknown_ eooDSType r.ty) rhs body
    else
      errorSingle [r.n.i]
        "Signature and definition names do not match"

  sem eooDSModelBinding : Expr -> EOOModelBinding -> Expr
  sem eooDSModelBinding body =
  | SimpleEOOModelBinding r ->
    let rhs = ModelEOOExpr {
      heads = r.heads, eqnkw = r.eqnkw, eqn = r.eqn, info = r.info
    } in
    eooDSDefBinding body
      (SimpleEOODefBinding {
        n = r.n, ty = r.ty, n2 = r.n2, params = r.params, rhs = rhs,
        info = r.info
      })

  sem eooDSExpr : EOOExpr -> Expr
  sem eooDSExpr =
  | VarEOOExpr r -> withInfo r.info (nvar_ r.n.v)
  | AbsEOOExpr r ->
    eooDSParamF (lam i. lam n. _tmLam i n (optionMapOr tyunknown_ eooDSType r.ty))
      (eooDSExpr r.body) r.param
  | AppEOOExpr r -> _tmApp r.info (eooDSExpr r.left) (eooDSExpr r.right)
  | NumEOOExpr r ->
    let val =
      switch (r.f, r.i)
      case (Some f, _) then CFloat { val = f.v }
      case (_, Some i) then CInt { val = i.v }
      case _ then error "impossible"
      end
    in
    TmConst { val = val, ty = tyunknown_, info = r.info }
  | StringEOOExpr r -> withInfo r.info (str_ r.v.v)
  | TrueEOOExpr r -> withInfo r.info true_
  | FalseEOOExpr r -> withInfo r.info false_
  | UnitEOOExpr r -> withInfo r.info unit_
  | e & TupEOOExpr r ->
    recursive let gather = lam es. lam e.
      match e with TupEOOExpr r then
        gather (cons (eooDSExpr r.right) es) r.left
      else cons (eooDSExpr e) es
    in
    tmTuple r.info tyunknown_ (gather [] e)
  | ConcatEOOExpr r ->
    _tmApp2 r.info
      (withInfo r.op (uconst_ (CConcat ())))
      (eooDSExpr r.left)
      (eooDSExpr r.right)
  | ConsEOOExpr r ->
    _tmApp2 r.info
      (withInfo r.op (uconst_ (CCons ())))
      (eooDSExpr r.left)
      (eooDSExpr r.right)
  | SeqEOOExpr r ->
    recursive let gather = lam es. lam e.
      match e with TupEOOExpr r then
        gather (cons (eooDSExpr r.right) es) r.left
      else cons (eooDSExpr e) es
    in
    let tms = optionMapOr [] (gather []) r.unbrokenElems in
    TmSeq { tms = tms, ty = tyunknown_, info = r.info }
  | LetEOOExpr r ->
    let rhs = eooDSExpr r.rhs in
    eooDSParamF
      (lam i. lam n. _tmLet i n tyunknown_ rhs)
      (eooDSExpr r.body)
      (PatEOOParam { pat = r.pat, info = r.info })
  | DefEOOExpr r ->
    eooDSDefBinding (eooDSExpr r.body) r.binding
  | IfEOOExpr r ->
    withInfo r.info (if_ (eooDSExpr r.c) (eooDSExpr r.t) (eooDSExpr r.e))
  | MatchingEOOExpr r ->
    let scrut = nameSym "scrut" in
    let scrutE = withInfo (get_EOOExpr_info r.scrut) (nvar_ scrut) in
    let scrutLet = nulet_ scrut (eooDSExpr r.scrut) in
    let mkArm = lam arm. lam cont.
      match_ scrutE (eooDSPat arm.pat) (eooDSExpr arm.body) cont
    in
    _tmLet r.info scrut tyunknown_
      (eooDSExpr r.scrut)
      (foldr mkArm never_ r.arms)
  | AddfEOOExpr r ->
    _tmApp2 r.info
      (withInfo r.op (uconst_ (CAddf ())))
      (eooDSExpr r.left)
      (eooDSExpr r.right)
  | SubfEOOExpr r ->
    _tmApp2 r.info
      (withInfo r.op (uconst_ (CSubf ())))
      (eooDSExpr r.left)
      (eooDSExpr r.right)
  | MulfEOOExpr r ->
    _tmApp2 r.info
      (withInfo r.op (uconst_ (CMulf ())))
      (eooDSExpr r.left)
      (eooDSExpr r.right)
  | DivfEOOExpr r ->
    _tmApp2 r.info
      (withInfo r.op (uconst_ (CDivf ())))
      (eooDSExpr r.left)
      (eooDSExpr r.right)
  | EqnEOOExpr r ->
    _tmEqn r.info (CEqnf ()) (eooDSExpr r.left) (eooDSExpr r.right)
  | InitEOOExpr r ->
    match r.right with EqnEOOExpr r then
      _tmEqn r.info (CIEqnf ()) (eooDSExpr r.left) (eooDSExpr r.right)
    else
      errorSingle
        [r.info, get_EOOExpr_info r.right]
        "Parse Error: Expected an initial equation"
  | e & PrimEOOExpr r ->
    recursive let order = lam n. lam e.
      match e with PrimEOOExpr r then order (addi n 1) r.left else (n, e)
    in
    match order 0 e with (n, e) in
    _tmApp2 r.info
      (withInfo r.info (uconst_ (CDotf ())))
      (withInfo r.info (int_ n))
      (eooDSExpr e)
  | ConnectEOOExpr r ->
    withInfo r.info (seq_ [_tmApps r.info [
      withInfo r.info (uconst_ (CEdgeff ())),
      eooDSExpr r.dom,
      eooDSExpr r.from,
      eooDSExpr r.to,
      eooDSExpr r.across,
      eooDSExpr r.through
    ]])
  | ModelEOOExpr r ->
    foldl eooDSModelHead (eooDSModelEquation r.eqnkw r.eqn) (reverse r.heads)

  sem eooDSPat : EOOPat -> Pat
  sem eooDSPat =
  | WildEOOPat r -> withInfoPat r.info pvarw_
  | BindEOOPat r -> withInfoPat r.info (npvar_ r.n.v)
  | UnitEOOPat r -> withInfoPat r.info punit_
  | p & TupEOOPat r ->
    recursive let gather = lam ps. lam p.
      match p with TupEOOPat r then
        gather (cons (eooDSPat r.right) ps) r.left
      else cons (eooDSPat p) ps
    in
    patTuple (gather [] p) r.info
  | SeqEOOPat r ->
    recursive let gather = lam ps. lam p.
      match p with TupEOOPat r then
        gather (cons (eooDSPat r.right) ps) r.left
      else cons (eooDSPat p) ps
    in
    withInfoPat r.info (pseqtot_ (gather [] r.elems))

  sem eooDSType : EOOType -> Type
  sem eooDSType =
  | VarEOOType r -> tyWithInfo r.info (ntyvar_ r.n.v)
  | ConEOOType r ->
    let ty =
      switch nameGetStr r.n.v
      case "Real" then tyfloat_
      case "Int" then tyint_
      case "Bool" then tybool_
      case "Unit" then tyunit_
      case _ then ntycon_ r.n.v
      end
    in
    tyWithInfo r.info ty
  | AppEOOType r ->
    let right = eooDSType r.right in
    let ty = TyApp { lhs = eooDSType r.left, rhs = right, info = r.info } in
    match r.left with ConEOOType leftr then
      switch nameGetStr leftr.n.v
      case "Seq" then TySeq { ty = right, info = r.info }
      case _ then ty
      end
    else ty
  | ty & TupEOOType r ->
    recursive let gather = lam tys. lam ty.
      match ty with TupEOOType r then
        gather (cons (eooDSType r.right) tys) r.left
      else cons (eooDSType ty) tys
    in
    tyWithInfo r.info (tytuple_ (gather [] ty))
  | SeqEOOType r -> TySeq { ty = eooDSType r.ty, info = r.info }
  | ArrowEOOType r -> TyArrow {
    from = eooDSType r.left,
    to = eooDSType r.right,
    info = r.info
  }
  | ForallEOOType r ->
    tyWithInfo r.info (foldr ntyall_ (eooDSType r.right) (map (lam n. n.v) r.ns))
end
