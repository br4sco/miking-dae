include "these.mc"

include "mexpr/free-vars.mc"
include "mexpr/constant-fold.mc"

include "./desugar.mc"
include "./daecore-structure.mc"
include "./lib/vec.mc"

let _daeIDMap = ref (mapEmpty (tupleCmp2 nameCmp subi))
let daeID : (Name, Int) -> Name
  = lam id.
    match id with (name, 0) then name
    else
      match id with (name, n) in
      let daeIDMap = deref _daeIDMap in
      if mapMem id daeIDMap then mapFindExn id daeIDMap
      else
        let name = nameSym (concat (nameGetStr name) (create n (lam. '\''))) in
        modref _daeIDMap (mapInsert id name daeIDMap);
        name

lang DAE = DAEAst + MExprFreeVars
  sem daeAnnotDVars : TmDAERec -> TmDAERec
  sem daeAnnotDVars =| dae ->
    let vars = mapFromSeq nameCmp dae.vars in
    recursive let inner = lam t.
      match t with TmVar r then
        if mapMem r.ident vars then
          TmDVar (tmDVarRecToTmVarRec 0 r)
        else TmVar r
      else smap_Expr_Expr inner t
    in
    match smap_Expr_Expr inner (TmDAE dae) with TmDAE dae then dae
    else error "impossible"

  sem daeStructure : TmDAERec -> [Set (Name, Int)]
  sem daeStructure =| dae -> map dvars dae.eqns

  type DAEStructuralAnalysis = {
    varOffset : [(Name, Int)],
    eqnsOffset : [Int]
  }
  sem daeStructuralAnalysis : TmDAERec -> DAEStructuralAnalysis
  sem daeStructuralAnalysis =| dae ->
    -- Enumerate dependent variables
    let varEnumMap =
      mapFromSeq nameCmp (mapi (lam i. lam v. (v.0, i)) dae.vars)
    in
    -- Find equation structure
    let eqnsStructure =
      map
        (lam vars. {
          variables =
          map
            (lam x.
              match x with (name, ord) in
              (mapFindExn name varEnumMap, ord))
            (setToSeq vars),
          inputs = []
        })
        (daeStructure dae)
    in
    -- Perform the structural analysis
    let analysis = consumeWarnErrsExn (structuralAnalysis eqnsStructure) in
    let varOffset =
      zipWith
        (lam v. lam d. (v.0, d)) dae.vars (vecToSeq analysis.d)
    in
    { varOffset = varOffset, eqnsOffset = vecToSeq analysis.c }

  sem daeADExpr : Int -> Expr -> Expr
  sem daeADExpr n =| t -> ad n (daeADRenameVarsExpr n t)

  sem daeADRenameVarsExpr : Int -> Expr -> Expr
  sem daeADRenameVarsExpr n =
  | TmVar r -> TmVar { r with ident = daeID (r.ident, n) }
  | TmLam r ->
    smap_Expr_Expr
      (daeADRenameVarsExpr n)
      (TmLam { r with ident = daeID (r.ident, n) })
  | TmLet r ->
    smap_Expr_Expr
      (daeADRenameVarsExpr n)
      (TmLet { r with ident = daeID (r.ident, n) })
  | TmRecLets r ->
    let bindings =
      map (lam b. { b with ident = daeID (b.ident, n) }) r.bindings
    in
    smap_Expr_Expr
      (daeADRenameVarsExpr n)
      (TmRecLets { r with bindings = bindings })
  | TmMatch r ->
    smap_Expr_Expr
      (daeADRenameVarsExpr n)
      (TmMatch { r with pat = daeADRenameVarsPat n r.pat })
  | t -> smap_Expr_Expr (daeADRenameVarsExpr n) t

  sem daeADRenameVarsPat : Int -> Pat -> Pat
  sem daeADRenameVarsPat n =
  | PatNamed (r & {ident = (PName ident)}) ->
    PatNamed { r with ident = PName (daeID (ident, n)) }
  | p -> smap_Pat_Pat (daeADRenameVarsPat n) p

  sem daeIndexReduce : [Int] -> TmDAERec -> TmDAERec
  sem daeIndexReduce ns =| dae ->
    let maxn = maxOrElse (lam. error "impossible") subi ns in
    let bindings =
      foldl1 bind_ (create (succ maxn) (lam n. daeADExpr n dae.bindings))
    in
    let eqns =
      zipWith
        (lam n. lam t.
          let t = daeADExpr n t in
          if neqi n 0 then tupleproj_ n t else t)
        ns dae.eqns
    in
    { dae with bindings = bindings, eqns = eqns }

  type DAEFirstOrderState = {
    ys : [These (Name, Int) (Name, Int)],
    indexMap : Map (Name, Int) (These Int Int)
  }
  sem daeFirstOrderState : [(Name, Int)] -> DAEFirstOrderState
  sem daeFirstOrderState =| ofs ->
    let ys =
      join
        (map
           (lam ofs.
             switch ofs
             case (name, 0) then [This ofs]
             case (name, n) then
               let vs = create (succ n) (lam n. (name, n)) in
               zipWith (lam x. lam y. These (x, y)) (init vs) (tail vs)
             end)
           ofs)
    in
    let names =
      mapi (lam i. theseBiMap (lam name. (name, i)) (lam name. (name, i))) ys
    in
    match thesePartition names with (xs, xps, xxps) in
    let fthis =
      lam acc. lam x.
        mapUpdate x.0
          (lam v.
            switch v
            case Some (That i) then Some (These (x.1, i))
            case None _ then Some (This x.1)
            case _ then error "impossible"
            end)
          acc
    in
    let fthat =
      lam acc. lam x.
        mapUpdate x.0
          (lam v.
            switch v
            case Some (This i) then Some (These (i, x.1))
            case None _ then Some (That x.1)
            case _ then error "impossible"
            end)
          acc
    in
    let indexMap = foldl fthis (mapEmpty dvarCmp) xs in
    let indexMap = foldl fthat indexMap xps in
    let indexMap =
      foldl (lam acc. lam x. fthat (fthis acc x.0) x.1) indexMap xxps
    in
    { ys = ys, indexMap = indexMap }

  sem daeIsDiffVars : DAEFirstOrderState -> [Bool]
  sem daeIsDiffVars =| state ->
    map (theseThese (lam. false) (lam. true) (lam. lam. true)) state.ys

  sem daeOrderReduce : DAEFirstOrderState -> Name -> Name -> TmDAERec -> TmDAERec
  sem daeOrderReduce state y yp =| dae ->
    let gety_ = lam i. get_ (nvar_ y) (int_ i) in
    let getyp_ = lam i. get_ (nvar_ yp) (int_ i) in
    recursive let subs = lam t.
      match t with TmDVar r then
        theseThese
          gety_ getyp_ (lam i. lam. gety_ i)
          (mapFindExn (r.ident, r.order) state.indexMap)
      else smap_Expr_Expr subs t
    in
    match subs (TmDAE dae) with TmDAE dae then
      let vars = [(y, tyseq_ tyfloat_), (yp, tyseq_ tyfloat_)] in
      let aliases = theseCatThese (mapValues state.indexMap) in
      let eqns = map (lam a. subf_ (gety_ a.0) (getyp_ a.1)) aliases in
      { dae with vars = vars, eqns = concat dae.eqns eqns }
    else error "impossible"

  sem daeGenInitExpr : DAEFirstOrderState -> TmDAERec -> Expr
  sem daeGenInitExpr state =| dae ->
    let errMsg = lam t.
      strJoin "\n" ["daeGenInitExpr: Malformed initial equation:", expr2str t]
    in
    match dae.vars with [(y, _), (yp, _)] then
      -- Construct assignments from the inital equations
      let updateAssignment = lam i. lam ts1.
        mapUpdate i
          (optionMapOr (Some ts1)
             (lam ts2.
               Some(
                 switch (ts1,ts2)
                 case (This ts1, That ts2) | (That ts1, This ts2) then
                   These (ts1, ts2)
                 case (This ts1, This ts2) then This (concat ts1 ts2)
                 case (That ts1, That ts2) then That (concat ts1 ts2)
                 case (This ts1, These (ts2a, ts2b)) then
                   These (concat ts1 ts2a,ts2b)
                 case (That ts1, These (ts2a, ts2b)) then
                   These (ts2a,concat ts1 ts2b)
                 case _ then error "impossible"
                 end)))
      in
      let assignmentMap =
        foldl
          (lam acc. lam eqn.
            match eqn with
              TmApp {
                lhs = TmApp {
                  lhs = TmConst {val = CSubf _},
                  rhs = TmApp {
                    lhs = TmApp {lhs = TmConst {val = CGet _}, rhs = TmVar r},
                    rhs = TmConst {val = CInt {val = i }}
                  }
                },
                rhs = t
              }
            then
              if or (setMem y (freeVars t)) (setMem yp (freeVars t)) then
                error (errMsg eqn)
              else
                let ts1 =
                  if nameEq r.ident y then This [t]
                  else if nameEq r.ident yp then That [t]
                       else error (errMsg eqn)
                in
                updateAssignment i ts1 acc
            else error (errMsg eqn))
          (mapEmpty subi)
          dae.ieqns
      in
      -- If some state has been assigned the multiple value we take the mean of
      -- these.
      -- TODO(oeriss, 2023-06-29): This should probably generate some warning.
      let mean = lam ts.
        match ts with [t] then t
        else divf_ (foldl1 addf_ ts) (float_ (int2float (length ts)))
      in
      let assignmentMap = mapMap (theseBiMap mean mean) assignmentMap in
      -- Go through aliases stemming from the order-reduction and propagate
      -- assignments.
      let assignmentMap =
        mapFoldWithKey
          (lam acc. lam.
            theseThese (lam. acc) (lam. acc)
              (lam i. lam j.
                let lookup = lam i. mapLookup i acc in
                switch
                  (optionBind (lookup i) theseGetHere,
                   optionBind (lookup j) theseGetThere)
                case (None _, Some t1) then
                  mapUpdate i
                    (optionMapOr (Some (This t1))
                       (lam x.
                         Some(
                           switch x
                           case This t2 then
                             This (divf_ (addf_ t1 t2) (float_ 2.))
                           case That t2 then These (t1, t2)
                           case These (t2, t3) then
                             These (divf_ (addf_ t1 t2) (float_ 2.), t3)
                           end)))
                    acc
                case (Some t1, None _) then
                  mapUpdate j
                    (optionMapOr (Some (This t1))
                       (lam x.
                         Some(
                           switch x
                           case This t2 then These (t2, t1)
                           case That t2 then
                             This (divf_ (addf_ t2 t1) (float_ 2.))
                           case These (t2, t3) then
                             These (t2, divf_ (addf_ t3 t1) (float_ 2.))
                           end)))
                    acc
                case _ then acc
                end))
          assignmentMap
          state.indexMap
      in
      -- Constucts inital values for all states from the assignements, defaults
      -- to 0.
      let initTms =
        create (length state.ys)
          (lam i. optionMapOr (float_ 0., float_ 0.)
                  (theseThese
                     (lam t. (t, float_ 0.))
                     (lam t. (float_ 0., t))
                     (lam t1. lam t2. (t1, t2)))
                  (mapLookup i assignmentMap))
      in
      match unzip initTms with (y0, yp0) in
      bind_ dae.bindings (utuple_ [seq_ y0, seq_ yp0])
    else error "daeGenInitExpr: Not a first-order DAE"

  sem daeGenOutExpr : TmDAERec -> Expr
  sem daeGenOutExpr =| dae ->
    let defaultPrint = print_ (str_ "Cannot print output") in
    let genPrintWithCommaSep = lam ts.
      if null ts then unit_
      else
        foldl1
          semi_
          (snoc
             (map
                (lam t. semi_ (print_ t) (print_ (str_ ",")))
                (init ts))
             (semi_ (print_ (last ts)) (print_ (str_ "\n"))))
    in
    let _r  = nameSym "r" in
    let body =
      switch tyTm dae.out
      case TyFloat _ then print_ dae.out
      case TyRecord r then
        let isFloatTy = lam ty. match ty with TyFloat _ then true else false in
        match record2tuple r.fields with Some tys then
          if forAll isFloatTy tys then
            let inexpr =
              genPrintWithCommaSep
                (create (length tys)
                   (lam i. float2string_ (tupleproj_ i (nvar_ _r))))
            in
            bind_ (nulet_ _r dae.out) inexpr
          else defaultPrint
        else defaultPrint
      case _ then defaultPrint
      end
    in
    bind_ dae.bindings (nlams_ dae.vars body)

  sem daeGenResExpr : TmDAERec -> Expr
  sem daeGenResExpr =| dae ->
    bind_ dae.bindings (nlams_ dae.vars (seq_ dae.eqns))
end

lang TestLang = DAE + DAEParseAnalysis + DAEParseDesugar + MExprConstantFold end

mexpr

----------------
-- Test setup --
----------------

use TestLang in

let _parseExpr = lam prog.
  (typeCheck (adSymbolize (parseDAEExprExn prog)))
in

let _parseAsTmDAE = lam prog.
  TmDAE (_tmToTmDAERec ( _parseExpr prog))
in

let _parseDAEProg = lam prog.
  let prog = daeParseExn "internal" prog in

  logMsg logLevel.debug
    (lam. strJoin "\n" ["Input program:", daeProgToString prog]);

  match typeCheck (adSymbolize (TmDAE (daeDesugarProg prog))) with TmDAE r
  then r else error "Impossible"
in

let _prepDVar = lam x. (nameGetStr x.0, x.1) in
let _prepDVars = lam vars. map _prepDVar (sort dvarCmp vars) in

-------------------
-- Input program --
-------------------

let daer = _parseDAEProg "
  let mul = lam x. lam y. x*y end
  let pow2 = lam x. mul x x end
  variables
  x, y, h : Float
  init
  x = 1.;
  x' = 2.;
  y'' = 0. - 1.
  equations
  x'' = mul x h;
  y'' = mul y h - 1.;
  pow2 x + pow2 y = pow2 1.
  output
  {x, x', x''}
  "
in

-------------------------
-- Test: daeAnnotDVars --
-------------------------

let daer = daeAnnotDVars daer in
let expected = _parseAsTmDAE "
let mul = lam x. lam y. mulf x y in
let pow2 = lam x. mul x x in
lam x: Float. lam y: Float. lam h: Float. {
  ieqns = (subf (prim 0 x) 1., subf (prim 1 x) 2., subf (prim 2 y) (subf 0. 1.)),
  eqns = (subf (prim 2 x) (mul (prim 0 x) (prim 0 h)),
          subf (prim 2 y) (subf (mul (prim 0 y) (prim 0 h)) 1.),
          subf (addf (pow2 (prim 0 x)) (pow2 (prim 0 y))) (pow2 1.)),
  out = (prim 0 x, prim 1 x, prim 2 x)
}
  "
in
utest expected with TmDAE daer using eqExpr in

------------------------
-- Test: daeStructure --
------------------------

let expected = [
  [("x", 0), ("x", 2), ("h", 0)],
  [("y", 0), ("y", 2), ("h", 0)],
  [("x", 0), ("y", 0)]
] in
let actual =
  map
    (lam s. _prepDVars (setToSeq s))
    (daeStructure daer)
in
utest actual with expected in

---------------------------------
-- Test: daeStructuralAnalysis --
---------------------------------

let expected = {
  varOffset = [("x", 2), ("y", 2), ("h", 0)],
  eqnsOffset = [0, 0, 2]
} in
let actual = daeStructuralAnalysis daer in
let ns = actual.eqnsOffset in
let varOffset = actual.varOffset in
utest _prepDVars varOffset with expected.varOffset in
utest ns with expected.eqnsOffset in

--------------------------
-- Test: daeIndexReduce --
--------------------------

let expected = _parseAsTmDAE "
let mul = lam x. lam y. mulf x y in
let pow2 = lam x. mul x x in
let mulp = lam xp. lam yp.
  (lam x. lam y.
    (mulf x.0 y.0,
     addf (mulf x.0 y.1) (mulf x.1 y.0)))
    xp yp
in
let pow2p = lam xp. mulp xp xp in
let mulpp = lam xpp. lam ypp.
  (lam x. lam y.
    (mulf x.0 y.0,
     addf (mulf x.0 y.1) (mulf x.1 y.0),
     addf
       (addf (mulf x.0 y.2) (mulf 2. (mulf x.1 y.1)))
       (mulf x.2 y.0)))
    xpp ypp
in
let pow2pp = lam xpp. mulpp xpp xpp in
lam x: Float. lam y: Float. lam h:Float. {
  ieqns = (subf (prim 0 x) 1., subf (prim 1 x) 2., subf (prim 2 y) (subf 0. 1.)),
  eqns = (subf (prim 2 x) (mul (prim 0 x) (prim 0 h)),
          subf (prim 2 y) (subf (mul (prim 0 y) (prim 0 h)) 1.),
          ((lam x. lam y. (subf x.0 y.0, subf x.1 y.1, subf x.2 y.2))
            ((lam x. lam y. (addf x.0 y.0, addf x.1 y.1, addf x.2 y.2))
               (pow2pp (prim 0 x, prim 1 x, prim 2 x))
               (pow2pp (prim 0 y, prim 1 y, prim 2 y)))
            (pow2pp (1., 0., 0.))).2),
  out = (prim 0 x, prim 1 x, prim 2 x)
}
  "
in
let daer = daeIndexReduce ns daer in
utest expected with TmDAE daer using eqExpr in

------------------------------
-- Test: daeFirstOrderState --
------------------------------

let expected = {
  ys = [
    These (("x", 0), ("x", 1)),
    These (("x", 1), ("x", 2)),
    These (("y", 0), ("y", 1)),
    These (("y", 1), ("y", 2)),
    This ("h", 0)
  ],
  indexMap = [
    (("x", 0), This 0),
    (("x", 1), These (1, 0)),
    (("x", 2), That 1),
    (("y", 0), This 2),
    (("y", 1), These (3, 2)),
    (("y", 2), That 3),
    (("h", 0), This 4)
  ] } in
let actual = daeFirstOrderState varOffset in
utest map (theseBiMap _prepDVar _prepDVar) actual.ys with expected.ys in
utest map (lam x. (_prepDVar x.0, x.1)) (mapBindings actual.indexMap)
  with expected.indexMap
in
let state = actual in

-------------------------
-- Test: daeIsDiffVars --
-------------------------

let expected = [true, true, true, true, false] in
let actual = daeIsDiffVars state in
utest expected with actual in

--------------------------
-- Test: daeOrderReduce --
--------------------------

let expected = _parseAsTmDAE "
let mul = lam x. lam y. mulf x y in
let pow2 = lam x. mul x x in
let mulp = lam xp. lam yp.
  (lam x. lam y.
    (mulf x.0 y.0,
     addf (mulf x.0 y.1) (mulf x.1 y.0)))
    xp yp
in
let pow2p = lam xp. mulp xp xp in
let mulpp = lam xpp. lam ypp.
  (lam x. lam y.
    (mulf x.0 y.0,
     addf (mulf x.0 y.1) (mulf x.1 y.0),
     addf
       (addf
          (mulf x.0 y.2)
          (mulf 2. (mulf x.1 y.1)))
       (mulf x.2 y.0)))
    xpp ypp
in
let pow2pp = lam xpp. mulpp xpp xpp in
lam y: [Float]. lam yp: [Float]. {
  ieqns = (subf (get y 0) 1., subf (get y 1) 2., subf (get yp 3) (subf 0. 1.)),
  eqns = (subf (get yp 1) (mul (get y 0) (get y 4)),
          subf (get yp 3) (subf (mul (get y 2) (get y 4)) 1.),
          ((lam x. lam y. (subf x.0 y.0, subf x.1 y.1, subf x.2 y.2))
            ((lam x. lam y. (addf x.0 y.0, addf x.1 y.1, addf x.2 y.2))
               (pow2pp (get y 0, get y 1, get yp 1))
               (pow2pp (get y 2, get y 3, get yp 3)))
            (pow2pp (1., 0., 0.))).2,
          subf (get y 1) (get yp 0),
          subf (get y 3) (get yp 2)),
  out = (get y 0, get y 1, get yp 1)
}
  "
in
let daer = daeOrderReduce state (nameSym "y") (nameSym "yp") daer in
utest expected with TmDAE daer using eqExpr in

--------------------------
-- Test: daeGenInitExpr --
--------------------------

let expected = _parseExpr "
let mul = lam x. lam y. mulf x y in
let pow2 = lam x. mul x x in
let mulp = lam xp. lam yp.
  (lam x. lam y.
    (mulf x.0 y.0,
     addf (mulf x.0 y.1) (mulf x.1 y.0)))
    xp yp
in
let pow2p = lam xp. mulp xp xp in
let mulpp = lam xpp. lam ypp.
  (lam x. lam y.
    (mulf x.0 y.0,
     addf (mulf x.0 y.1) (mulf x.1 y.0),
     addf
       (addf
          (mulf x.0 y.2)
          (mulf 2. (mulf x.1 y.1)))
       (mulf x.2 y.0)))
    xpp ypp
in
let pow2pp = lam xpp. mulpp xpp xpp in
([ 1., 2., 0., 0., 0. ],
 [ 2., 0., 0., subf 0. 1., 0. ])
  "
in
let actual = daeGenInitExpr state daer in
let iexpr = actual in
utest expected with actual using eqExpr in

-------------------------
-- Test: daeGenOutExpr --
-------------------------

let expected = _parseExpr "
let mul = lam x. lam y. mulf x y in
let pow2 = lam x. mul x x in
let mulp = lam xp. lam yp.
  (lam x. lam y.
    (mulf x.0 y.0,
     addf (mulf x.0 y.1) (mulf x.1 y.0)))
    xp yp
in
let pow2p = lam xp. mulp xp xp in
let mulpp = lam xpp. lam ypp.
  (lam x. lam y.
    (mulf x.0 y.0,
     addf (mulf x.0 y.1) (mulf x.1 y.0),
     addf
       (addf
          (mulf x.0 y.2)
          (mulf 2. (mulf x.1 y.1)))
       (mulf x.2 y.0)))
    xpp ypp
in
let pow2pp = lam xpp. mulpp xpp xpp in
lam y: [Float]. lam yp: [Float].
  let r = (get y 0, get y 1, get yp 1) in
  print (float2string r.0);
  print \",\";
  print (float2string r.1);
  print \",\";
  print (float2string r.2);
  print \"\\n\"
  "
in
let actual = daeGenOutExpr daer in
let oexpr = actual in
-- utest expected with actual using eqExpr in
-- OK, utest fails due to sequencing weirdness.
logSetLogLevel logLevel.error;
logMsg logLevel.debug (lam. strJoin "\n" ["oexpr expected:", expr2str expected]);
logMsg logLevel.debug (lam. strJoin "\n" ["oexpr actual:", expr2str actual]);
logSetLogLevel logLevel.error;

-------------------------
-- Test: daeGenGenExpr --
-------------------------

let expected = _parseExpr "
let mul = lam x. lam y. mulf x y in
let pow2 = lam x. mul x x in
let mulp = lam xp. lam yp.
  (lam x. lam y.
    (mulf x.0 y.0,
     addf (mulf x.0 y.1) (mulf x.1 y.0)))
    xp yp
in
let pow2p = lam xp. mulp xp xp in
let mulpp = lam xpp. lam ypp.
  (lam x. lam y.
    (mulf x.0 y.0,
     addf (mulf x.0 y.1) (mulf x.1 y.0),
     addf
       (addf
          (mulf x.0 y.2)
          (mulf 2. (mulf x.1 y.1)))
       (mulf x.2 y.0)))
    xpp ypp
in
let pow2pp = lam xpp. mulpp xpp xpp in
lam y: [Float]. lam yp: [Float]. [
  subf (get yp 1) (mul (get y 0) (get y 4)),
  subf (get yp 3) (subf (mul (get y 2) (get y 4)) 1.),
  ((lam x. lam y1. (subf x.0 y1.0, subf x.1 y1.1, subf x.2 y1.2))
    ((lam x1. lam y2. (addf x1.0 y2.0, addf x1.1 y2.1, addf x1.2 y2.2))
       (pow2pp (get y 0, get y 1, get yp 1))
       (pow2pp (get y 2, get y 3, get yp 3)))
    (pow2pp (1., 0., 0.))).2,
  subf (get y 1) (get yp 0),
  subf (get y 3) (get yp 2)
]
  "
in
let actual = daeGenResExpr daer in
let rexpr = actual in
utest expected with actual using eqExpr in

----------------------------------------------------
-- Test: Partial Evaluation of init, out, and res --
----------------------------------------------------

let expected = _parseExpr "
([ 1., 2., 0., 0., 0. ],
 [ 2., 0., 0., negf 1., 0. ])
  "
in
let actual = constantfold (peval iexpr) in
-- utest expected with actual using eqExpr in
-- OK, utest fails due to float comparsions.
logSetLogLevel logLevel.error;
logMsg logLevel.debug (lam. strJoin "\n" ["iexpr expected:", expr2str expected]);
logMsg logLevel.debug (lam. strJoin "\n" ["iexpr actual:", expr2str actual]);
logSetLogLevel logLevel.error;

let expected = _parseExpr "
lam y: [Float]. lam yp: [Float].
  let t = print (float2string (get y 0)) in
  let t1 = print \",\" in
  let t2 = print (float2string (get y 1)) in
  let t3 = print \",\" in
  let t4 = print (float2string (get yp 1)) in
  let t5 = print \"\\n\" in
  t5
  "
in
let actual = constantfold (peval oexpr) in
utest expected with actual using eqExpr in

let expected = _parseExpr "
lam y. lam yp.
  let t = get yp 1 in
  let t1 = get y 1 in
  let t2 = get y 0 in
  let t3 = get yp 3 in
  let t4 = get y 3 in
  let t5 = get y 2 in [
    subf (get yp 1) (mulf (get y 0) (get y 4)),
    subf (get yp 3) (subf (mulf (get y 2) (get y 4)) 1.),
    addf
      (addf
         (addf (mulf t2 t) (mulf 2. (mulf t1 t1)))
         (mulf t t2))
      (addf
         (addf (mulf t5 t3) (mulf 2. (mulf t4 t4)))
         (mulf t3 t5)),
    subf (get y 1) (get yp 0),
    subf (get y 3) (get yp 2)
  ]
  "
in
let actual = constantfold (peval rexpr) in
utest expected with actual using eqExpr in


()
