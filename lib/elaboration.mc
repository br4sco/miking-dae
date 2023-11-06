/-
  This File implements elaboration of balance equations from a graph
  representation of the model topology. E.g., see Gordon C Andrews, Marc J
  Richard, Ronald J Anderson, A general vector-network formulation for dynamic
  systems with kinematic constraints and/or McPhee, J.J. On the use of linear
  graph theory in multibody system dynamics.
 -/

include "char.mc"
include "tuple.mc"
include "set.mc"
include "eqset.mc"

include "./linear-graph.mc"

type ElabGraph v t a = {
  cmpv : v -> v -> Int,
  cmpl : (t, a) -> (t, a) -> Int,
  edges : [((v, v), (t, a))]
}

let elabGraphGetVertices : all v. all t. all a. ElabGraph v t a -> [v]
  = lam g.
    setToSeq
      (foldl
         (lam acc. lam e.
           match e with ((v1, v2), _) in (setInsert v2 (setInsert v1 acc)))
         (setEmpty g.cmpv)
         g.edges)

let elabGraphToDot
  : all v. all t. all a. (v -> String) -> ((t, a) -> String) -> ElabGraph v t a -> String
  = lam v2str. lam l2str. lam g.
    join [
      "digraph {\n",
      strJoin "\n"
        (map (lam e.
          match e with ((v1, v2), l) in
          join
            ["  ", v2str v1, " -> ", v2str v2, " [label=\"", l2str l, "\"];"])
           g.edges),
      "\n}\n"
    ]

-- A sequence of terms/factors (c, v) with coefficients c that should be equal
-- to zero (where the representation of zero depends on the type of
-- v). Moreover, the operation to combine these terms/factors and coefficients
-- depends on this type. E.g, for v:Float, the zero is 0, we combine the terms
-- with addf, and c chould be interpreted as a float.
type ElabEquation a = (a, [(LGEdgeDirection, a)])

-- Equality of equation with associative terms/factors.
let elabEquationAssocEq
  : all a. all b. (a -> b -> Bool) -> ElabEquation a -> ElabEquation b -> Bool
  = lam eq. lam l. lam r.
    and
      (eq l.0 r.0)
      (eqsetEqual
         (lam l. lam r.
           (and (eqi l.0 r.0) (eq l.1 r.1)))
         l.1 r.1)

-- String representation of equation of scalar terms
let elabEquationScalarToString : all a. (a -> String) -> ElabEquation a -> String
  = lam toStr. lam e.
    join [
      toStr e.0,
      " = ",
      strJoin " + "
        (map
           (lam t. if lti t.0 0 then cons '-' (toStr t.1)
                 else if eqi t.0 0 then "0"
                      else (toStr t.1))
           e.1)
    ]

-- Empty elaboration graph
let elabGraphEmpty
  : all v. all t. all a.
    (v -> v -> Int) -> (t -> t -> Int) -> (a -> a -> Int) -> ElabGraph v t a
  = lam cmpv. lam cmpa. lam cmpt.
    { cmpv = cmpv, cmpl = tupleCmp2 cmpa cmpt, edges = [] }

let elabGraphToIncidenceMatrix
  : all v. all t. all a. [(t, a)] -> ElabGraph v t a -> (LGIMatrix, [v])
  = lam terms. lam g.
    let labelIndexMap =
      mapFromSeq g.cmpl (mapi (lam i. lam c. (c, i)) terms)
    in
    let vertices = elabGraphGetVertices g in
    let vertexIndexMap =
      mapFromSeq g.cmpv (mapi (lam i. lam v. (v, i)) vertices)
    in
    let mat =
      foldl
        (lam mat. lam e.
          match e with ((v1, v2), l) in
          if eqi (g.cmpv v1 v2) 0 then mat -- Ignore self-loops
          else
            let j = mapFindExn l labelIndexMap in
            let mat = lgIMSet mat (mapFindExn v2 vertexIndexMap) j lgOutEdge in
            lgIMSet mat (mapFindExn v1 vertexIndexMap) j lgInEdge)
        (lgIMEmpty (length vertices) (length terms))
        g.edges
    in
    (mat, vertices)

-- Elaborates elaboration graph to fundamental cutset and circuitset equations.
let elab
  : all v. all t. all a.
    [(t, a)] -> ElabGraph v t a -> ([ElabEquation t], [ElabEquation a])
  = lam terms. lam g.
    match elabGraphToIncidenceMatrix terms g with (mat, _) in
    match lgFCutCircSet mat terms with (circ, cut, terms) in
    let n = length circ in
    match splitAt terms n with (cutterms, circterms) in
    match unzip circterms with (acrossterms, throughvars) in
    match unzip cutterms with (acrossvars, througterms) in
    (zipWith
       (lam a. lam cs. (a, (filter (lam t. neqi t.0 0)) (zip cs acrossterms)))
       acrossvars circ,
     zipWith
       (lam t. lam cs. (t, (filter (lam t. neqi t.0 0)) (zip cs througterms)))
       throughvars cut)

mexpr
  let e = lam l. (cons 'i' l, cons 'u' l) in
  let g = elabGraphEmpty cmpChar cmpString cmpString in
  let g = {
    g with edges = [
      (('a', 'b'), e "L1"),
      (('c', 'b'), e "R3"),
      (('f', 'a'), e "V7"),
      (('e', 'b'), e "R4"),
      (('c', 'd'), e "C6"),
      (('f', 'e'), e "R5"),
      (('e', 'd'), e "L2")
    ]
  } in
  -- digraphPrintDot g (snoc []) (lam e. tail e.0);
  -- https://dot-to-ascii.ggerganov.com/ to visualize
  --                    ┌─────┐
  -- ┌───────────────── │  c  │
  -- │                  └─────┘
  -- │                    │
  -- │                    │ R3
  -- │                    ▼
  -- │            R4    ┌─────┐
  -- │      ┌─────────▶ │  b  │ ◀┐
  -- │      │           └─────┘  │
  -- │      │                    │
  -- │ C6   │                    │
  -- │      │                    │
  -- │    ┌─────┐  R5   ┌─────┐  │
  -- │    │  e  │ ◀──── │  f  │  │ L1
  -- │    └─────┘       └─────┘  │
  -- │      │             │      │
  -- │      │ L2          │ V7   │
  -- │      ▼             ▼      │
  -- │    ┌─────┐       ┌─────┐  │
  -- └──▶ │  d  │       │  a  │ ─┘
  --      └─────┘       └─────┘

  let terms = map e [
    "L1", "L2", "R3", "R4", "R5", "C6", "V7"
  ] in
  let expectedMat = lgIMOfIntSeq [
    [negi 1, 0, 0, 0, 0, 0, 1],
    [1, 0, 1, 1, 0, 0, 0],
    [0, 0, negi 1, 0, 0, negi 1, 0],
    [0, 1, 0, 0, 0, 1, 0],
    [0, negi 1, 0, negi 1, 1, 0, 0],
    [0, 0, 0, 0, negi 1, 0, negi 1]
  ] in
  let expectedVertices = ['a', 'b', 'c', 'd', 'e', 'f'] in
  match elabGraphToIncidenceMatrix terms g with (mat, vertices) in
  utest mat with expectedMat using lgIMEq else lgIMUtestToString in
  utest vertices with expectedVertices in

  let eq : [ElabEquation String] -> [ElabEquation String] -> Bool =
    eqsetEqual (elabEquationAssocEq eqString)
  in
  let toString =
    lam es. strJoin "\n" (map (elabEquationScalarToString (lam x. x)) es)
  in
  let toString = utestDefaultToString toString toString in
  match elab terms g with (circ, cut) in
  utest circ with [
    ("iL1", [(negi 1, "iV7")]),
    ("iL2", [(1, "iC6")]),
    ("iR3", [(1, "iC6")]),
    ("iR4", [(negi 1, "iC6"), (1, "iV7")]),
    ("iR5", [(1, "iV7")])
  ] using eq else toString in
  utest cut with [
    ("uC6", [(negi 1, "uL2"), (negi 1, "uR3"), (1, "uR4")]),
    ("uV7", [(1, "uL1"), (negi 1, "uR4"), (negi 1, "uR5")])
  ] using eq else toString in
  ()
