type Node = Symbol

let genNode = gensym

let join = foldl concat []

let electricalDomain = gensym ()

let electricalEdge :
  { from : Node, to : Node, u : Float, i : Float } -> ModelFragment
  = lam r. edgeff electricalDomain r.from r.to r.u r.i

-- Creates resistor from node `n1` to node `n2`.
let resistor :
  { resistance : Float, u : Float, i : Float } -> Node -> Node -> [ModelFragment]
  = lam r. lam n1. lam n2.
  [
    eqnf r.u (mulf r.resistance r.i),
    electricalEdge { from = n1, to = n2, u = r.u, i = r.i }
  ]

-- Creates inductor from node `n1` to node `n2`.
let inductor :
  { inductance : Float, u : Float, i : Float } -> Node -> Node -> [ModelFragment]
  = lam r. lam n1. lam n2.
  [
    eqnf r.u (mulf r.inductance (dotf 1 r.i)),
    electricalEdge { from = n1, to = n2, u = r.u, i = r.i }
  ]

-- Creates capacitor from node `n1` to node `n2`.
let capacitor :
  { capacitance : Float, u : Float, i : Float } -> Node -> Node -> [ModelFragment]
  = lam r. lam n1. lam n2.
  [
    eqnf (dotf 1 r.u) (mulf r.capacitance r.i),
    electricalEdge { from = n1, to = n2, u = r.u, i = r.i }
  ]
