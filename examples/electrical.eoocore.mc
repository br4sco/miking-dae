include "lib.eoocore.mc"

type Node = Symbol

let genNode = gensym

let electricalDomain = gensym ()

let electricalEdge :
  { from : Node, to : Node, u : Float, i : Float } -> Equation
  = lam r. edgeff electricalDomain r.from r.to r.u r.i

-- Creates resistor from node `n1` to node `n2`.
let resistor :
  { resistance : Float, u : Float, i : Float } -> Node -> Node -> [Equation]
  = lam r. lam n1. lam n2.
  [
    eqnf r.u (mulf r.resistance r.i),
    electricalEdge { from = n1, to = n2, u = r.u, i = r.i }
  ]

-- Creates inductor from node `n1` to node `n2`.
let inductor :
  { inductance : Float, u : Float, i : Float } -> Node -> Node -> [Equation]
  = lam r. lam n1. lam n2.
  [
    eqnf r.u (mulf r.inductance (dotf 1 r.i)),
    electricalEdge { from = n1, to = n2, u = r.u, i = r.i }
  ]

-- Creates capacitor from node `n1` to node `n2`.
let capacitor :
  { capacitance : Float, u : Float, i : Float } -> Node -> Node -> [Equation]
  = lam r. lam n1. lam n2.
  [
    eqnf (dotf 1 r.u) (mulf r.capacitance r.i),
    electricalEdge { from = n1, to = n2, u = r.u, i = r.i }
  ]

-- Creates voltages source from node `n1` to node `n2`
let voltageSource : Float -> Node -> Node -> [Equation]
  = lam u. lam n1. lam n2.
    let i = gendynvarf "iVs" in
    [
      electricalEdge { from = n1, to = n2, u = u, i = i }
    ]

-- Creates voltages sensor from node `n1` to node `n2`
let voltageSensor : Float -> Node -> Node -> [Equation]
  = lam u. lam n1. lam n2.
    [
      electricalEdge { from = n1, to = n2, u = u, i = 0. }
    ]
