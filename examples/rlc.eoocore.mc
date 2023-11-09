include "./electrical.eoocore.mc"

mexpr

-- EOO Model of a simple RLC circuit.

let i = lam. gendynvarf "i" in
let u = lam. gendynvarf "u" in
let iL = i () in
let uL = u () in
let n1 = genNode () in
let n2 = genNode () in
let n3 = genNode () in
let lrc = join [
  [
    ieqnf uL 1.,
    ieqnf iL 1.
  ],
  resistor { resistance = 1., u = u (), i = i () } n1 n2,
  inductor { inductance = 1., u = uL, i = iL } n2 n3,
  capacitor { capacitance = 1., u = u (), i = i () } n3 n1
] in
let out = (iL, uL) in
(lrc, out)
