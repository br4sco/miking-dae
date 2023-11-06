include "./electrical.eoo.mc"

mexpr

let i = lam . gendynvarf "i" in
let u = lam . gendynvarf "u" in
let iL = i () in
let uL = u () in
let n1 = genNode () in
let n2 = genNode () in
let n3 = genNode () in
let lrc =
  concat
    [
      ieqnf uL 1.,
      ieqnf iL 1.
    ]
    (join
       [
         resistor { resistance = 1., u = u (), i = i () } n1 n2,
         inductor { inductance = 1., u = uL, i = iL } n2 n3,
         capacitor { capacitance = 1., u = u (), i = i () } n3 n1
       ])
in
(lrc, (iL, iL))
