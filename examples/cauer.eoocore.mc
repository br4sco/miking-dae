include "./lib.eoocore.mc"
include "./electrical.eoocore.mc"

mexpr

let cR = 1. in
let cL = 1. in
let cC = 1. in

let i = lam. gendynvarf "i" in
let u = lam. gendynvarf "u" in

-- Models a Ciciuit with the k'th Cauer topology
-- (https://en.wikipedia.org/wiki/Network_synthesis_filters#/media/File:Cauer_lowpass.svg)
-- where `uIn` is the input voltge and `uOut` is the output voltage.
let cauer = lam uIn. lam uOut. lam k.
  let n3 = genNode () in
  let section = lam ns.
    match ns with (n1, n2) in join [
      inductor { inductance = cL, u = u (), i = i () } n1 n2,
      capacitor { capacitance = 1., u = u (), i = i () } n2 n3
    ]
  in
  let ns = create k (lam. genNode ()) in
  let n1 = head ns in
  let n2 = head (reverse ns) in
  let nss = zip ns (tail ns) in
  join [
    voltageSource uIn n1 n3,
    join (map section nss),
    capacitor { capacitance = cC, u = u (), i = i () } n3 n2,
    resistor { resistance = cR, u = u (), i = i () } n3 n2,
    voltageSensor uOut n3 n2
  ]
in

let uIn = u () in
let uOut = u () in
let t = gendynvarf "t" in       -- time signal
let k = 10 in                   -- the number of section
let model =
  concat
    [
      eqnf (dotf 1 t) 1.,
      eqnf uIn (divf 1. (addf 1. (exp (negf t))))
    ]
    (cauer uIn uOut k)
in
let out = (uIn, uOut) in
(model, out)
