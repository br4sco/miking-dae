include "mexpr/ast-builder.mc"

include "name.mc"

-- We assume that these names are defined in the runtime.mc
let daeBuiltins =
  mapFromSeq cmpString (map (lam str. (str, nameSym str)) [
    "onehot"
  ])

let _bi = lam str. mapFindExn str daeBuiltins

let onehot_ = lam n. lam i. appf2_ (nvar_ (_bi "onehot")) n i
