include "mexpr/ast-builder.mc"

include "name.mc"

-- We assume that these names are defined in the runtime.mc
let _daeBuiltins : Map String (Name, Type) =
  mapFromSeq cmpString (map (lam x. (x.0, (nameSym x.0, x.1))) [
    ("onehot", tyarrows_ [tyint_, tyint_, tyseq_ tyfloat_])
  ])

let daeBuiltins : Map String Name = mapMap (lam x. x.0) _daeBuiltins
let daeTCVarEnv : Map Name Type = mapFromSeq nameCmp (mapValues _daeBuiltins)

let _bi = lam str. mapFindExn str daeBuiltins
let onehot_ = lam n. lam i. appf2_ (nvar_ (_bi "onehot")) n i
