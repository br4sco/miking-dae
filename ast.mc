include "mexpr/ast-builder.mc"
include "mexpr/const-types.mc"
include "mexpr/ast.mc"

let tymodelfragment_ = tycon_ "ModelFragment"

lang EOOCoreAst = Ast
  -- Extended builtins
  syn Const =
  | CDotf {}
  | CGenDynVarf {}
  | CEqnf {}
  | CIEqnf {}
  | CEdgeff {}

  sem getConstStringCode (indent : Int) =
  | CDotf _ -> "dotf"
  | CGenDynVarf _ -> "gendynvarf"
  | CEqnf _ -> "eqnf"
  | CIEqnf _ -> "ieqnf"
  | CEdgeff _ -> "edgeff"

  sem eooCoreBuiltin : () -> [(String, Const)]
  sem eooCoreBuiltin =| _ ->
    [
      ("dotf", CDotf ()),
      ("gendynvarf", CGenDynVarf ()),
      ("eqnf", CEqnf ()),
      ("ieqnf", CIEqnf ()),
      ("edgeff", CEdgeff ())
    ]

  sem eooCoreBuiltinTypes : () -> [(String, [String])]
  sem eooCoreBuiltinTypes =| _ -> [
    ("ModelFragment", []),
    ("ModelDomain", ["a", "b"])
  ]

  sem constArity =
  | CDotf _ -> 2
  | CGenDynVarf _ -> 1
  | CEqnf _ -> 2
  | CIEqnf _ -> 2
  | CEdgeff _ -> 5

  sem tyConst =
  | CDotf _ -> tyarrows_ [tyint_, tyfloat_, tyfloat_]
  | CGenDynVarf _ -> tyarrow_ tystr_ tyfloat_
  | CEqnf _ -> tyarrows_ [tyfloat_, tyfloat_, tymodelfragment_]
  | CIEqnf _ -> tyarrows_ [tyfloat_, tyfloat_, tymodelfragment_]
  | CEdgeff _ ->
    (tyarrows_ [tysym_, tysym_, tysym_, tyfloat_, tyfloat_, tymodelfragment_])
end
