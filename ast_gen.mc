include "seq.mc"
include "parser/ll1.mc"
include "parser/breakable.mc"
include "./lib/eoo-lexer.mc"
lang EOOBaseAst
  syn EOOModelHead =
  syn EOOType =
  syn EOOPat =
  syn EOOExpr =
  syn EOOModelBinding =
  syn EOODefBinding =
  syn EOOParam =
  syn EOOTop =
  syn EOOProg =
  sem smapAccumL_EOOProg_EOOProg : all a. (a -> EOOProg -> (a, EOOProg)) -> a -> EOOProg -> (a, EOOProg)
sem smapAccumL_EOOProg_EOOProg f acc =
  | x ->
    (acc, x)
  sem smap_EOOProg_EOOProg : (EOOProg -> EOOProg) -> EOOProg -> EOOProg
sem smap_EOOProg_EOOProg f =
  | x ->
    (smapAccumL_EOOProg_EOOProg
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOProg_EOOProg : all a. (a -> EOOProg -> a) -> a -> EOOProg -> a
sem sfold_EOOProg_EOOProg f acc =
  | x ->
    (smapAccumL_EOOProg_EOOProg
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOProg_EOOTop : all a. (a -> EOOTop -> (a, EOOTop)) -> a -> EOOProg -> (a, EOOProg)
sem smapAccumL_EOOProg_EOOTop f acc =
  | x ->
    (acc, x)
  sem smap_EOOProg_EOOTop : (EOOTop -> EOOTop) -> EOOProg -> EOOProg
sem smap_EOOProg_EOOTop f =
  | x ->
    (smapAccumL_EOOProg_EOOTop
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOProg_EOOTop : all a. (a -> EOOTop -> a) -> a -> EOOProg -> a
sem sfold_EOOProg_EOOTop f acc =
  | x ->
    (smapAccumL_EOOProg_EOOTop
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOProg_EOOParam : all a. (a -> EOOParam -> (a, EOOParam)) -> a -> EOOProg -> (a, EOOProg)
sem smapAccumL_EOOProg_EOOParam f acc =
  | x ->
    (acc, x)
  sem smap_EOOProg_EOOParam : (EOOParam -> EOOParam) -> EOOProg -> EOOProg
sem smap_EOOProg_EOOParam f =
  | x ->
    (smapAccumL_EOOProg_EOOParam
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOProg_EOOParam : all a. (a -> EOOParam -> a) -> a -> EOOProg -> a
sem sfold_EOOProg_EOOParam f acc =
  | x ->
    (smapAccumL_EOOProg_EOOParam
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOProg_EOODefBinding : all a. (a -> EOODefBinding -> (a, EOODefBinding)) -> a -> EOOProg -> (a, EOOProg)
sem smapAccumL_EOOProg_EOODefBinding f acc =
  | x ->
    (acc, x)
  sem smap_EOOProg_EOODefBinding : (EOODefBinding -> EOODefBinding) -> EOOProg -> EOOProg
sem smap_EOOProg_EOODefBinding f =
  | x ->
    (smapAccumL_EOOProg_EOODefBinding
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOProg_EOODefBinding : all a. (a -> EOODefBinding -> a) -> a -> EOOProg -> a
sem sfold_EOOProg_EOODefBinding f acc =
  | x ->
    (smapAccumL_EOOProg_EOODefBinding
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOProg_EOOModelBinding : all a. (a -> EOOModelBinding -> (a, EOOModelBinding)) -> a -> EOOProg -> (a, EOOProg)
sem smapAccumL_EOOProg_EOOModelBinding f acc =
  | x ->
    (acc, x)
  sem smap_EOOProg_EOOModelBinding : (EOOModelBinding -> EOOModelBinding) -> EOOProg -> EOOProg
sem smap_EOOProg_EOOModelBinding f =
  | x ->
    (smapAccumL_EOOProg_EOOModelBinding
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOProg_EOOModelBinding : all a. (a -> EOOModelBinding -> a) -> a -> EOOProg -> a
sem sfold_EOOProg_EOOModelBinding f acc =
  | x ->
    (smapAccumL_EOOProg_EOOModelBinding
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOProg_EOOExpr : all a. (a -> EOOExpr -> (a, EOOExpr)) -> a -> EOOProg -> (a, EOOProg)
sem smapAccumL_EOOProg_EOOExpr f acc =
  | x ->
    (acc, x)
  sem smap_EOOProg_EOOExpr : (EOOExpr -> EOOExpr) -> EOOProg -> EOOProg
sem smap_EOOProg_EOOExpr f =
  | x ->
    (smapAccumL_EOOProg_EOOExpr
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOProg_EOOExpr : all a. (a -> EOOExpr -> a) -> a -> EOOProg -> a
sem sfold_EOOProg_EOOExpr f acc =
  | x ->
    (smapAccumL_EOOProg_EOOExpr
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOProg_EOOPat : all a. (a -> EOOPat -> (a, EOOPat)) -> a -> EOOProg -> (a, EOOProg)
sem smapAccumL_EOOProg_EOOPat f acc =
  | x ->
    (acc, x)
  sem smap_EOOProg_EOOPat : (EOOPat -> EOOPat) -> EOOProg -> EOOProg
sem smap_EOOProg_EOOPat f =
  | x ->
    (smapAccumL_EOOProg_EOOPat
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOProg_EOOPat : all a. (a -> EOOPat -> a) -> a -> EOOProg -> a
sem sfold_EOOProg_EOOPat f acc =
  | x ->
    (smapAccumL_EOOProg_EOOPat
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOProg_EOOType : all a. (a -> EOOType -> (a, EOOType)) -> a -> EOOProg -> (a, EOOProg)
sem smapAccumL_EOOProg_EOOType f acc =
  | x ->
    (acc, x)
  sem smap_EOOProg_EOOType : (EOOType -> EOOType) -> EOOProg -> EOOProg
sem smap_EOOProg_EOOType f =
  | x ->
    (smapAccumL_EOOProg_EOOType
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOProg_EOOType : all a. (a -> EOOType -> a) -> a -> EOOProg -> a
sem sfold_EOOProg_EOOType f acc =
  | x ->
    (smapAccumL_EOOProg_EOOType
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOProg_EOOModelHead : all a. (a -> EOOModelHead -> (a, EOOModelHead)) -> a -> EOOProg -> (a, EOOProg)
sem smapAccumL_EOOProg_EOOModelHead f acc =
  | x ->
    (acc, x)
  sem smap_EOOProg_EOOModelHead : (EOOModelHead -> EOOModelHead) -> EOOProg -> EOOProg
sem smap_EOOProg_EOOModelHead f =
  | x ->
    (smapAccumL_EOOProg_EOOModelHead
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOProg_EOOModelHead : all a. (a -> EOOModelHead -> a) -> a -> EOOProg -> a
sem sfold_EOOProg_EOOModelHead f acc =
  | x ->
    (smapAccumL_EOOProg_EOOModelHead
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOTop_EOOProg : all a. (a -> EOOProg -> (a, EOOProg)) -> a -> EOOTop -> (a, EOOTop)
sem smapAccumL_EOOTop_EOOProg f acc =
  | x ->
    (acc, x)
  sem smap_EOOTop_EOOProg : (EOOProg -> EOOProg) -> EOOTop -> EOOTop
sem smap_EOOTop_EOOProg f =
  | x ->
    (smapAccumL_EOOTop_EOOProg
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOTop_EOOProg : all a. (a -> EOOProg -> a) -> a -> EOOTop -> a
sem sfold_EOOTop_EOOProg f acc =
  | x ->
    (smapAccumL_EOOTop_EOOProg
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOTop_EOOTop : all a. (a -> EOOTop -> (a, EOOTop)) -> a -> EOOTop -> (a, EOOTop)
sem smapAccumL_EOOTop_EOOTop f acc =
  | x ->
    (acc, x)
  sem smap_EOOTop_EOOTop : (EOOTop -> EOOTop) -> EOOTop -> EOOTop
sem smap_EOOTop_EOOTop f =
  | x ->
    (smapAccumL_EOOTop_EOOTop
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOTop_EOOTop : all a. (a -> EOOTop -> a) -> a -> EOOTop -> a
sem sfold_EOOTop_EOOTop f acc =
  | x ->
    (smapAccumL_EOOTop_EOOTop
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOTop_EOOParam : all a. (a -> EOOParam -> (a, EOOParam)) -> a -> EOOTop -> (a, EOOTop)
sem smapAccumL_EOOTop_EOOParam f acc =
  | x ->
    (acc, x)
  sem smap_EOOTop_EOOParam : (EOOParam -> EOOParam) -> EOOTop -> EOOTop
sem smap_EOOTop_EOOParam f =
  | x ->
    (smapAccumL_EOOTop_EOOParam
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOTop_EOOParam : all a. (a -> EOOParam -> a) -> a -> EOOTop -> a
sem sfold_EOOTop_EOOParam f acc =
  | x ->
    (smapAccumL_EOOTop_EOOParam
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOTop_EOODefBinding : all a. (a -> EOODefBinding -> (a, EOODefBinding)) -> a -> EOOTop -> (a, EOOTop)
sem smapAccumL_EOOTop_EOODefBinding f acc =
  | x ->
    (acc, x)
  sem smap_EOOTop_EOODefBinding : (EOODefBinding -> EOODefBinding) -> EOOTop -> EOOTop
sem smap_EOOTop_EOODefBinding f =
  | x ->
    (smapAccumL_EOOTop_EOODefBinding
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOTop_EOODefBinding : all a. (a -> EOODefBinding -> a) -> a -> EOOTop -> a
sem sfold_EOOTop_EOODefBinding f acc =
  | x ->
    (smapAccumL_EOOTop_EOODefBinding
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOTop_EOOModelBinding : all a. (a -> EOOModelBinding -> (a, EOOModelBinding)) -> a -> EOOTop -> (a, EOOTop)
sem smapAccumL_EOOTop_EOOModelBinding f acc =
  | x ->
    (acc, x)
  sem smap_EOOTop_EOOModelBinding : (EOOModelBinding -> EOOModelBinding) -> EOOTop -> EOOTop
sem smap_EOOTop_EOOModelBinding f =
  | x ->
    (smapAccumL_EOOTop_EOOModelBinding
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOTop_EOOModelBinding : all a. (a -> EOOModelBinding -> a) -> a -> EOOTop -> a
sem sfold_EOOTop_EOOModelBinding f acc =
  | x ->
    (smapAccumL_EOOTop_EOOModelBinding
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOTop_EOOExpr : all a. (a -> EOOExpr -> (a, EOOExpr)) -> a -> EOOTop -> (a, EOOTop)
sem smapAccumL_EOOTop_EOOExpr f acc =
  | x ->
    (acc, x)
  sem smap_EOOTop_EOOExpr : (EOOExpr -> EOOExpr) -> EOOTop -> EOOTop
sem smap_EOOTop_EOOExpr f =
  | x ->
    (smapAccumL_EOOTop_EOOExpr
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOTop_EOOExpr : all a. (a -> EOOExpr -> a) -> a -> EOOTop -> a
sem sfold_EOOTop_EOOExpr f acc =
  | x ->
    (smapAccumL_EOOTop_EOOExpr
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOTop_EOOPat : all a. (a -> EOOPat -> (a, EOOPat)) -> a -> EOOTop -> (a, EOOTop)
sem smapAccumL_EOOTop_EOOPat f acc =
  | x ->
    (acc, x)
  sem smap_EOOTop_EOOPat : (EOOPat -> EOOPat) -> EOOTop -> EOOTop
sem smap_EOOTop_EOOPat f =
  | x ->
    (smapAccumL_EOOTop_EOOPat
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOTop_EOOPat : all a. (a -> EOOPat -> a) -> a -> EOOTop -> a
sem sfold_EOOTop_EOOPat f acc =
  | x ->
    (smapAccumL_EOOTop_EOOPat
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOTop_EOOType : all a. (a -> EOOType -> (a, EOOType)) -> a -> EOOTop -> (a, EOOTop)
sem smapAccumL_EOOTop_EOOType f acc =
  | x ->
    (acc, x)
  sem smap_EOOTop_EOOType : (EOOType -> EOOType) -> EOOTop -> EOOTop
sem smap_EOOTop_EOOType f =
  | x ->
    (smapAccumL_EOOTop_EOOType
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOTop_EOOType : all a. (a -> EOOType -> a) -> a -> EOOTop -> a
sem sfold_EOOTop_EOOType f acc =
  | x ->
    (smapAccumL_EOOTop_EOOType
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOTop_EOOModelHead : all a. (a -> EOOModelHead -> (a, EOOModelHead)) -> a -> EOOTop -> (a, EOOTop)
sem smapAccumL_EOOTop_EOOModelHead f acc =
  | x ->
    (acc, x)
  sem smap_EOOTop_EOOModelHead : (EOOModelHead -> EOOModelHead) -> EOOTop -> EOOTop
sem smap_EOOTop_EOOModelHead f =
  | x ->
    (smapAccumL_EOOTop_EOOModelHead
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOTop_EOOModelHead : all a. (a -> EOOModelHead -> a) -> a -> EOOTop -> a
sem sfold_EOOTop_EOOModelHead f acc =
  | x ->
    (smapAccumL_EOOTop_EOOModelHead
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOParam_EOOProg : all a. (a -> EOOProg -> (a, EOOProg)) -> a -> EOOParam -> (a, EOOParam)
sem smapAccumL_EOOParam_EOOProg f acc =
  | x ->
    (acc, x)
  sem smap_EOOParam_EOOProg : (EOOProg -> EOOProg) -> EOOParam -> EOOParam
sem smap_EOOParam_EOOProg f =
  | x ->
    (smapAccumL_EOOParam_EOOProg
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOParam_EOOProg : all a. (a -> EOOProg -> a) -> a -> EOOParam -> a
sem sfold_EOOParam_EOOProg f acc =
  | x ->
    (smapAccumL_EOOParam_EOOProg
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOParam_EOOTop : all a. (a -> EOOTop -> (a, EOOTop)) -> a -> EOOParam -> (a, EOOParam)
sem smapAccumL_EOOParam_EOOTop f acc =
  | x ->
    (acc, x)
  sem smap_EOOParam_EOOTop : (EOOTop -> EOOTop) -> EOOParam -> EOOParam
sem smap_EOOParam_EOOTop f =
  | x ->
    (smapAccumL_EOOParam_EOOTop
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOParam_EOOTop : all a. (a -> EOOTop -> a) -> a -> EOOParam -> a
sem sfold_EOOParam_EOOTop f acc =
  | x ->
    (smapAccumL_EOOParam_EOOTop
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOParam_EOOParam : all a. (a -> EOOParam -> (a, EOOParam)) -> a -> EOOParam -> (a, EOOParam)
sem smapAccumL_EOOParam_EOOParam f acc =
  | x ->
    (acc, x)
  sem smap_EOOParam_EOOParam : (EOOParam -> EOOParam) -> EOOParam -> EOOParam
sem smap_EOOParam_EOOParam f =
  | x ->
    (smapAccumL_EOOParam_EOOParam
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOParam_EOOParam : all a. (a -> EOOParam -> a) -> a -> EOOParam -> a
sem sfold_EOOParam_EOOParam f acc =
  | x ->
    (smapAccumL_EOOParam_EOOParam
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOParam_EOODefBinding : all a. (a -> EOODefBinding -> (a, EOODefBinding)) -> a -> EOOParam -> (a, EOOParam)
sem smapAccumL_EOOParam_EOODefBinding f acc =
  | x ->
    (acc, x)
  sem smap_EOOParam_EOODefBinding : (EOODefBinding -> EOODefBinding) -> EOOParam -> EOOParam
sem smap_EOOParam_EOODefBinding f =
  | x ->
    (smapAccumL_EOOParam_EOODefBinding
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOParam_EOODefBinding : all a. (a -> EOODefBinding -> a) -> a -> EOOParam -> a
sem sfold_EOOParam_EOODefBinding f acc =
  | x ->
    (smapAccumL_EOOParam_EOODefBinding
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOParam_EOOModelBinding : all a. (a -> EOOModelBinding -> (a, EOOModelBinding)) -> a -> EOOParam -> (a, EOOParam)
sem smapAccumL_EOOParam_EOOModelBinding f acc =
  | x ->
    (acc, x)
  sem smap_EOOParam_EOOModelBinding : (EOOModelBinding -> EOOModelBinding) -> EOOParam -> EOOParam
sem smap_EOOParam_EOOModelBinding f =
  | x ->
    (smapAccumL_EOOParam_EOOModelBinding
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOParam_EOOModelBinding : all a. (a -> EOOModelBinding -> a) -> a -> EOOParam -> a
sem sfold_EOOParam_EOOModelBinding f acc =
  | x ->
    (smapAccumL_EOOParam_EOOModelBinding
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOParam_EOOExpr : all a. (a -> EOOExpr -> (a, EOOExpr)) -> a -> EOOParam -> (a, EOOParam)
sem smapAccumL_EOOParam_EOOExpr f acc =
  | x ->
    (acc, x)
  sem smap_EOOParam_EOOExpr : (EOOExpr -> EOOExpr) -> EOOParam -> EOOParam
sem smap_EOOParam_EOOExpr f =
  | x ->
    (smapAccumL_EOOParam_EOOExpr
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOParam_EOOExpr : all a. (a -> EOOExpr -> a) -> a -> EOOParam -> a
sem sfold_EOOParam_EOOExpr f acc =
  | x ->
    (smapAccumL_EOOParam_EOOExpr
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOParam_EOOPat : all a. (a -> EOOPat -> (a, EOOPat)) -> a -> EOOParam -> (a, EOOParam)
sem smapAccumL_EOOParam_EOOPat f acc =
  | x ->
    (acc, x)
  sem smap_EOOParam_EOOPat : (EOOPat -> EOOPat) -> EOOParam -> EOOParam
sem smap_EOOParam_EOOPat f =
  | x ->
    (smapAccumL_EOOParam_EOOPat
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOParam_EOOPat : all a. (a -> EOOPat -> a) -> a -> EOOParam -> a
sem sfold_EOOParam_EOOPat f acc =
  | x ->
    (smapAccumL_EOOParam_EOOPat
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOParam_EOOType : all a. (a -> EOOType -> (a, EOOType)) -> a -> EOOParam -> (a, EOOParam)
sem smapAccumL_EOOParam_EOOType f acc =
  | x ->
    (acc, x)
  sem smap_EOOParam_EOOType : (EOOType -> EOOType) -> EOOParam -> EOOParam
sem smap_EOOParam_EOOType f =
  | x ->
    (smapAccumL_EOOParam_EOOType
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOParam_EOOType : all a. (a -> EOOType -> a) -> a -> EOOParam -> a
sem sfold_EOOParam_EOOType f acc =
  | x ->
    (smapAccumL_EOOParam_EOOType
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOParam_EOOModelHead : all a. (a -> EOOModelHead -> (a, EOOModelHead)) -> a -> EOOParam -> (a, EOOParam)
sem smapAccumL_EOOParam_EOOModelHead f acc =
  | x ->
    (acc, x)
  sem smap_EOOParam_EOOModelHead : (EOOModelHead -> EOOModelHead) -> EOOParam -> EOOParam
sem smap_EOOParam_EOOModelHead f =
  | x ->
    (smapAccumL_EOOParam_EOOModelHead
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOParam_EOOModelHead : all a. (a -> EOOModelHead -> a) -> a -> EOOParam -> a
sem sfold_EOOParam_EOOModelHead f acc =
  | x ->
    (smapAccumL_EOOParam_EOOModelHead
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOODefBinding_EOOProg : all a. (a -> EOOProg -> (a, EOOProg)) -> a -> EOODefBinding -> (a, EOODefBinding)
sem smapAccumL_EOODefBinding_EOOProg f acc =
  | x ->
    (acc, x)
  sem smap_EOODefBinding_EOOProg : (EOOProg -> EOOProg) -> EOODefBinding -> EOODefBinding
sem smap_EOODefBinding_EOOProg f =
  | x ->
    (smapAccumL_EOODefBinding_EOOProg
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOODefBinding_EOOProg : all a. (a -> EOOProg -> a) -> a -> EOODefBinding -> a
sem sfold_EOODefBinding_EOOProg f acc =
  | x ->
    (smapAccumL_EOODefBinding_EOOProg
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOODefBinding_EOOTop : all a. (a -> EOOTop -> (a, EOOTop)) -> a -> EOODefBinding -> (a, EOODefBinding)
sem smapAccumL_EOODefBinding_EOOTop f acc =
  | x ->
    (acc, x)
  sem smap_EOODefBinding_EOOTop : (EOOTop -> EOOTop) -> EOODefBinding -> EOODefBinding
sem smap_EOODefBinding_EOOTop f =
  | x ->
    (smapAccumL_EOODefBinding_EOOTop
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOODefBinding_EOOTop : all a. (a -> EOOTop -> a) -> a -> EOODefBinding -> a
sem sfold_EOODefBinding_EOOTop f acc =
  | x ->
    (smapAccumL_EOODefBinding_EOOTop
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOODefBinding_EOOParam : all a. (a -> EOOParam -> (a, EOOParam)) -> a -> EOODefBinding -> (a, EOODefBinding)
sem smapAccumL_EOODefBinding_EOOParam f acc =
  | x ->
    (acc, x)
  sem smap_EOODefBinding_EOOParam : (EOOParam -> EOOParam) -> EOODefBinding -> EOODefBinding
sem smap_EOODefBinding_EOOParam f =
  | x ->
    (smapAccumL_EOODefBinding_EOOParam
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOODefBinding_EOOParam : all a. (a -> EOOParam -> a) -> a -> EOODefBinding -> a
sem sfold_EOODefBinding_EOOParam f acc =
  | x ->
    (smapAccumL_EOODefBinding_EOOParam
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOODefBinding_EOODefBinding : all a. (a -> EOODefBinding -> (a, EOODefBinding)) -> a -> EOODefBinding -> (a, EOODefBinding)
sem smapAccumL_EOODefBinding_EOODefBinding f acc =
  | x ->
    (acc, x)
  sem smap_EOODefBinding_EOODefBinding : (EOODefBinding -> EOODefBinding) -> EOODefBinding -> EOODefBinding
sem smap_EOODefBinding_EOODefBinding f =
  | x ->
    (smapAccumL_EOODefBinding_EOODefBinding
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOODefBinding_EOODefBinding : all a. (a -> EOODefBinding -> a) -> a -> EOODefBinding -> a
sem sfold_EOODefBinding_EOODefBinding f acc =
  | x ->
    (smapAccumL_EOODefBinding_EOODefBinding
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOODefBinding_EOOModelBinding : all a. (a -> EOOModelBinding -> (a, EOOModelBinding)) -> a -> EOODefBinding -> (a, EOODefBinding)
sem smapAccumL_EOODefBinding_EOOModelBinding f acc =
  | x ->
    (acc, x)
  sem smap_EOODefBinding_EOOModelBinding : (EOOModelBinding -> EOOModelBinding) -> EOODefBinding -> EOODefBinding
sem smap_EOODefBinding_EOOModelBinding f =
  | x ->
    (smapAccumL_EOODefBinding_EOOModelBinding
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOODefBinding_EOOModelBinding : all a. (a -> EOOModelBinding -> a) -> a -> EOODefBinding -> a
sem sfold_EOODefBinding_EOOModelBinding f acc =
  | x ->
    (smapAccumL_EOODefBinding_EOOModelBinding
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOODefBinding_EOOExpr : all a. (a -> EOOExpr -> (a, EOOExpr)) -> a -> EOODefBinding -> (a, EOODefBinding)
sem smapAccumL_EOODefBinding_EOOExpr f acc =
  | x ->
    (acc, x)
  sem smap_EOODefBinding_EOOExpr : (EOOExpr -> EOOExpr) -> EOODefBinding -> EOODefBinding
sem smap_EOODefBinding_EOOExpr f =
  | x ->
    (smapAccumL_EOODefBinding_EOOExpr
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOODefBinding_EOOExpr : all a. (a -> EOOExpr -> a) -> a -> EOODefBinding -> a
sem sfold_EOODefBinding_EOOExpr f acc =
  | x ->
    (smapAccumL_EOODefBinding_EOOExpr
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOODefBinding_EOOPat : all a. (a -> EOOPat -> (a, EOOPat)) -> a -> EOODefBinding -> (a, EOODefBinding)
sem smapAccumL_EOODefBinding_EOOPat f acc =
  | x ->
    (acc, x)
  sem smap_EOODefBinding_EOOPat : (EOOPat -> EOOPat) -> EOODefBinding -> EOODefBinding
sem smap_EOODefBinding_EOOPat f =
  | x ->
    (smapAccumL_EOODefBinding_EOOPat
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOODefBinding_EOOPat : all a. (a -> EOOPat -> a) -> a -> EOODefBinding -> a
sem sfold_EOODefBinding_EOOPat f acc =
  | x ->
    (smapAccumL_EOODefBinding_EOOPat
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOODefBinding_EOOType : all a. (a -> EOOType -> (a, EOOType)) -> a -> EOODefBinding -> (a, EOODefBinding)
sem smapAccumL_EOODefBinding_EOOType f acc =
  | x ->
    (acc, x)
  sem smap_EOODefBinding_EOOType : (EOOType -> EOOType) -> EOODefBinding -> EOODefBinding
sem smap_EOODefBinding_EOOType f =
  | x ->
    (smapAccumL_EOODefBinding_EOOType
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOODefBinding_EOOType : all a. (a -> EOOType -> a) -> a -> EOODefBinding -> a
sem sfold_EOODefBinding_EOOType f acc =
  | x ->
    (smapAccumL_EOODefBinding_EOOType
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOODefBinding_EOOModelHead : all a. (a -> EOOModelHead -> (a, EOOModelHead)) -> a -> EOODefBinding -> (a, EOODefBinding)
sem smapAccumL_EOODefBinding_EOOModelHead f acc =
  | x ->
    (acc, x)
  sem smap_EOODefBinding_EOOModelHead : (EOOModelHead -> EOOModelHead) -> EOODefBinding -> EOODefBinding
sem smap_EOODefBinding_EOOModelHead f =
  | x ->
    (smapAccumL_EOODefBinding_EOOModelHead
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOODefBinding_EOOModelHead : all a. (a -> EOOModelHead -> a) -> a -> EOODefBinding -> a
sem sfold_EOODefBinding_EOOModelHead f acc =
  | x ->
    (smapAccumL_EOODefBinding_EOOModelHead
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOModelBinding_EOOProg : all a. (a -> EOOProg -> (a, EOOProg)) -> a -> EOOModelBinding -> (a, EOOModelBinding)
sem smapAccumL_EOOModelBinding_EOOProg f acc =
  | x ->
    (acc, x)
  sem smap_EOOModelBinding_EOOProg : (EOOProg -> EOOProg) -> EOOModelBinding -> EOOModelBinding
sem smap_EOOModelBinding_EOOProg f =
  | x ->
    (smapAccumL_EOOModelBinding_EOOProg
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOModelBinding_EOOProg : all a. (a -> EOOProg -> a) -> a -> EOOModelBinding -> a
sem sfold_EOOModelBinding_EOOProg f acc =
  | x ->
    (smapAccumL_EOOModelBinding_EOOProg
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOModelBinding_EOOTop : all a. (a -> EOOTop -> (a, EOOTop)) -> a -> EOOModelBinding -> (a, EOOModelBinding)
sem smapAccumL_EOOModelBinding_EOOTop f acc =
  | x ->
    (acc, x)
  sem smap_EOOModelBinding_EOOTop : (EOOTop -> EOOTop) -> EOOModelBinding -> EOOModelBinding
sem smap_EOOModelBinding_EOOTop f =
  | x ->
    (smapAccumL_EOOModelBinding_EOOTop
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOModelBinding_EOOTop : all a. (a -> EOOTop -> a) -> a -> EOOModelBinding -> a
sem sfold_EOOModelBinding_EOOTop f acc =
  | x ->
    (smapAccumL_EOOModelBinding_EOOTop
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOModelBinding_EOOParam : all a. (a -> EOOParam -> (a, EOOParam)) -> a -> EOOModelBinding -> (a, EOOModelBinding)
sem smapAccumL_EOOModelBinding_EOOParam f acc =
  | x ->
    (acc, x)
  sem smap_EOOModelBinding_EOOParam : (EOOParam -> EOOParam) -> EOOModelBinding -> EOOModelBinding
sem smap_EOOModelBinding_EOOParam f =
  | x ->
    (smapAccumL_EOOModelBinding_EOOParam
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOModelBinding_EOOParam : all a. (a -> EOOParam -> a) -> a -> EOOModelBinding -> a
sem sfold_EOOModelBinding_EOOParam f acc =
  | x ->
    (smapAccumL_EOOModelBinding_EOOParam
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOModelBinding_EOODefBinding : all a. (a -> EOODefBinding -> (a, EOODefBinding)) -> a -> EOOModelBinding -> (a, EOOModelBinding)
sem smapAccumL_EOOModelBinding_EOODefBinding f acc =
  | x ->
    (acc, x)
  sem smap_EOOModelBinding_EOODefBinding : (EOODefBinding -> EOODefBinding) -> EOOModelBinding -> EOOModelBinding
sem smap_EOOModelBinding_EOODefBinding f =
  | x ->
    (smapAccumL_EOOModelBinding_EOODefBinding
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOModelBinding_EOODefBinding : all a. (a -> EOODefBinding -> a) -> a -> EOOModelBinding -> a
sem sfold_EOOModelBinding_EOODefBinding f acc =
  | x ->
    (smapAccumL_EOOModelBinding_EOODefBinding
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOModelBinding_EOOModelBinding : all a. (a -> EOOModelBinding -> (a, EOOModelBinding)) -> a -> EOOModelBinding -> (a, EOOModelBinding)
sem smapAccumL_EOOModelBinding_EOOModelBinding f acc =
  | x ->
    (acc, x)
  sem smap_EOOModelBinding_EOOModelBinding : (EOOModelBinding -> EOOModelBinding) -> EOOModelBinding -> EOOModelBinding
sem smap_EOOModelBinding_EOOModelBinding f =
  | x ->
    (smapAccumL_EOOModelBinding_EOOModelBinding
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOModelBinding_EOOModelBinding : all a. (a -> EOOModelBinding -> a) -> a -> EOOModelBinding -> a
sem sfold_EOOModelBinding_EOOModelBinding f acc =
  | x ->
    (smapAccumL_EOOModelBinding_EOOModelBinding
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOModelBinding_EOOExpr : all a. (a -> EOOExpr -> (a, EOOExpr)) -> a -> EOOModelBinding -> (a, EOOModelBinding)
sem smapAccumL_EOOModelBinding_EOOExpr f acc =
  | x ->
    (acc, x)
  sem smap_EOOModelBinding_EOOExpr : (EOOExpr -> EOOExpr) -> EOOModelBinding -> EOOModelBinding
sem smap_EOOModelBinding_EOOExpr f =
  | x ->
    (smapAccumL_EOOModelBinding_EOOExpr
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOModelBinding_EOOExpr : all a. (a -> EOOExpr -> a) -> a -> EOOModelBinding -> a
sem sfold_EOOModelBinding_EOOExpr f acc =
  | x ->
    (smapAccumL_EOOModelBinding_EOOExpr
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOModelBinding_EOOPat : all a. (a -> EOOPat -> (a, EOOPat)) -> a -> EOOModelBinding -> (a, EOOModelBinding)
sem smapAccumL_EOOModelBinding_EOOPat f acc =
  | x ->
    (acc, x)
  sem smap_EOOModelBinding_EOOPat : (EOOPat -> EOOPat) -> EOOModelBinding -> EOOModelBinding
sem smap_EOOModelBinding_EOOPat f =
  | x ->
    (smapAccumL_EOOModelBinding_EOOPat
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOModelBinding_EOOPat : all a. (a -> EOOPat -> a) -> a -> EOOModelBinding -> a
sem sfold_EOOModelBinding_EOOPat f acc =
  | x ->
    (smapAccumL_EOOModelBinding_EOOPat
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOModelBinding_EOOType : all a. (a -> EOOType -> (a, EOOType)) -> a -> EOOModelBinding -> (a, EOOModelBinding)
sem smapAccumL_EOOModelBinding_EOOType f acc =
  | x ->
    (acc, x)
  sem smap_EOOModelBinding_EOOType : (EOOType -> EOOType) -> EOOModelBinding -> EOOModelBinding
sem smap_EOOModelBinding_EOOType f =
  | x ->
    (smapAccumL_EOOModelBinding_EOOType
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOModelBinding_EOOType : all a. (a -> EOOType -> a) -> a -> EOOModelBinding -> a
sem sfold_EOOModelBinding_EOOType f acc =
  | x ->
    (smapAccumL_EOOModelBinding_EOOType
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOModelBinding_EOOModelHead : all a. (a -> EOOModelHead -> (a, EOOModelHead)) -> a -> EOOModelBinding -> (a, EOOModelBinding)
sem smapAccumL_EOOModelBinding_EOOModelHead f acc =
  | x ->
    (acc, x)
  sem smap_EOOModelBinding_EOOModelHead : (EOOModelHead -> EOOModelHead) -> EOOModelBinding -> EOOModelBinding
sem smap_EOOModelBinding_EOOModelHead f =
  | x ->
    (smapAccumL_EOOModelBinding_EOOModelHead
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOModelBinding_EOOModelHead : all a. (a -> EOOModelHead -> a) -> a -> EOOModelBinding -> a
sem sfold_EOOModelBinding_EOOModelHead f acc =
  | x ->
    (smapAccumL_EOOModelBinding_EOOModelHead
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOExpr_EOOProg : all a. (a -> EOOProg -> (a, EOOProg)) -> a -> EOOExpr -> (a, EOOExpr)
sem smapAccumL_EOOExpr_EOOProg f acc =
  | x ->
    (acc, x)
  sem smap_EOOExpr_EOOProg : (EOOProg -> EOOProg) -> EOOExpr -> EOOExpr
sem smap_EOOExpr_EOOProg f =
  | x ->
    (smapAccumL_EOOExpr_EOOProg
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOExpr_EOOProg : all a. (a -> EOOProg -> a) -> a -> EOOExpr -> a
sem sfold_EOOExpr_EOOProg f acc =
  | x ->
    (smapAccumL_EOOExpr_EOOProg
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOExpr_EOOTop : all a. (a -> EOOTop -> (a, EOOTop)) -> a -> EOOExpr -> (a, EOOExpr)
sem smapAccumL_EOOExpr_EOOTop f acc =
  | x ->
    (acc, x)
  sem smap_EOOExpr_EOOTop : (EOOTop -> EOOTop) -> EOOExpr -> EOOExpr
sem smap_EOOExpr_EOOTop f =
  | x ->
    (smapAccumL_EOOExpr_EOOTop
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOExpr_EOOTop : all a. (a -> EOOTop -> a) -> a -> EOOExpr -> a
sem sfold_EOOExpr_EOOTop f acc =
  | x ->
    (smapAccumL_EOOExpr_EOOTop
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOExpr_EOOParam : all a. (a -> EOOParam -> (a, EOOParam)) -> a -> EOOExpr -> (a, EOOExpr)
sem smapAccumL_EOOExpr_EOOParam f acc =
  | x ->
    (acc, x)
  sem smap_EOOExpr_EOOParam : (EOOParam -> EOOParam) -> EOOExpr -> EOOExpr
sem smap_EOOExpr_EOOParam f =
  | x ->
    (smapAccumL_EOOExpr_EOOParam
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOExpr_EOOParam : all a. (a -> EOOParam -> a) -> a -> EOOExpr -> a
sem sfold_EOOExpr_EOOParam f acc =
  | x ->
    (smapAccumL_EOOExpr_EOOParam
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOExpr_EOODefBinding : all a. (a -> EOODefBinding -> (a, EOODefBinding)) -> a -> EOOExpr -> (a, EOOExpr)
sem smapAccumL_EOOExpr_EOODefBinding f acc =
  | x ->
    (acc, x)
  sem smap_EOOExpr_EOODefBinding : (EOODefBinding -> EOODefBinding) -> EOOExpr -> EOOExpr
sem smap_EOOExpr_EOODefBinding f =
  | x ->
    (smapAccumL_EOOExpr_EOODefBinding
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOExpr_EOODefBinding : all a. (a -> EOODefBinding -> a) -> a -> EOOExpr -> a
sem sfold_EOOExpr_EOODefBinding f acc =
  | x ->
    (smapAccumL_EOOExpr_EOODefBinding
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOExpr_EOOModelBinding : all a. (a -> EOOModelBinding -> (a, EOOModelBinding)) -> a -> EOOExpr -> (a, EOOExpr)
sem smapAccumL_EOOExpr_EOOModelBinding f acc =
  | x ->
    (acc, x)
  sem smap_EOOExpr_EOOModelBinding : (EOOModelBinding -> EOOModelBinding) -> EOOExpr -> EOOExpr
sem smap_EOOExpr_EOOModelBinding f =
  | x ->
    (smapAccumL_EOOExpr_EOOModelBinding
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOExpr_EOOModelBinding : all a. (a -> EOOModelBinding -> a) -> a -> EOOExpr -> a
sem sfold_EOOExpr_EOOModelBinding f acc =
  | x ->
    (smapAccumL_EOOExpr_EOOModelBinding
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOExpr_EOOExpr : all a. (a -> EOOExpr -> (a, EOOExpr)) -> a -> EOOExpr -> (a, EOOExpr)
sem smapAccumL_EOOExpr_EOOExpr f acc =
  | x ->
    (acc, x)
  sem smap_EOOExpr_EOOExpr : (EOOExpr -> EOOExpr) -> EOOExpr -> EOOExpr
sem smap_EOOExpr_EOOExpr f =
  | x ->
    (smapAccumL_EOOExpr_EOOExpr
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOExpr_EOOExpr : all a. (a -> EOOExpr -> a) -> a -> EOOExpr -> a
sem sfold_EOOExpr_EOOExpr f acc =
  | x ->
    (smapAccumL_EOOExpr_EOOExpr
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOExpr_EOOPat : all a. (a -> EOOPat -> (a, EOOPat)) -> a -> EOOExpr -> (a, EOOExpr)
sem smapAccumL_EOOExpr_EOOPat f acc =
  | x ->
    (acc, x)
  sem smap_EOOExpr_EOOPat : (EOOPat -> EOOPat) -> EOOExpr -> EOOExpr
sem smap_EOOExpr_EOOPat f =
  | x ->
    (smapAccumL_EOOExpr_EOOPat
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOExpr_EOOPat : all a. (a -> EOOPat -> a) -> a -> EOOExpr -> a
sem sfold_EOOExpr_EOOPat f acc =
  | x ->
    (smapAccumL_EOOExpr_EOOPat
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOExpr_EOOType : all a. (a -> EOOType -> (a, EOOType)) -> a -> EOOExpr -> (a, EOOExpr)
sem smapAccumL_EOOExpr_EOOType f acc =
  | x ->
    (acc, x)
  sem smap_EOOExpr_EOOType : (EOOType -> EOOType) -> EOOExpr -> EOOExpr
sem smap_EOOExpr_EOOType f =
  | x ->
    (smapAccumL_EOOExpr_EOOType
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOExpr_EOOType : all a. (a -> EOOType -> a) -> a -> EOOExpr -> a
sem sfold_EOOExpr_EOOType f acc =
  | x ->
    (smapAccumL_EOOExpr_EOOType
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOExpr_EOOModelHead : all a. (a -> EOOModelHead -> (a, EOOModelHead)) -> a -> EOOExpr -> (a, EOOExpr)
sem smapAccumL_EOOExpr_EOOModelHead f acc =
  | x ->
    (acc, x)
  sem smap_EOOExpr_EOOModelHead : (EOOModelHead -> EOOModelHead) -> EOOExpr -> EOOExpr
sem smap_EOOExpr_EOOModelHead f =
  | x ->
    (smapAccumL_EOOExpr_EOOModelHead
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOExpr_EOOModelHead : all a. (a -> EOOModelHead -> a) -> a -> EOOExpr -> a
sem sfold_EOOExpr_EOOModelHead f acc =
  | x ->
    (smapAccumL_EOOExpr_EOOModelHead
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOPat_EOOProg : all a. (a -> EOOProg -> (a, EOOProg)) -> a -> EOOPat -> (a, EOOPat)
sem smapAccumL_EOOPat_EOOProg f acc =
  | x ->
    (acc, x)
  sem smap_EOOPat_EOOProg : (EOOProg -> EOOProg) -> EOOPat -> EOOPat
sem smap_EOOPat_EOOProg f =
  | x ->
    (smapAccumL_EOOPat_EOOProg
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOPat_EOOProg : all a. (a -> EOOProg -> a) -> a -> EOOPat -> a
sem sfold_EOOPat_EOOProg f acc =
  | x ->
    (smapAccumL_EOOPat_EOOProg
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOPat_EOOTop : all a. (a -> EOOTop -> (a, EOOTop)) -> a -> EOOPat -> (a, EOOPat)
sem smapAccumL_EOOPat_EOOTop f acc =
  | x ->
    (acc, x)
  sem smap_EOOPat_EOOTop : (EOOTop -> EOOTop) -> EOOPat -> EOOPat
sem smap_EOOPat_EOOTop f =
  | x ->
    (smapAccumL_EOOPat_EOOTop
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOPat_EOOTop : all a. (a -> EOOTop -> a) -> a -> EOOPat -> a
sem sfold_EOOPat_EOOTop f acc =
  | x ->
    (smapAccumL_EOOPat_EOOTop
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOPat_EOOParam : all a. (a -> EOOParam -> (a, EOOParam)) -> a -> EOOPat -> (a, EOOPat)
sem smapAccumL_EOOPat_EOOParam f acc =
  | x ->
    (acc, x)
  sem smap_EOOPat_EOOParam : (EOOParam -> EOOParam) -> EOOPat -> EOOPat
sem smap_EOOPat_EOOParam f =
  | x ->
    (smapAccumL_EOOPat_EOOParam
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOPat_EOOParam : all a. (a -> EOOParam -> a) -> a -> EOOPat -> a
sem sfold_EOOPat_EOOParam f acc =
  | x ->
    (smapAccumL_EOOPat_EOOParam
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOPat_EOODefBinding : all a. (a -> EOODefBinding -> (a, EOODefBinding)) -> a -> EOOPat -> (a, EOOPat)
sem smapAccumL_EOOPat_EOODefBinding f acc =
  | x ->
    (acc, x)
  sem smap_EOOPat_EOODefBinding : (EOODefBinding -> EOODefBinding) -> EOOPat -> EOOPat
sem smap_EOOPat_EOODefBinding f =
  | x ->
    (smapAccumL_EOOPat_EOODefBinding
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOPat_EOODefBinding : all a. (a -> EOODefBinding -> a) -> a -> EOOPat -> a
sem sfold_EOOPat_EOODefBinding f acc =
  | x ->
    (smapAccumL_EOOPat_EOODefBinding
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOPat_EOOModelBinding : all a. (a -> EOOModelBinding -> (a, EOOModelBinding)) -> a -> EOOPat -> (a, EOOPat)
sem smapAccumL_EOOPat_EOOModelBinding f acc =
  | x ->
    (acc, x)
  sem smap_EOOPat_EOOModelBinding : (EOOModelBinding -> EOOModelBinding) -> EOOPat -> EOOPat
sem smap_EOOPat_EOOModelBinding f =
  | x ->
    (smapAccumL_EOOPat_EOOModelBinding
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOPat_EOOModelBinding : all a. (a -> EOOModelBinding -> a) -> a -> EOOPat -> a
sem sfold_EOOPat_EOOModelBinding f acc =
  | x ->
    (smapAccumL_EOOPat_EOOModelBinding
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOPat_EOOExpr : all a. (a -> EOOExpr -> (a, EOOExpr)) -> a -> EOOPat -> (a, EOOPat)
sem smapAccumL_EOOPat_EOOExpr f acc =
  | x ->
    (acc, x)
  sem smap_EOOPat_EOOExpr : (EOOExpr -> EOOExpr) -> EOOPat -> EOOPat
sem smap_EOOPat_EOOExpr f =
  | x ->
    (smapAccumL_EOOPat_EOOExpr
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOPat_EOOExpr : all a. (a -> EOOExpr -> a) -> a -> EOOPat -> a
sem sfold_EOOPat_EOOExpr f acc =
  | x ->
    (smapAccumL_EOOPat_EOOExpr
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOPat_EOOPat : all a. (a -> EOOPat -> (a, EOOPat)) -> a -> EOOPat -> (a, EOOPat)
sem smapAccumL_EOOPat_EOOPat f acc =
  | x ->
    (acc, x)
  sem smap_EOOPat_EOOPat : (EOOPat -> EOOPat) -> EOOPat -> EOOPat
sem smap_EOOPat_EOOPat f =
  | x ->
    (smapAccumL_EOOPat_EOOPat
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOPat_EOOPat : all a. (a -> EOOPat -> a) -> a -> EOOPat -> a
sem sfold_EOOPat_EOOPat f acc =
  | x ->
    (smapAccumL_EOOPat_EOOPat
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOPat_EOOType : all a. (a -> EOOType -> (a, EOOType)) -> a -> EOOPat -> (a, EOOPat)
sem smapAccumL_EOOPat_EOOType f acc =
  | x ->
    (acc, x)
  sem smap_EOOPat_EOOType : (EOOType -> EOOType) -> EOOPat -> EOOPat
sem smap_EOOPat_EOOType f =
  | x ->
    (smapAccumL_EOOPat_EOOType
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOPat_EOOType : all a. (a -> EOOType -> a) -> a -> EOOPat -> a
sem sfold_EOOPat_EOOType f acc =
  | x ->
    (smapAccumL_EOOPat_EOOType
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOPat_EOOModelHead : all a. (a -> EOOModelHead -> (a, EOOModelHead)) -> a -> EOOPat -> (a, EOOPat)
sem smapAccumL_EOOPat_EOOModelHead f acc =
  | x ->
    (acc, x)
  sem smap_EOOPat_EOOModelHead : (EOOModelHead -> EOOModelHead) -> EOOPat -> EOOPat
sem smap_EOOPat_EOOModelHead f =
  | x ->
    (smapAccumL_EOOPat_EOOModelHead
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOPat_EOOModelHead : all a. (a -> EOOModelHead -> a) -> a -> EOOPat -> a
sem sfold_EOOPat_EOOModelHead f acc =
  | x ->
    (smapAccumL_EOOPat_EOOModelHead
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOType_EOOProg : all a. (a -> EOOProg -> (a, EOOProg)) -> a -> EOOType -> (a, EOOType)
sem smapAccumL_EOOType_EOOProg f acc =
  | x ->
    (acc, x)
  sem smap_EOOType_EOOProg : (EOOProg -> EOOProg) -> EOOType -> EOOType
sem smap_EOOType_EOOProg f =
  | x ->
    (smapAccumL_EOOType_EOOProg
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOType_EOOProg : all a. (a -> EOOProg -> a) -> a -> EOOType -> a
sem sfold_EOOType_EOOProg f acc =
  | x ->
    (smapAccumL_EOOType_EOOProg
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOType_EOOTop : all a. (a -> EOOTop -> (a, EOOTop)) -> a -> EOOType -> (a, EOOType)
sem smapAccumL_EOOType_EOOTop f acc =
  | x ->
    (acc, x)
  sem smap_EOOType_EOOTop : (EOOTop -> EOOTop) -> EOOType -> EOOType
sem smap_EOOType_EOOTop f =
  | x ->
    (smapAccumL_EOOType_EOOTop
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOType_EOOTop : all a. (a -> EOOTop -> a) -> a -> EOOType -> a
sem sfold_EOOType_EOOTop f acc =
  | x ->
    (smapAccumL_EOOType_EOOTop
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOType_EOOParam : all a. (a -> EOOParam -> (a, EOOParam)) -> a -> EOOType -> (a, EOOType)
sem smapAccumL_EOOType_EOOParam f acc =
  | x ->
    (acc, x)
  sem smap_EOOType_EOOParam : (EOOParam -> EOOParam) -> EOOType -> EOOType
sem smap_EOOType_EOOParam f =
  | x ->
    (smapAccumL_EOOType_EOOParam
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOType_EOOParam : all a. (a -> EOOParam -> a) -> a -> EOOType -> a
sem sfold_EOOType_EOOParam f acc =
  | x ->
    (smapAccumL_EOOType_EOOParam
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOType_EOODefBinding : all a. (a -> EOODefBinding -> (a, EOODefBinding)) -> a -> EOOType -> (a, EOOType)
sem smapAccumL_EOOType_EOODefBinding f acc =
  | x ->
    (acc, x)
  sem smap_EOOType_EOODefBinding : (EOODefBinding -> EOODefBinding) -> EOOType -> EOOType
sem smap_EOOType_EOODefBinding f =
  | x ->
    (smapAccumL_EOOType_EOODefBinding
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOType_EOODefBinding : all a. (a -> EOODefBinding -> a) -> a -> EOOType -> a
sem sfold_EOOType_EOODefBinding f acc =
  | x ->
    (smapAccumL_EOOType_EOODefBinding
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOType_EOOModelBinding : all a. (a -> EOOModelBinding -> (a, EOOModelBinding)) -> a -> EOOType -> (a, EOOType)
sem smapAccumL_EOOType_EOOModelBinding f acc =
  | x ->
    (acc, x)
  sem smap_EOOType_EOOModelBinding : (EOOModelBinding -> EOOModelBinding) -> EOOType -> EOOType
sem smap_EOOType_EOOModelBinding f =
  | x ->
    (smapAccumL_EOOType_EOOModelBinding
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOType_EOOModelBinding : all a. (a -> EOOModelBinding -> a) -> a -> EOOType -> a
sem sfold_EOOType_EOOModelBinding f acc =
  | x ->
    (smapAccumL_EOOType_EOOModelBinding
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOType_EOOExpr : all a. (a -> EOOExpr -> (a, EOOExpr)) -> a -> EOOType -> (a, EOOType)
sem smapAccumL_EOOType_EOOExpr f acc =
  | x ->
    (acc, x)
  sem smap_EOOType_EOOExpr : (EOOExpr -> EOOExpr) -> EOOType -> EOOType
sem smap_EOOType_EOOExpr f =
  | x ->
    (smapAccumL_EOOType_EOOExpr
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOType_EOOExpr : all a. (a -> EOOExpr -> a) -> a -> EOOType -> a
sem sfold_EOOType_EOOExpr f acc =
  | x ->
    (smapAccumL_EOOType_EOOExpr
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOType_EOOPat : all a. (a -> EOOPat -> (a, EOOPat)) -> a -> EOOType -> (a, EOOType)
sem smapAccumL_EOOType_EOOPat f acc =
  | x ->
    (acc, x)
  sem smap_EOOType_EOOPat : (EOOPat -> EOOPat) -> EOOType -> EOOType
sem smap_EOOType_EOOPat f =
  | x ->
    (smapAccumL_EOOType_EOOPat
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOType_EOOPat : all a. (a -> EOOPat -> a) -> a -> EOOType -> a
sem sfold_EOOType_EOOPat f acc =
  | x ->
    (smapAccumL_EOOType_EOOPat
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOType_EOOType : all a. (a -> EOOType -> (a, EOOType)) -> a -> EOOType -> (a, EOOType)
sem smapAccumL_EOOType_EOOType f acc =
  | x ->
    (acc, x)
  sem smap_EOOType_EOOType : (EOOType -> EOOType) -> EOOType -> EOOType
sem smap_EOOType_EOOType f =
  | x ->
    (smapAccumL_EOOType_EOOType
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOType_EOOType : all a. (a -> EOOType -> a) -> a -> EOOType -> a
sem sfold_EOOType_EOOType f acc =
  | x ->
    (smapAccumL_EOOType_EOOType
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOType_EOOModelHead : all a. (a -> EOOModelHead -> (a, EOOModelHead)) -> a -> EOOType -> (a, EOOType)
sem smapAccumL_EOOType_EOOModelHead f acc =
  | x ->
    (acc, x)
  sem smap_EOOType_EOOModelHead : (EOOModelHead -> EOOModelHead) -> EOOType -> EOOType
sem smap_EOOType_EOOModelHead f =
  | x ->
    (smapAccumL_EOOType_EOOModelHead
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOType_EOOModelHead : all a. (a -> EOOModelHead -> a) -> a -> EOOType -> a
sem sfold_EOOType_EOOModelHead f acc =
  | x ->
    (smapAccumL_EOOType_EOOModelHead
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOModelHead_EOOProg : all a. (a -> EOOProg -> (a, EOOProg)) -> a -> EOOModelHead -> (a, EOOModelHead)
sem smapAccumL_EOOModelHead_EOOProg f acc =
  | x ->
    (acc, x)
  sem smap_EOOModelHead_EOOProg : (EOOProg -> EOOProg) -> EOOModelHead -> EOOModelHead
sem smap_EOOModelHead_EOOProg f =
  | x ->
    (smapAccumL_EOOModelHead_EOOProg
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOModelHead_EOOProg : all a. (a -> EOOProg -> a) -> a -> EOOModelHead -> a
sem sfold_EOOModelHead_EOOProg f acc =
  | x ->
    (smapAccumL_EOOModelHead_EOOProg
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOModelHead_EOOTop : all a. (a -> EOOTop -> (a, EOOTop)) -> a -> EOOModelHead -> (a, EOOModelHead)
sem smapAccumL_EOOModelHead_EOOTop f acc =
  | x ->
    (acc, x)
  sem smap_EOOModelHead_EOOTop : (EOOTop -> EOOTop) -> EOOModelHead -> EOOModelHead
sem smap_EOOModelHead_EOOTop f =
  | x ->
    (smapAccumL_EOOModelHead_EOOTop
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOModelHead_EOOTop : all a. (a -> EOOTop -> a) -> a -> EOOModelHead -> a
sem sfold_EOOModelHead_EOOTop f acc =
  | x ->
    (smapAccumL_EOOModelHead_EOOTop
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOModelHead_EOOParam : all a. (a -> EOOParam -> (a, EOOParam)) -> a -> EOOModelHead -> (a, EOOModelHead)
sem smapAccumL_EOOModelHead_EOOParam f acc =
  | x ->
    (acc, x)
  sem smap_EOOModelHead_EOOParam : (EOOParam -> EOOParam) -> EOOModelHead -> EOOModelHead
sem smap_EOOModelHead_EOOParam f =
  | x ->
    (smapAccumL_EOOModelHead_EOOParam
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOModelHead_EOOParam : all a. (a -> EOOParam -> a) -> a -> EOOModelHead -> a
sem sfold_EOOModelHead_EOOParam f acc =
  | x ->
    (smapAccumL_EOOModelHead_EOOParam
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOModelHead_EOODefBinding : all a. (a -> EOODefBinding -> (a, EOODefBinding)) -> a -> EOOModelHead -> (a, EOOModelHead)
sem smapAccumL_EOOModelHead_EOODefBinding f acc =
  | x ->
    (acc, x)
  sem smap_EOOModelHead_EOODefBinding : (EOODefBinding -> EOODefBinding) -> EOOModelHead -> EOOModelHead
sem smap_EOOModelHead_EOODefBinding f =
  | x ->
    (smapAccumL_EOOModelHead_EOODefBinding
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOModelHead_EOODefBinding : all a. (a -> EOODefBinding -> a) -> a -> EOOModelHead -> a
sem sfold_EOOModelHead_EOODefBinding f acc =
  | x ->
    (smapAccumL_EOOModelHead_EOODefBinding
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOModelHead_EOOModelBinding : all a. (a -> EOOModelBinding -> (a, EOOModelBinding)) -> a -> EOOModelHead -> (a, EOOModelHead)
sem smapAccumL_EOOModelHead_EOOModelBinding f acc =
  | x ->
    (acc, x)
  sem smap_EOOModelHead_EOOModelBinding : (EOOModelBinding -> EOOModelBinding) -> EOOModelHead -> EOOModelHead
sem smap_EOOModelHead_EOOModelBinding f =
  | x ->
    (smapAccumL_EOOModelHead_EOOModelBinding
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOModelHead_EOOModelBinding : all a. (a -> EOOModelBinding -> a) -> a -> EOOModelHead -> a
sem sfold_EOOModelHead_EOOModelBinding f acc =
  | x ->
    (smapAccumL_EOOModelHead_EOOModelBinding
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOModelHead_EOOExpr : all a. (a -> EOOExpr -> (a, EOOExpr)) -> a -> EOOModelHead -> (a, EOOModelHead)
sem smapAccumL_EOOModelHead_EOOExpr f acc =
  | x ->
    (acc, x)
  sem smap_EOOModelHead_EOOExpr : (EOOExpr -> EOOExpr) -> EOOModelHead -> EOOModelHead
sem smap_EOOModelHead_EOOExpr f =
  | x ->
    (smapAccumL_EOOModelHead_EOOExpr
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOModelHead_EOOExpr : all a. (a -> EOOExpr -> a) -> a -> EOOModelHead -> a
sem sfold_EOOModelHead_EOOExpr f acc =
  | x ->
    (smapAccumL_EOOModelHead_EOOExpr
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOModelHead_EOOPat : all a. (a -> EOOPat -> (a, EOOPat)) -> a -> EOOModelHead -> (a, EOOModelHead)
sem smapAccumL_EOOModelHead_EOOPat f acc =
  | x ->
    (acc, x)
  sem smap_EOOModelHead_EOOPat : (EOOPat -> EOOPat) -> EOOModelHead -> EOOModelHead
sem smap_EOOModelHead_EOOPat f =
  | x ->
    (smapAccumL_EOOModelHead_EOOPat
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOModelHead_EOOPat : all a. (a -> EOOPat -> a) -> a -> EOOModelHead -> a
sem sfold_EOOModelHead_EOOPat f acc =
  | x ->
    (smapAccumL_EOOModelHead_EOOPat
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOModelHead_EOOType : all a. (a -> EOOType -> (a, EOOType)) -> a -> EOOModelHead -> (a, EOOModelHead)
sem smapAccumL_EOOModelHead_EOOType f acc =
  | x ->
    (acc, x)
  sem smap_EOOModelHead_EOOType : (EOOType -> EOOType) -> EOOModelHead -> EOOModelHead
sem smap_EOOModelHead_EOOType f =
  | x ->
    (smapAccumL_EOOModelHead_EOOType
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOModelHead_EOOType : all a. (a -> EOOType -> a) -> a -> EOOModelHead -> a
sem sfold_EOOModelHead_EOOType f acc =
  | x ->
    (smapAccumL_EOOModelHead_EOOType
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem smapAccumL_EOOModelHead_EOOModelHead : all a. (a -> EOOModelHead -> (a, EOOModelHead)) -> a -> EOOModelHead -> (a, EOOModelHead)
sem smapAccumL_EOOModelHead_EOOModelHead f acc =
  | x ->
    (acc, x)
  sem smap_EOOModelHead_EOOModelHead : (EOOModelHead -> EOOModelHead) -> EOOModelHead -> EOOModelHead
sem smap_EOOModelHead_EOOModelHead f =
  | x ->
    (smapAccumL_EOOModelHead_EOOModelHead
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).1
  sem sfold_EOOModelHead_EOOModelHead : all a. (a -> EOOModelHead -> a) -> a -> EOOModelHead -> a
sem sfold_EOOModelHead_EOOModelHead f acc =
  | x ->
    (smapAccumL_EOOModelHead_EOOModelHead
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).0
  sem get_EOOProg_info : EOOProg -> Info
  sem set_EOOProg_info : Info -> EOOProg -> EOOProg
sem set_EOOProg_info val =
  sem mapAccum_EOOProg_info : all a. (a -> Info -> (a, Info)) -> a -> EOOProg -> (a, EOOProg)
sem mapAccum_EOOProg_info f acc =
  | target ->
    match
      f
        acc
        (get_EOOProg_info
           target)
    with
      (acc, val)
    in
    (acc, set_EOOProg_info
        val
        target)
  sem map_EOOProg_info : (Info -> Info) -> EOOProg -> EOOProg
sem map_EOOProg_info f =
  | target ->
    set_EOOProg_info
      (f
         (get_EOOProg_info
            target))
      target
  sem get_EOOTop_info : EOOTop -> Info
  sem set_EOOTop_info : Info -> EOOTop -> EOOTop
sem set_EOOTop_info val =
  sem mapAccum_EOOTop_info : all a. (a -> Info -> (a, Info)) -> a -> EOOTop -> (a, EOOTop)
sem mapAccum_EOOTop_info f acc =
  | target ->
    match
      f
        acc
        (get_EOOTop_info
           target)
    with
      (acc, val)
    in
    (acc, set_EOOTop_info
        val
        target)
  sem map_EOOTop_info : (Info -> Info) -> EOOTop -> EOOTop
sem map_EOOTop_info f =
  | target ->
    set_EOOTop_info
      (f
         (get_EOOTop_info
            target))
      target
  sem get_EOOParam_info : EOOParam -> Info
  sem set_EOOParam_info : Info -> EOOParam -> EOOParam
sem set_EOOParam_info val =
  sem mapAccum_EOOParam_info : all a. (a -> Info -> (a, Info)) -> a -> EOOParam -> (a, EOOParam)
sem mapAccum_EOOParam_info f acc =
  | target ->
    match
      f
        acc
        (get_EOOParam_info
           target)
    with
      (acc, val)
    in
    (acc, set_EOOParam_info
        val
        target)
  sem map_EOOParam_info : (Info -> Info) -> EOOParam -> EOOParam
sem map_EOOParam_info f =
  | target ->
    set_EOOParam_info
      (f
         (get_EOOParam_info
            target))
      target
  sem get_EOODefBinding_info : EOODefBinding -> Info
  sem set_EOODefBinding_info : Info -> EOODefBinding -> EOODefBinding
sem set_EOODefBinding_info val =
  sem mapAccum_EOODefBinding_info : all a. (a -> Info -> (a, Info)) -> a -> EOODefBinding -> (a, EOODefBinding)
sem mapAccum_EOODefBinding_info f acc =
  | target ->
    match
      f
        acc
        (get_EOODefBinding_info
           target)
    with
      (acc, val)
    in
    (acc, set_EOODefBinding_info
        val
        target)
  sem map_EOODefBinding_info : (Info -> Info) -> EOODefBinding -> EOODefBinding
sem map_EOODefBinding_info f =
  | target ->
    set_EOODefBinding_info
      (f
         (get_EOODefBinding_info
            target))
      target
  sem get_EOOModelBinding_info : EOOModelBinding -> Info
  sem set_EOOModelBinding_info : Info -> EOOModelBinding -> EOOModelBinding
sem set_EOOModelBinding_info val =
  sem mapAccum_EOOModelBinding_info : all a. (a -> Info -> (a, Info)) -> a -> EOOModelBinding -> (a, EOOModelBinding)
sem mapAccum_EOOModelBinding_info f acc =
  | target ->
    match
      f
        acc
        (get_EOOModelBinding_info
           target)
    with
      (acc, val)
    in
    (acc, set_EOOModelBinding_info
        val
        target)
  sem map_EOOModelBinding_info : (Info -> Info) -> EOOModelBinding -> EOOModelBinding
sem map_EOOModelBinding_info f =
  | target ->
    set_EOOModelBinding_info
      (f
         (get_EOOModelBinding_info
            target))
      target
  sem get_EOOExpr_info : EOOExpr -> Info
  sem set_EOOExpr_info : Info -> EOOExpr -> EOOExpr
sem set_EOOExpr_info val =
  sem mapAccum_EOOExpr_info : all a. (a -> Info -> (a, Info)) -> a -> EOOExpr -> (a, EOOExpr)
sem mapAccum_EOOExpr_info f acc =
  | target ->
    match
      f
        acc
        (get_EOOExpr_info
           target)
    with
      (acc, val)
    in
    (acc, set_EOOExpr_info
        val
        target)
  sem map_EOOExpr_info : (Info -> Info) -> EOOExpr -> EOOExpr
sem map_EOOExpr_info f =
  | target ->
    set_EOOExpr_info
      (f
         (get_EOOExpr_info
            target))
      target
  sem get_EOOPat_info : EOOPat -> Info
  sem set_EOOPat_info : Info -> EOOPat -> EOOPat
sem set_EOOPat_info val =
  sem mapAccum_EOOPat_info : all a. (a -> Info -> (a, Info)) -> a -> EOOPat -> (a, EOOPat)
sem mapAccum_EOOPat_info f acc =
  | target ->
    match
      f
        acc
        (get_EOOPat_info
           target)
    with
      (acc, val)
    in
    (acc, set_EOOPat_info
        val
        target)
  sem map_EOOPat_info : (Info -> Info) -> EOOPat -> EOOPat
sem map_EOOPat_info f =
  | target ->
    set_EOOPat_info
      (f
         (get_EOOPat_info
            target))
      target
  sem get_EOOType_info : EOOType -> Info
  sem set_EOOType_info : Info -> EOOType -> EOOType
sem set_EOOType_info val =
  sem mapAccum_EOOType_info : all a. (a -> Info -> (a, Info)) -> a -> EOOType -> (a, EOOType)
sem mapAccum_EOOType_info f acc =
  | target ->
    match
      f
        acc
        (get_EOOType_info
           target)
    with
      (acc, val)
    in
    (acc, set_EOOType_info
        val
        target)
  sem map_EOOType_info : (Info -> Info) -> EOOType -> EOOType
sem map_EOOType_info f =
  | target ->
    set_EOOType_info
      (f
         (get_EOOType_info
            target))
      target
  sem get_EOOModelHead_info : EOOModelHead -> Info
  sem set_EOOModelHead_info : Info -> EOOModelHead -> EOOModelHead
sem set_EOOModelHead_info val =
  sem mapAccum_EOOModelHead_info : all a. (a -> Info -> (a, Info)) -> a -> EOOModelHead -> (a, EOOModelHead)
sem mapAccum_EOOModelHead_info f acc =
  | target ->
    match
      f
        acc
        (get_EOOModelHead_info
           target)
    with
      (acc, val)
    in
    (acc, set_EOOModelHead_info
        val
        target)
  sem map_EOOModelHead_info : (Info -> Info) -> EOOModelHead -> EOOModelHead
sem map_EOOModelHead_info f =
  | target ->
    set_EOOModelHead_info
      (f
         (get_EOOModelHead_info
            target))
      target
end
lang ProgEOOProgAst =
  EOOBaseAst
  type ProgEOOProgRecord =
    {eqn: EOOExpr, info: Info, tops: [EOOTop], eqnkw: Info, heads: [EOOModelHead], output: EOOExpr}
  syn EOOProg =
  | ProgEOOProg ProgEOOProgRecord
  sem smapAccumL_EOOProg_EOOTop f acc =
  | ProgEOOProg x ->
    match
      match
        let tops =
          x.tops
        in
        mapAccumL
          (lam acc1.
             lam x1: EOOTop.
               f
                 acc1
                 x1)
          acc
          tops
      with
        (acc, tops)
      in
      (acc, { x
          with
          tops =
            tops })
    with
      (acc, x)
    in
    (acc, ProgEOOProg
        x)
  sem smapAccumL_EOOProg_EOOExpr f acc =
  | ProgEOOProg x ->
    match
      match
        let eqn =
          x.eqn
        in
        f
          acc
          eqn
      with
        (acc, eqn)
      in
      match
          let output =
            x.output
          in
          f
            acc
            output
        with
          (acc, output)
        in
        (acc, { { x
              with
              eqn =
                eqn }
            with
            output =
              output })
    with
      (acc, x)
    in
    (acc, ProgEOOProg
        x)
  sem smapAccumL_EOOProg_EOOModelHead f acc =
  | ProgEOOProg x ->
    match
      match
        let heads =
          x.heads
        in
        mapAccumL
          (lam acc1.
             lam x1: EOOModelHead.
               f
                 acc1
                 x1)
          acc
          heads
      with
        (acc, heads)
      in
      (acc, { x
          with
          heads =
            heads })
    with
      (acc, x)
    in
    (acc, ProgEOOProg
        x)
  sem get_EOOProg_info =
  | ProgEOOProg target ->
    target.info
  sem set_EOOProg_info val =
  | ProgEOOProg target ->
    ProgEOOProg
      { target
        with
        info =
          val }
end
lang DefEOOTopAst =
  EOOBaseAst
  type DefEOOTopRecord =
    {info: Info, binding: EOODefBinding}
  syn EOOTop =
  | DefEOOTop DefEOOTopRecord
  sem smapAccumL_EOOTop_EOODefBinding f acc =
  | DefEOOTop x ->
    match
      match
        let binding =
          x.binding
        in
        f
          acc
          binding
      with
        (acc, binding)
      in
      (acc, { x
          with
          binding =
            binding })
    with
      (acc, x)
    in
    (acc, DefEOOTop
        x)
  sem get_EOOTop_info =
  | DefEOOTop target ->
    target.info
  sem set_EOOTop_info val =
  | DefEOOTop target ->
    DefEOOTop
      { target
        with
        info =
          val }
end
lang ModelEOOTopAst =
  EOOBaseAst
  type ModelEOOTopRecord =
    {info: Info, binding: EOOModelBinding}
  syn EOOTop =
  | ModelEOOTop ModelEOOTopRecord
  sem smapAccumL_EOOTop_EOOModelBinding f acc =
  | ModelEOOTop x ->
    match
      match
        let binding =
          x.binding
        in
        f
          acc
          binding
      with
        (acc, binding)
      in
      (acc, { x
          with
          binding =
            binding })
    with
      (acc, x)
    in
    (acc, ModelEOOTop
        x)
  sem get_EOOTop_info =
  | ModelEOOTop target ->
    target.info
  sem set_EOOTop_info val =
  | ModelEOOTop target ->
    ModelEOOTop
      { target
        with
        info =
          val }
end
lang TypeEOOTopAst =
  EOOBaseAst
  type TypeEOOTopRecord =
    {n: {i: Info, v: Name}, rhs: EOOType, info: Info, params: [{i: Info, v: Name}]}
  syn EOOTop =
  | TypeEOOTop TypeEOOTopRecord
  sem smapAccumL_EOOTop_EOOType f acc =
  | TypeEOOTop x ->
    match
      match
        let rhs =
          x.rhs
        in
        f
          acc
          rhs
      with
        (acc, rhs)
      in
      (acc, { x
          with
          rhs =
            rhs })
    with
      (acc, x)
    in
    (acc, TypeEOOTop
        x)
  sem get_EOOTop_info =
  | TypeEOOTop target ->
    target.info
  sem set_EOOTop_info val =
  | TypeEOOTop target ->
    TypeEOOTop
      { target
        with
        info =
          val }
end
lang PatEOOParamAst =
  EOOBaseAst
  type PatEOOParamRecord =
    {pat: EOOPat, info: Info}
  syn EOOParam =
  | PatEOOParam PatEOOParamRecord
  sem smapAccumL_EOOParam_EOOPat f acc =
  | PatEOOParam x ->
    match
      match
        let pat =
          x.pat
        in
        f
          acc
          pat
      with
        (acc, pat)
      in
      (acc, { x
          with
          pat =
            pat })
    with
      (acc, x)
    in
    (acc, PatEOOParam
        x)
  sem get_EOOParam_info =
  | PatEOOParam target ->
    target.info
  sem set_EOOParam_info val =
  | PatEOOParam target ->
    PatEOOParam
      { target
        with
        info =
          val }
end
lang NameEOOParamAst =
  EOOBaseAst
  type NameEOOParamRecord =
    {n: {i: Info, v: Name}, info: Info}
  syn EOOParam =
  | NameEOOParam NameEOOParamRecord
  sem get_EOOParam_info =
  | NameEOOParam target ->
    target.info
  sem set_EOOParam_info val =
  | NameEOOParam target ->
    NameEOOParam
      { target
        with
        info =
          val }
end
lang IgnoreEOOParamAst =
  EOOBaseAst
  type IgnoreEOOParamRecord =
    {info: Info}
  syn EOOParam =
  | IgnoreEOOParam IgnoreEOOParamRecord
  sem get_EOOParam_info =
  | IgnoreEOOParam target ->
    target.info
  sem set_EOOParam_info val =
  | IgnoreEOOParam target ->
    IgnoreEOOParam
      { target
        with
        info =
          val }
end
lang SimpleEOODefBindingAst =
  EOOBaseAst
  type SimpleEOODefBindingRecord =
    {n: {i: Info, v: Name}, n2: Option {i: Info, v: Name}, ty: Option EOOType, rhs: EOOExpr, info: Info, params: [EOOParam]}
  syn EOODefBinding =
  | SimpleEOODefBinding SimpleEOODefBindingRecord
  sem smapAccumL_EOODefBinding_EOOParam f acc =
  | SimpleEOODefBinding x ->
    match
      match
        let params =
          x.params
        in
        mapAccumL
          (lam acc1.
             lam x1: EOOParam.
               f
                 acc1
                 x1)
          acc
          params
      with
        (acc, params)
      in
      (acc, { x
          with
          params =
            params })
    with
      (acc, x)
    in
    (acc, SimpleEOODefBinding
        x)
  sem smapAccumL_EOODefBinding_EOOExpr f acc =
  | SimpleEOODefBinding x ->
    match
      match
        let rhs =
          x.rhs
        in
        f
          acc
          rhs
      with
        (acc, rhs)
      in
      (acc, { x
          with
          rhs =
            rhs })
    with
      (acc, x)
    in
    (acc, SimpleEOODefBinding
        x)
  sem smapAccumL_EOODefBinding_EOOType f acc =
  | SimpleEOODefBinding x ->
    match
      match
        let ty =
          x.ty
        in
        optionMapAccum
          (lam acc1.
             lam x1.
               f
                 acc1
                 x1)
          acc
          ty
      with
        (acc, ty)
      in
      (acc, { x
          with
          ty =
            ty })
    with
      (acc, x)
    in
    (acc, SimpleEOODefBinding
        x)
  sem get_EOODefBinding_info =
  | SimpleEOODefBinding target ->
    target.info
  sem set_EOODefBinding_info val =
  | SimpleEOODefBinding target ->
    SimpleEOODefBinding
      { target
        with
        info =
          val }
end
lang SimpleEOOModelBindingAst =
  EOOBaseAst
  type SimpleEOOModelBindingRecord =
    {n: {i: Info, v: Name}, n2: Option {i: Info, v: Name}, ty: Option EOOType, eqn: EOOExpr, info: Info, eqnkw: Info, heads: [EOOModelHead], params: [EOOParam]}
  syn EOOModelBinding =
  | SimpleEOOModelBinding SimpleEOOModelBindingRecord
  sem smapAccumL_EOOModelBinding_EOOParam f acc =
  | SimpleEOOModelBinding x ->
    match
      match
        let params =
          x.params
        in
        mapAccumL
          (lam acc1.
             lam x1: EOOParam.
               f
                 acc1
                 x1)
          acc
          params
      with
        (acc, params)
      in
      (acc, { x
          with
          params =
            params })
    with
      (acc, x)
    in
    (acc, SimpleEOOModelBinding
        x)
  sem smapAccumL_EOOModelBinding_EOOExpr f acc =
  | SimpleEOOModelBinding x ->
    match
      match
        let eqn =
          x.eqn
        in
        f
          acc
          eqn
      with
        (acc, eqn)
      in
      (acc, { x
          with
          eqn =
            eqn })
    with
      (acc, x)
    in
    (acc, SimpleEOOModelBinding
        x)
  sem smapAccumL_EOOModelBinding_EOOType f acc =
  | SimpleEOOModelBinding x ->
    match
      match
        let ty =
          x.ty
        in
        optionMapAccum
          (lam acc1.
             lam x1.
               f
                 acc1
                 x1)
          acc
          ty
      with
        (acc, ty)
      in
      (acc, { x
          with
          ty =
            ty })
    with
      (acc, x)
    in
    (acc, SimpleEOOModelBinding
        x)
  sem smapAccumL_EOOModelBinding_EOOModelHead f acc =
  | SimpleEOOModelBinding x ->
    match
      match
        let heads =
          x.heads
        in
        mapAccumL
          (lam acc1.
             lam x1: EOOModelHead.
               f
                 acc1
                 x1)
          acc
          heads
      with
        (acc, heads)
      in
      (acc, { x
          with
          heads =
            heads })
    with
      (acc, x)
    in
    (acc, SimpleEOOModelBinding
        x)
  sem get_EOOModelBinding_info =
  | SimpleEOOModelBinding target ->
    target.info
  sem set_EOOModelBinding_info val =
  | SimpleEOOModelBinding target ->
    SimpleEOOModelBinding
      { target
        with
        info =
          val }
end
lang VarEOOExprAst =
  EOOBaseAst
  type VarEOOExprRecord =
    {n: {i: Info, v: Name}, info: Info}
  syn EOOExpr =
  | VarEOOExpr VarEOOExprRecord
  sem get_EOOExpr_info =
  | VarEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | VarEOOExpr target ->
    VarEOOExpr
      { target
        with
        info =
          val }
end
lang AbsEOOExprAst =
  EOOBaseAst
  type AbsEOOExprRecord =
    {ty: Option EOOType, body: EOOExpr, info: Info, param: EOOParam}
  syn EOOExpr =
  | AbsEOOExpr AbsEOOExprRecord
  sem smapAccumL_EOOExpr_EOOParam f acc =
  | AbsEOOExpr x ->
    match
      match
        let param =
          x.param
        in
        f
          acc
          param
      with
        (acc, param)
      in
      (acc, { x
          with
          param =
            param })
    with
      (acc, x)
    in
    (acc, AbsEOOExpr
        x)
  sem smapAccumL_EOOExpr_EOOExpr f acc =
  | AbsEOOExpr x ->
    match
      match
        let body =
          x.body
        in
        f
          acc
          body
      with
        (acc, body)
      in
      (acc, { x
          with
          body =
            body })
    with
      (acc, x)
    in
    (acc, AbsEOOExpr
        x)
  sem smapAccumL_EOOExpr_EOOType f acc =
  | AbsEOOExpr x ->
    match
      match
        let ty =
          x.ty
        in
        optionMapAccum
          (lam acc1.
             lam x1.
               f
                 acc1
                 x1)
          acc
          ty
      with
        (acc, ty)
      in
      (acc, { x
          with
          ty =
            ty })
    with
      (acc, x)
    in
    (acc, AbsEOOExpr
        x)
  sem get_EOOExpr_info =
  | AbsEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | AbsEOOExpr target ->
    AbsEOOExpr
      { target
        with
        info =
          val }
end
lang AppEOOExprAst =
  EOOBaseAst
  type AppEOOExprRecord =
    {info: Info, left: EOOExpr, right: EOOExpr}
  syn EOOExpr =
  | AppEOOExpr AppEOOExprRecord
  sem smapAccumL_EOOExpr_EOOExpr f acc =
  | AppEOOExpr x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      in
      match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        in
        (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
    with
      (acc, x)
    in
    (acc, AppEOOExpr
        x)
  sem get_EOOExpr_info =
  | AppEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | AppEOOExpr target ->
    AppEOOExpr
      { target
        with
        info =
          val }
end
lang NumEOOExprAst =
  EOOBaseAst
  type NumEOOExprRecord =
    {f: Option {i: Info, v: Float}, i: Option {i: Info, v: Int}, info: Info}
  syn EOOExpr =
  | NumEOOExpr NumEOOExprRecord
  sem get_EOOExpr_info =
  | NumEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | NumEOOExpr target ->
    NumEOOExpr
      { target
        with
        info =
          val }
end
lang StringEOOExprAst =
  EOOBaseAst
  type StringEOOExprRecord =
    {v: {i: Info, v: String}, info: Info}
  syn EOOExpr =
  | StringEOOExpr StringEOOExprRecord
  sem get_EOOExpr_info =
  | StringEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | StringEOOExpr target ->
    StringEOOExpr
      { target
        with
        info =
          val }
end
lang TrueEOOExprAst =
  EOOBaseAst
  type TrueEOOExprRecord =
    {info: Info}
  syn EOOExpr =
  | TrueEOOExpr TrueEOOExprRecord
  sem get_EOOExpr_info =
  | TrueEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | TrueEOOExpr target ->
    TrueEOOExpr
      { target
        with
        info =
          val }
end
lang FalseEOOExprAst =
  EOOBaseAst
  type FalseEOOExprRecord =
    {info: Info}
  syn EOOExpr =
  | FalseEOOExpr FalseEOOExprRecord
  sem get_EOOExpr_info =
  | FalseEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | FalseEOOExpr target ->
    FalseEOOExpr
      { target
        with
        info =
          val }
end
lang UnitEOOExprAst =
  EOOBaseAst
  type UnitEOOExprRecord =
    {info: Info}
  syn EOOExpr =
  | UnitEOOExpr UnitEOOExprRecord
  sem get_EOOExpr_info =
  | UnitEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | UnitEOOExpr target ->
    UnitEOOExpr
      { target
        with
        info =
          val }
end
lang TupEOOExprAst =
  EOOBaseAst
  type TupEOOExprRecord =
    {info: Info, left: EOOExpr, right: EOOExpr}
  syn EOOExpr =
  | TupEOOExpr TupEOOExprRecord
  sem smapAccumL_EOOExpr_EOOExpr f acc =
  | TupEOOExpr x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      in
      match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        in
        (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
    with
      (acc, x)
    in
    (acc, TupEOOExpr
        x)
  sem get_EOOExpr_info =
  | TupEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | TupEOOExpr target ->
    TupEOOExpr
      { target
        with
        info =
          val }
end
lang ConcatEOOExprAst =
  EOOBaseAst
  type ConcatEOOExprRecord =
    {op: Info, info: Info, left: EOOExpr, right: EOOExpr}
  syn EOOExpr =
  | ConcatEOOExpr ConcatEOOExprRecord
  sem smapAccumL_EOOExpr_EOOExpr f acc =
  | ConcatEOOExpr x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      in
      match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        in
        (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
    with
      (acc, x)
    in
    (acc, ConcatEOOExpr
        x)
  sem get_EOOExpr_info =
  | ConcatEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | ConcatEOOExpr target ->
    ConcatEOOExpr
      { target
        with
        info =
          val }
end
lang ConsEOOExprAst =
  EOOBaseAst
  type ConsEOOExprRecord =
    {op: Info, info: Info, left: EOOExpr, right: EOOExpr}
  syn EOOExpr =
  | ConsEOOExpr ConsEOOExprRecord
  sem smapAccumL_EOOExpr_EOOExpr f acc =
  | ConsEOOExpr x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      in
      match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        in
        (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
    with
      (acc, x)
    in
    (acc, ConsEOOExpr
        x)
  sem get_EOOExpr_info =
  | ConsEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | ConsEOOExpr target ->
    ConsEOOExpr
      { target
        with
        info =
          val }
end
lang SeqEOOExprAst =
  EOOBaseAst
  type SeqEOOExprRecord =
    {info: Info, unbrokenElems: Option EOOExpr}
  syn EOOExpr =
  | SeqEOOExpr SeqEOOExprRecord
  sem smapAccumL_EOOExpr_EOOExpr f acc =
  | SeqEOOExpr x ->
    match
      match
        let unbrokenElems =
          x.unbrokenElems
        in
        optionMapAccum
          (lam acc1.
             lam x1.
               f
                 acc1
                 x1)
          acc
          unbrokenElems
      with
        (acc, unbrokenElems)
      in
      (acc, { x
          with
          unbrokenElems =
            unbrokenElems })
    with
      (acc, x)
    in
    (acc, SeqEOOExpr
        x)
  sem get_EOOExpr_info =
  | SeqEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | SeqEOOExpr target ->
    SeqEOOExpr
      { target
        with
        info =
          val }
end
lang LetEOOExprAst =
  EOOBaseAst
  type LetEOOExprRecord =
    {ty: Option EOOType, pat: EOOPat, rhs: EOOExpr, body: EOOExpr, info: Info}
  syn EOOExpr =
  | LetEOOExpr LetEOOExprRecord
  sem smapAccumL_EOOExpr_EOOExpr f acc =
  | LetEOOExpr x ->
    match
      match
        let rhs =
          x.rhs
        in
        f
          acc
          rhs
      with
        (acc, rhs)
      in
      match
          let body =
            x.body
          in
          f
            acc
            body
        with
          (acc, body)
        in
        (acc, { { x
              with
              rhs =
                rhs }
            with
            body =
              body })
    with
      (acc, x)
    in
    (acc, LetEOOExpr
        x)
  sem smapAccumL_EOOExpr_EOOPat f acc =
  | LetEOOExpr x ->
    match
      match
        let pat =
          x.pat
        in
        f
          acc
          pat
      with
        (acc, pat)
      in
      (acc, { x
          with
          pat =
            pat })
    with
      (acc, x)
    in
    (acc, LetEOOExpr
        x)
  sem smapAccumL_EOOExpr_EOOType f acc =
  | LetEOOExpr x ->
    match
      match
        let ty =
          x.ty
        in
        optionMapAccum
          (lam acc1.
             lam x1.
               f
                 acc1
                 x1)
          acc
          ty
      with
        (acc, ty)
      in
      (acc, { x
          with
          ty =
            ty })
    with
      (acc, x)
    in
    (acc, LetEOOExpr
        x)
  sem get_EOOExpr_info =
  | LetEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | LetEOOExpr target ->
    LetEOOExpr
      { target
        with
        info =
          val }
end
lang DefEOOExprAst =
  EOOBaseAst
  type DefEOOExprRecord =
    {body: EOOExpr, info: Info, binding: EOODefBinding}
  syn EOOExpr =
  | DefEOOExpr DefEOOExprRecord
  sem smapAccumL_EOOExpr_EOODefBinding f acc =
  | DefEOOExpr x ->
    match
      match
        let binding =
          x.binding
        in
        f
          acc
          binding
      with
        (acc, binding)
      in
      (acc, { x
          with
          binding =
            binding })
    with
      (acc, x)
    in
    (acc, DefEOOExpr
        x)
  sem smapAccumL_EOOExpr_EOOExpr f acc =
  | DefEOOExpr x ->
    match
      match
        let body =
          x.body
        in
        f
          acc
          body
      with
        (acc, body)
      in
      (acc, { x
          with
          body =
            body })
    with
      (acc, x)
    in
    (acc, DefEOOExpr
        x)
  sem get_EOOExpr_info =
  | DefEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | DefEOOExpr target ->
    DefEOOExpr
      { target
        with
        info =
          val }
end
lang IfEOOExprAst =
  EOOBaseAst
  type IfEOOExprRecord =
    {c: EOOExpr, e: EOOExpr, t: EOOExpr, info: Info}
  syn EOOExpr =
  | IfEOOExpr IfEOOExprRecord
  sem smapAccumL_EOOExpr_EOOExpr f acc =
  | IfEOOExpr x ->
    match
      match
        let c =
          x.c
        in
        f
          acc
          c
      with
        (acc, c)
      in
      match
          let e =
            x.e
          in
          f
            acc
            e
        with
          (acc, e)
        in
        match
            let t =
              x.t
            in
            f
              acc
              t
          with
            (acc, t)
          in
          (acc, { { { x
                  with
                  c =
                    c }
                with
                e =
                  e }
              with
              t =
                t })
    with
      (acc, x)
    in
    (acc, IfEOOExpr
        x)
  sem get_EOOExpr_info =
  | IfEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | IfEOOExpr target ->
    IfEOOExpr
      { target
        with
        info =
          val }
end
lang MatchingEOOExprAst =
  EOOBaseAst
  type MatchingEOOExprRecord =
    {arms: [{pat: EOOPat, body: EOOExpr}], info: Info, scrut: EOOExpr}
  syn EOOExpr =
  | MatchingEOOExpr MatchingEOOExprRecord
  sem smapAccumL_EOOExpr_EOOExpr f acc =
  | MatchingEOOExpr x ->
    match
      match
        let arms =
          x.arms
        in
        mapAccumL
          (lam acc1.
             lam x1: {pat: EOOPat, body: EOOExpr}.
               match
                 let body =
                   x1.body
                 in
                 f
                   acc1
                   body
               with
                 (acc1, body)
               in
               (acc1, { x1
                   with
                   body =
                     body }))
          acc
          arms
      with
        (acc, arms)
      in
      match
          let scrut =
            x.scrut
          in
          f
            acc
            scrut
        with
          (acc, scrut)
        in
        (acc, { { x
              with
              arms =
                arms }
            with
            scrut =
              scrut })
    with
      (acc, x)
    in
    (acc, MatchingEOOExpr
        x)
  sem smapAccumL_EOOExpr_EOOPat f acc =
  | MatchingEOOExpr x ->
    match
      match
        let arms =
          x.arms
        in
        mapAccumL
          (lam acc1.
             lam x1: {pat: EOOPat, body: EOOExpr}.
               match
                 let pat =
                   x1.pat
                 in
                 f
                   acc1
                   pat
               with
                 (acc1, pat)
               in
               (acc1, { x1
                   with
                   pat =
                     pat }))
          acc
          arms
      with
        (acc, arms)
      in
      (acc, { x
          with
          arms =
            arms })
    with
      (acc, x)
    in
    (acc, MatchingEOOExpr
        x)
  sem get_EOOExpr_info =
  | MatchingEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | MatchingEOOExpr target ->
    MatchingEOOExpr
      { target
        with
        info =
          val }
end
lang AddfEOOExprAst =
  EOOBaseAst
  type AddfEOOExprRecord =
    {op: Info, info: Info, left: EOOExpr, right: EOOExpr}
  syn EOOExpr =
  | AddfEOOExpr AddfEOOExprRecord
  sem smapAccumL_EOOExpr_EOOExpr f acc =
  | AddfEOOExpr x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      in
      match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        in
        (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
    with
      (acc, x)
    in
    (acc, AddfEOOExpr
        x)
  sem get_EOOExpr_info =
  | AddfEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | AddfEOOExpr target ->
    AddfEOOExpr
      { target
        with
        info =
          val }
end
lang SubfEOOExprAst =
  EOOBaseAst
  type SubfEOOExprRecord =
    {op: Info, info: Info, left: EOOExpr, right: EOOExpr}
  syn EOOExpr =
  | SubfEOOExpr SubfEOOExprRecord
  sem smapAccumL_EOOExpr_EOOExpr f acc =
  | SubfEOOExpr x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      in
      match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        in
        (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
    with
      (acc, x)
    in
    (acc, SubfEOOExpr
        x)
  sem get_EOOExpr_info =
  | SubfEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | SubfEOOExpr target ->
    SubfEOOExpr
      { target
        with
        info =
          val }
end
lang MulfEOOExprAst =
  EOOBaseAst
  type MulfEOOExprRecord =
    {op: Info, info: Info, left: EOOExpr, right: EOOExpr}
  syn EOOExpr =
  | MulfEOOExpr MulfEOOExprRecord
  sem smapAccumL_EOOExpr_EOOExpr f acc =
  | MulfEOOExpr x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      in
      match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        in
        (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
    with
      (acc, x)
    in
    (acc, MulfEOOExpr
        x)
  sem get_EOOExpr_info =
  | MulfEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | MulfEOOExpr target ->
    MulfEOOExpr
      { target
        with
        info =
          val }
end
lang DivfEOOExprAst =
  EOOBaseAst
  type DivfEOOExprRecord =
    {op: Info, info: Info, left: EOOExpr, right: EOOExpr}
  syn EOOExpr =
  | DivfEOOExpr DivfEOOExprRecord
  sem smapAccumL_EOOExpr_EOOExpr f acc =
  | DivfEOOExpr x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      in
      match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        in
        (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
    with
      (acc, x)
    in
    (acc, DivfEOOExpr
        x)
  sem get_EOOExpr_info =
  | DivfEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | DivfEOOExpr target ->
    DivfEOOExpr
      { target
        with
        info =
          val }
end
lang PrimEOOExprAst =
  EOOBaseAst
  type PrimEOOExprRecord =
    {op: Info, info: Info, left: EOOExpr}
  syn EOOExpr =
  | PrimEOOExpr PrimEOOExprRecord
  sem smapAccumL_EOOExpr_EOOExpr f acc =
  | PrimEOOExpr x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      in
      (acc, { x
          with
          left =
            left })
    with
      (acc, x)
    in
    (acc, PrimEOOExpr
        x)
  sem get_EOOExpr_info =
  | PrimEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | PrimEOOExpr target ->
    PrimEOOExpr
      { target
        with
        info =
          val }
end
lang EqnEOOExprAst =
  EOOBaseAst
  type EqnEOOExprRecord =
    {op: Info, info: Info, left: EOOExpr, right: EOOExpr}
  syn EOOExpr =
  | EqnEOOExpr EqnEOOExprRecord
  sem smapAccumL_EOOExpr_EOOExpr f acc =
  | EqnEOOExpr x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      in
      match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        in
        (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
    with
      (acc, x)
    in
    (acc, EqnEOOExpr
        x)
  sem get_EOOExpr_info =
  | EqnEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | EqnEOOExpr target ->
    EqnEOOExpr
      { target
        with
        info =
          val }
end
lang InitEOOExprAst =
  EOOBaseAst
  type InitEOOExprRecord =
    {info: Info, right: EOOExpr}
  syn EOOExpr =
  | InitEOOExpr InitEOOExprRecord
  sem smapAccumL_EOOExpr_EOOExpr f acc =
  | InitEOOExpr x ->
    match
      match
        let right =
          x.right
        in
        f
          acc
          right
      with
        (acc, right)
      in
      (acc, { x
          with
          right =
            right })
    with
      (acc, x)
    in
    (acc, InitEOOExpr
        x)
  sem get_EOOExpr_info =
  | InitEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | InitEOOExpr target ->
    InitEOOExpr
      { target
        with
        info =
          val }
end
lang ConnectEOOExprAst =
  EOOBaseAst
  type ConnectEOOExprRecord =
    {to: EOOExpr, dom: EOOExpr, from: EOOExpr, info: Info, across: EOOExpr, through: EOOExpr}
  syn EOOExpr =
  | ConnectEOOExpr ConnectEOOExprRecord
  sem smapAccumL_EOOExpr_EOOExpr f acc =
  | ConnectEOOExpr x ->
    match
      match
        let to =
          x.to
        in
        f
          acc
          to
      with
        (acc, to)
      in
      match
          let dom =
            x.dom
          in
          f
            acc
            dom
        with
          (acc, dom)
        in
        match
            let from =
              x.from
            in
            f
              acc
              from
          with
            (acc, from)
          in
          match
              let across =
                x.across
              in
              f
                acc
                across
            with
              (acc, across)
            in
            match
                let through =
                  x.through
                in
                f
                  acc
                  through
              with
                (acc, through)
              in
              (acc, { { { { { x
                          with
                          to =
                            to }
                        with
                        dom =
                          dom }
                      with
                      from =
                        from }
                    with
                    across =
                      across }
                  with
                  through =
                    through })
    with
      (acc, x)
    in
    (acc, ConnectEOOExpr
        x)
  sem get_EOOExpr_info =
  | ConnectEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | ConnectEOOExpr target ->
    ConnectEOOExpr
      { target
        with
        info =
          val }
end
lang ModelEOOExprAst =
  EOOBaseAst
  type ModelEOOExprRecord =
    {eqn: EOOExpr, info: Info, eqnkw: Info, heads: [EOOModelHead]}
  syn EOOExpr =
  | ModelEOOExpr ModelEOOExprRecord
  sem smapAccumL_EOOExpr_EOOExpr f acc =
  | ModelEOOExpr x ->
    match
      match
        let eqn =
          x.eqn
        in
        f
          acc
          eqn
      with
        (acc, eqn)
      in
      (acc, { x
          with
          eqn =
            eqn })
    with
      (acc, x)
    in
    (acc, ModelEOOExpr
        x)
  sem smapAccumL_EOOExpr_EOOModelHead f acc =
  | ModelEOOExpr x ->
    match
      match
        let heads =
          x.heads
        in
        mapAccumL
          (lam acc1.
             lam x1: EOOModelHead.
               f
                 acc1
                 x1)
          acc
          heads
      with
        (acc, heads)
      in
      (acc, { x
          with
          heads =
            heads })
    with
      (acc, x)
    in
    (acc, ModelEOOExpr
        x)
  sem get_EOOExpr_info =
  | ModelEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | ModelEOOExpr target ->
    ModelEOOExpr
      { target
        with
        info =
          val }
end
lang VarEOOModelHeadAst =
  EOOBaseAst
  type VarEOOModelHeadRecord =
    {ns: [{i: Info, v: Name}], ty: EOOType, info: Info}
  syn EOOModelHead =
  | VarEOOModelHead VarEOOModelHeadRecord
  sem smapAccumL_EOOModelHead_EOOType f acc =
  | VarEOOModelHead x ->
    match
      match
        let ty =
          x.ty
        in
        f
          acc
          ty
      with
        (acc, ty)
      in
      (acc, { x
          with
          ty =
            ty })
    with
      (acc, x)
    in
    (acc, VarEOOModelHead
        x)
  sem get_EOOModelHead_info =
  | VarEOOModelHead target ->
    target.info
  sem set_EOOModelHead_info val =
  | VarEOOModelHead target ->
    VarEOOModelHead
      { target
        with
        info =
          val }
end
lang NodeEOOModelHeadAst =
  EOOBaseAst
  type NodeEOOModelHeadRecord =
    {ns: [{i: Info, v: Name}], info: Info}
  syn EOOModelHead =
  | NodeEOOModelHead NodeEOOModelHeadRecord
  sem get_EOOModelHead_info =
  | NodeEOOModelHead target ->
    target.info
  sem set_EOOModelHead_info val =
  | NodeEOOModelHead target ->
    NodeEOOModelHead
      { target
        with
        info =
          val }
end
lang DefEOOModelHeadAst =
  EOOBaseAst
  type DefEOOModelHeadRecord =
    {info: Info, binding: EOODefBinding}
  syn EOOModelHead =
  | DefEOOModelHead DefEOOModelHeadRecord
  sem smapAccumL_EOOModelHead_EOODefBinding f acc =
  | DefEOOModelHead x ->
    match
      match
        let binding =
          x.binding
        in
        f
          acc
          binding
      with
        (acc, binding)
      in
      (acc, { x
          with
          binding =
            binding })
    with
      (acc, x)
    in
    (acc, DefEOOModelHead
        x)
  sem get_EOOModelHead_info =
  | DefEOOModelHead target ->
    target.info
  sem set_EOOModelHead_info val =
  | DefEOOModelHead target ->
    DefEOOModelHead
      { target
        with
        info =
          val }
end
lang ModelEOOModelHeadAst =
  EOOBaseAst
  type ModelEOOModelHeadRecord =
    {info: Info, binding: EOOModelBinding}
  syn EOOModelHead =
  | ModelEOOModelHead ModelEOOModelHeadRecord
  sem smapAccumL_EOOModelHead_EOOModelBinding f acc =
  | ModelEOOModelHead x ->
    match
      match
        let binding =
          x.binding
        in
        f
          acc
          binding
      with
        (acc, binding)
      in
      (acc, { x
          with
          binding =
            binding })
    with
      (acc, x)
    in
    (acc, ModelEOOModelHead
        x)
  sem get_EOOModelHead_info =
  | ModelEOOModelHead target ->
    target.info
  sem set_EOOModelHead_info val =
  | ModelEOOModelHead target ->
    ModelEOOModelHead
      { target
        with
        info =
          val }
end
lang WildEOOPatAst =
  EOOBaseAst
  type WildEOOPatRecord =
    {info: Info}
  syn EOOPat =
  | WildEOOPat WildEOOPatRecord
  sem get_EOOPat_info =
  | WildEOOPat target ->
    target.info
  sem set_EOOPat_info val =
  | WildEOOPat target ->
    WildEOOPat
      { target
        with
        info =
          val }
end
lang BindEOOPatAst =
  EOOBaseAst
  type BindEOOPatRecord =
    {n: {i: Info, v: Name}, info: Info}
  syn EOOPat =
  | BindEOOPat BindEOOPatRecord
  sem get_EOOPat_info =
  | BindEOOPat target ->
    target.info
  sem set_EOOPat_info val =
  | BindEOOPat target ->
    BindEOOPat
      { target
        with
        info =
          val }
end
lang UnitEOOPatAst =
  EOOBaseAst
  type UnitEOOPatRecord =
    {info: Info}
  syn EOOPat =
  | UnitEOOPat UnitEOOPatRecord
  sem get_EOOPat_info =
  | UnitEOOPat target ->
    target.info
  sem set_EOOPat_info val =
  | UnitEOOPat target ->
    UnitEOOPat
      { target
        with
        info =
          val }
end
lang TupEOOPatAst =
  EOOBaseAst
  type TupEOOPatRecord =
    {info: Info, left: EOOPat, right: EOOPat}
  syn EOOPat =
  | TupEOOPat TupEOOPatRecord
  sem smapAccumL_EOOPat_EOOPat f acc =
  | TupEOOPat x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      in
      match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        in
        (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
    with
      (acc, x)
    in
    (acc, TupEOOPat
        x)
  sem get_EOOPat_info =
  | TupEOOPat target ->
    target.info
  sem set_EOOPat_info val =
  | TupEOOPat target ->
    TupEOOPat
      { target
        with
        info =
          val }
end
lang ConsEOOPatAst =
  EOOBaseAst
  type ConsEOOPatRecord =
    {info: Info, left: EOOPat, right: EOOPat}
  syn EOOPat =
  | ConsEOOPat ConsEOOPatRecord
  sem smapAccumL_EOOPat_EOOPat f acc =
  | ConsEOOPat x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      in
      match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        in
        (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
    with
      (acc, x)
    in
    (acc, ConsEOOPat
        x)
  sem get_EOOPat_info =
  | ConsEOOPat target ->
    target.info
  sem set_EOOPat_info val =
  | ConsEOOPat target ->
    ConsEOOPat
      { target
        with
        info =
          val }
end
lang SeqEOOPatAst =
  EOOBaseAst
  type SeqEOOPatRecord =
    {info: Info, elems: EOOPat}
  syn EOOPat =
  | SeqEOOPat SeqEOOPatRecord
  sem smapAccumL_EOOPat_EOOPat f acc =
  | SeqEOOPat x ->
    match
      match
        let elems =
          x.elems
        in
        f
          acc
          elems
      with
        (acc, elems)
      in
      (acc, { x
          with
          elems =
            elems })
    with
      (acc, x)
    in
    (acc, SeqEOOPat
        x)
  sem get_EOOPat_info =
  | SeqEOOPat target ->
    target.info
  sem set_EOOPat_info val =
  | SeqEOOPat target ->
    SeqEOOPat
      { target
        with
        info =
          val }
end
lang VarEOOTypeAst =
  EOOBaseAst
  type VarEOOTypeRecord =
    {n: {i: Info, v: Name}, info: Info}
  syn EOOType =
  | VarEOOType VarEOOTypeRecord
  sem get_EOOType_info =
  | VarEOOType target ->
    target.info
  sem set_EOOType_info val =
  | VarEOOType target ->
    VarEOOType
      { target
        with
        info =
          val }
end
lang ConEOOTypeAst =
  EOOBaseAst
  type ConEOOTypeRecord =
    {n: {i: Info, v: Name}, info: Info}
  syn EOOType =
  | ConEOOType ConEOOTypeRecord
  sem get_EOOType_info =
  | ConEOOType target ->
    target.info
  sem set_EOOType_info val =
  | ConEOOType target ->
    ConEOOType
      { target
        with
        info =
          val }
end
lang AppEOOTypeAst =
  EOOBaseAst
  type AppEOOTypeRecord =
    {info: Info, left: EOOType, right: EOOType}
  syn EOOType =
  | AppEOOType AppEOOTypeRecord
  sem smapAccumL_EOOType_EOOType f acc =
  | AppEOOType x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      in
      match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        in
        (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
    with
      (acc, x)
    in
    (acc, AppEOOType
        x)
  sem get_EOOType_info =
  | AppEOOType target ->
    target.info
  sem set_EOOType_info val =
  | AppEOOType target ->
    AppEOOType
      { target
        with
        info =
          val }
end
lang TupEOOTypeAst =
  EOOBaseAst
  type TupEOOTypeRecord =
    {info: Info, left: EOOType, right: EOOType}
  syn EOOType =
  | TupEOOType TupEOOTypeRecord
  sem smapAccumL_EOOType_EOOType f acc =
  | TupEOOType x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      in
      match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        in
        (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
    with
      (acc, x)
    in
    (acc, TupEOOType
        x)
  sem get_EOOType_info =
  | TupEOOType target ->
    target.info
  sem set_EOOType_info val =
  | TupEOOType target ->
    TupEOOType
      { target
        with
        info =
          val }
end
lang SeqEOOTypeAst =
  EOOBaseAst
  type SeqEOOTypeRecord =
    {ty: EOOType, info: Info}
  syn EOOType =
  | SeqEOOType SeqEOOTypeRecord
  sem smapAccumL_EOOType_EOOType f acc =
  | SeqEOOType x ->
    match
      match
        let ty =
          x.ty
        in
        f
          acc
          ty
      with
        (acc, ty)
      in
      (acc, { x
          with
          ty =
            ty })
    with
      (acc, x)
    in
    (acc, SeqEOOType
        x)
  sem get_EOOType_info =
  | SeqEOOType target ->
    target.info
  sem set_EOOType_info val =
  | SeqEOOType target ->
    SeqEOOType
      { target
        with
        info =
          val }
end
lang ArrowEOOTypeAst =
  EOOBaseAst
  type ArrowEOOTypeRecord =
    {info: Info, left: EOOType, right: EOOType}
  syn EOOType =
  | ArrowEOOType ArrowEOOTypeRecord
  sem smapAccumL_EOOType_EOOType f acc =
  | ArrowEOOType x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      in
      match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        in
        (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
    with
      (acc, x)
    in
    (acc, ArrowEOOType
        x)
  sem get_EOOType_info =
  | ArrowEOOType target ->
    target.info
  sem set_EOOType_info val =
  | ArrowEOOType target ->
    ArrowEOOType
      { target
        with
        info =
          val }
end
lang ForallEOOTypeAst =
  EOOBaseAst
  type ForallEOOTypeRecord =
    {ns: [{i: Info, v: Name}], info: Info, right: EOOType}
  syn EOOType =
  | ForallEOOType ForallEOOTypeRecord
  sem smapAccumL_EOOType_EOOType f acc =
  | ForallEOOType x ->
    match
      match
        let right =
          x.right
        in
        f
          acc
          right
      with
        (acc, right)
      in
      (acc, { x
          with
          right =
            right })
    with
      (acc, x)
    in
    (acc, ForallEOOType
        x)
  sem get_EOOType_info =
  | ForallEOOType target ->
    target.info
  sem set_EOOType_info val =
  | ForallEOOType target ->
    ForallEOOType
      { target
        with
        info =
          val }
end
lang BadEOOProgAst =
  EOOBaseAst
  type BadEOOProgRecord =
    {info: Info}
  syn EOOProg =
  | BadEOOProg BadEOOProgRecord
  sem get_EOOProg_info =
  | BadEOOProg target ->
    target.info
  sem set_EOOProg_info val =
  | BadEOOProg target ->
    BadEOOProg
      { target
        with
        info =
          val }
end
lang BadEOOTopAst =
  EOOBaseAst
  type BadEOOTopRecord =
    {info: Info}
  syn EOOTop =
  | BadEOOTop BadEOOTopRecord
  sem get_EOOTop_info =
  | BadEOOTop target ->
    target.info
  sem set_EOOTop_info val =
  | BadEOOTop target ->
    BadEOOTop
      { target
        with
        info =
          val }
end
lang BadEOOParamAst =
  EOOBaseAst
  type BadEOOParamRecord =
    {info: Info}
  syn EOOParam =
  | BadEOOParam BadEOOParamRecord
  sem get_EOOParam_info =
  | BadEOOParam target ->
    target.info
  sem set_EOOParam_info val =
  | BadEOOParam target ->
    BadEOOParam
      { target
        with
        info =
          val }
end
lang BadEOODefBindingAst =
  EOOBaseAst
  type BadEOODefBindingRecord =
    {info: Info}
  syn EOODefBinding =
  | BadEOODefBinding BadEOODefBindingRecord
  sem get_EOODefBinding_info =
  | BadEOODefBinding target ->
    target.info
  sem set_EOODefBinding_info val =
  | BadEOODefBinding target ->
    BadEOODefBinding
      { target
        with
        info =
          val }
end
lang BadEOOModelBindingAst =
  EOOBaseAst
  type BadEOOModelBindingRecord =
    {info: Info}
  syn EOOModelBinding =
  | BadEOOModelBinding BadEOOModelBindingRecord
  sem get_EOOModelBinding_info =
  | BadEOOModelBinding target ->
    target.info
  sem set_EOOModelBinding_info val =
  | BadEOOModelBinding target ->
    BadEOOModelBinding
      { target
        with
        info =
          val }
end
lang BadEOOExprAst =
  EOOBaseAst
  type BadEOOExprRecord =
    {info: Info}
  syn EOOExpr =
  | BadEOOExpr BadEOOExprRecord
  sem get_EOOExpr_info =
  | BadEOOExpr target ->
    target.info
  sem set_EOOExpr_info val =
  | BadEOOExpr target ->
    BadEOOExpr
      { target
        with
        info =
          val }
end
lang BadEOOPatAst =
  EOOBaseAst
  type BadEOOPatRecord =
    {info: Info}
  syn EOOPat =
  | BadEOOPat BadEOOPatRecord
  sem get_EOOPat_info =
  | BadEOOPat target ->
    target.info
  sem set_EOOPat_info val =
  | BadEOOPat target ->
    BadEOOPat
      { target
        with
        info =
          val }
end
lang BadEOOTypeAst =
  EOOBaseAst
  type BadEOOTypeRecord =
    {info: Info}
  syn EOOType =
  | BadEOOType BadEOOTypeRecord
  sem get_EOOType_info =
  | BadEOOType target ->
    target.info
  sem set_EOOType_info val =
  | BadEOOType target ->
    BadEOOType
      { target
        with
        info =
          val }
end
lang BadEOOModelHeadAst =
  EOOBaseAst
  type BadEOOModelHeadRecord =
    {info: Info}
  syn EOOModelHead =
  | BadEOOModelHead BadEOOModelHeadRecord
  sem get_EOOModelHead_info =
  | BadEOOModelHead target ->
    target.info
  sem set_EOOModelHead_info val =
  | BadEOOModelHead target ->
    BadEOOModelHead
      { target
        with
        info =
          val }
end
lang EOOAst =
  ProgEOOProgAst
  + DefEOOTopAst
  + ModelEOOTopAst
  + TypeEOOTopAst
  + PatEOOParamAst
  + NameEOOParamAst
  + IgnoreEOOParamAst
  + SimpleEOODefBindingAst
  + SimpleEOOModelBindingAst
  + VarEOOExprAst
  + AbsEOOExprAst
  + AppEOOExprAst
  + NumEOOExprAst
  + StringEOOExprAst
  + TrueEOOExprAst
  + FalseEOOExprAst
  + UnitEOOExprAst
  + TupEOOExprAst
  + ConcatEOOExprAst
  + ConsEOOExprAst
  + SeqEOOExprAst
  + LetEOOExprAst
  + DefEOOExprAst
  + IfEOOExprAst
  + MatchingEOOExprAst
  + AddfEOOExprAst
  + SubfEOOExprAst
  + MulfEOOExprAst
  + DivfEOOExprAst
  + PrimEOOExprAst
  + EqnEOOExprAst
  + InitEOOExprAst
  + ConnectEOOExprAst
  + ModelEOOExprAst
  + VarEOOModelHeadAst
  + NodeEOOModelHeadAst
  + DefEOOModelHeadAst
  + ModelEOOModelHeadAst
  + WildEOOPatAst
  + BindEOOPatAst
  + UnitEOOPatAst
  + TupEOOPatAst
  + ConsEOOPatAst
  + SeqEOOPatAst
  + VarEOOTypeAst
  + ConEOOTypeAst
  + AppEOOTypeAst
  + TupEOOTypeAst
  + SeqEOOTypeAst
  + ArrowEOOTypeAst
  + ForallEOOTypeAst
  + BadEOOProgAst
  + BadEOOTopAst
  + BadEOOParamAst
  + BadEOODefBindingAst
  + BadEOOModelBindingAst
  + BadEOOExprAst
  + BadEOOPatAst
  + BadEOOTypeAst
  + BadEOOModelHeadAst
end
lang EOOProgOpBase =
  EOOAst
  syn EOOProgOp lstyle rstyle =
  sem topAllowed_EOOProgOp : all lstyle. all rstyle. EOOProgOp lstyle rstyle -> Bool
sem topAllowed_EOOProgOp =
  | _ ->
    true
  sem leftAllowed_EOOProgOp : all lstyle. all style. all rstyle. {child: EOOProgOp lstyle rstyle, parent: EOOProgOp LOpen style} -> Bool
sem leftAllowed_EOOProgOp =
  | _ ->
    true
  sem rightAllowed_EOOProgOp : all style. all lstyle. all rstyle. {child: EOOProgOp lstyle rstyle, parent: EOOProgOp style ROpen} -> Bool
sem rightAllowed_EOOProgOp =
  | _ ->
    true
  sem groupingsAllowed_EOOProgOp : all lstyle. all rstyle. (EOOProgOp lstyle ROpen, EOOProgOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_EOOProgOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_EOOProgOp : all lstyle. all rstyle. EOOProgOp lstyle rstyle -> AllowedDirection
sem parenAllowed_EOOProgOp =
  | _ ->
    GEither
      {}
  sem getInfo_EOOProgOp : all lstyle. all rstyle. EOOProgOp lstyle rstyle -> Info
  sem getTerms_EOOProgOp : all lstyle. all rstyle. EOOProgOp lstyle rstyle -> [Info]
  sem unsplit_EOOProgOp : PermanentNode EOOProgOp -> (Info, EOOProg)
end
lang EOOTopOpBase =
  EOOAst
  syn EOOTopOp lstyle rstyle =
  sem topAllowed_EOOTopOp : all lstyle. all rstyle. EOOTopOp lstyle rstyle -> Bool
sem topAllowed_EOOTopOp =
  | _ ->
    true
  sem leftAllowed_EOOTopOp : all lstyle. all style. all rstyle. {child: EOOTopOp lstyle rstyle, parent: EOOTopOp LOpen style} -> Bool
sem leftAllowed_EOOTopOp =
  | _ ->
    true
  sem rightAllowed_EOOTopOp : all style. all lstyle. all rstyle. {child: EOOTopOp lstyle rstyle, parent: EOOTopOp style ROpen} -> Bool
sem rightAllowed_EOOTopOp =
  | _ ->
    true
  sem groupingsAllowed_EOOTopOp : all lstyle. all rstyle. (EOOTopOp lstyle ROpen, EOOTopOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_EOOTopOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_EOOTopOp : all lstyle. all rstyle. EOOTopOp lstyle rstyle -> AllowedDirection
sem parenAllowed_EOOTopOp =
  | _ ->
    GEither
      {}
  sem getInfo_EOOTopOp : all lstyle. all rstyle. EOOTopOp lstyle rstyle -> Info
  sem getTerms_EOOTopOp : all lstyle. all rstyle. EOOTopOp lstyle rstyle -> [Info]
  sem unsplit_EOOTopOp : PermanentNode EOOTopOp -> (Info, EOOTop)
end
lang EOOParamOpBase =
  EOOAst
  syn EOOParamOp lstyle rstyle =
  sem topAllowed_EOOParamOp : all lstyle. all rstyle. EOOParamOp lstyle rstyle -> Bool
sem topAllowed_EOOParamOp =
  | _ ->
    true
  sem leftAllowed_EOOParamOp : all lstyle. all style. all rstyle. {child: EOOParamOp lstyle rstyle, parent: EOOParamOp LOpen style} -> Bool
sem leftAllowed_EOOParamOp =
  | _ ->
    true
  sem rightAllowed_EOOParamOp : all style. all lstyle. all rstyle. {child: EOOParamOp lstyle rstyle, parent: EOOParamOp style ROpen} -> Bool
sem rightAllowed_EOOParamOp =
  | _ ->
    true
  sem groupingsAllowed_EOOParamOp : all lstyle. all rstyle. (EOOParamOp lstyle ROpen, EOOParamOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_EOOParamOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_EOOParamOp : all lstyle. all rstyle. EOOParamOp lstyle rstyle -> AllowedDirection
sem parenAllowed_EOOParamOp =
  | _ ->
    GEither
      {}
  sem getInfo_EOOParamOp : all lstyle. all rstyle. EOOParamOp lstyle rstyle -> Info
  sem getTerms_EOOParamOp : all lstyle. all rstyle. EOOParamOp lstyle rstyle -> [Info]
  sem unsplit_EOOParamOp : PermanentNode EOOParamOp -> (Info, EOOParam)
end
lang EOODefBindingOpBase =
  EOOAst
  syn EOODefBindingOp lstyle rstyle =
  sem topAllowed_EOODefBindingOp : all lstyle. all rstyle. EOODefBindingOp lstyle rstyle -> Bool
sem topAllowed_EOODefBindingOp =
  | _ ->
    true
  sem leftAllowed_EOODefBindingOp : all lstyle. all style. all rstyle. {child: EOODefBindingOp lstyle rstyle, parent: EOODefBindingOp LOpen style} -> Bool
sem leftAllowed_EOODefBindingOp =
  | _ ->
    true
  sem rightAllowed_EOODefBindingOp : all style. all lstyle. all rstyle. {child: EOODefBindingOp lstyle rstyle, parent: EOODefBindingOp style ROpen} -> Bool
sem rightAllowed_EOODefBindingOp =
  | _ ->
    true
  sem groupingsAllowed_EOODefBindingOp : all lstyle. all rstyle. (EOODefBindingOp lstyle ROpen, EOODefBindingOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_EOODefBindingOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_EOODefBindingOp : all lstyle. all rstyle. EOODefBindingOp lstyle rstyle -> AllowedDirection
sem parenAllowed_EOODefBindingOp =
  | _ ->
    GEither
      {}
  sem getInfo_EOODefBindingOp : all lstyle. all rstyle. EOODefBindingOp lstyle rstyle -> Info
  sem getTerms_EOODefBindingOp : all lstyle. all rstyle. EOODefBindingOp lstyle rstyle -> [Info]
  sem unsplit_EOODefBindingOp : PermanentNode EOODefBindingOp -> (Info, EOODefBinding)
end
lang EOOModelBindingOpBase =
  EOOAst
  syn EOOModelBindingOp lstyle rstyle =
  sem topAllowed_EOOModelBindingOp : all lstyle. all rstyle. EOOModelBindingOp lstyle rstyle -> Bool
sem topAllowed_EOOModelBindingOp =
  | _ ->
    true
  sem leftAllowed_EOOModelBindingOp : all lstyle. all style. all rstyle. {child: EOOModelBindingOp lstyle rstyle, parent: EOOModelBindingOp LOpen style} -> Bool
sem leftAllowed_EOOModelBindingOp =
  | _ ->
    true
  sem rightAllowed_EOOModelBindingOp : all style. all lstyle. all rstyle. {child: EOOModelBindingOp lstyle rstyle, parent: EOOModelBindingOp style ROpen} -> Bool
sem rightAllowed_EOOModelBindingOp =
  | _ ->
    true
  sem groupingsAllowed_EOOModelBindingOp : all lstyle. all rstyle. (EOOModelBindingOp lstyle ROpen, EOOModelBindingOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_EOOModelBindingOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_EOOModelBindingOp : all lstyle. all rstyle. EOOModelBindingOp lstyle rstyle -> AllowedDirection
sem parenAllowed_EOOModelBindingOp =
  | _ ->
    GEither
      {}
  sem getInfo_EOOModelBindingOp : all lstyle. all rstyle. EOOModelBindingOp lstyle rstyle -> Info
  sem getTerms_EOOModelBindingOp : all lstyle. all rstyle. EOOModelBindingOp lstyle rstyle -> [Info]
  sem unsplit_EOOModelBindingOp : PermanentNode EOOModelBindingOp -> (Info, EOOModelBinding)
end
lang EOOExprOpBase =
  EOOAst
  syn EOOExprOp lstyle rstyle =
  sem topAllowed_EOOExprOp : all lstyle. all rstyle. EOOExprOp lstyle rstyle -> Bool
sem topAllowed_EOOExprOp =
  | _ ->
    true
  sem leftAllowed_EOOExprOp : all lstyle. all style. all rstyle. {child: EOOExprOp lstyle rstyle, parent: EOOExprOp LOpen style} -> Bool
sem leftAllowed_EOOExprOp =
  | _ ->
    true
  sem rightAllowed_EOOExprOp : all style. all lstyle. all rstyle. {child: EOOExprOp lstyle rstyle, parent: EOOExprOp style ROpen} -> Bool
sem rightAllowed_EOOExprOp =
  | _ ->
    true
  sem groupingsAllowed_EOOExprOp : all lstyle. all rstyle. (EOOExprOp lstyle ROpen, EOOExprOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_EOOExprOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_EOOExprOp : all lstyle. all rstyle. EOOExprOp lstyle rstyle -> AllowedDirection
sem parenAllowed_EOOExprOp =
  | _ ->
    GEither
      {}
  sem getInfo_EOOExprOp : all lstyle. all rstyle. EOOExprOp lstyle rstyle -> Info
  sem getTerms_EOOExprOp : all lstyle. all rstyle. EOOExprOp lstyle rstyle -> [Info]
  sem unsplit_EOOExprOp : PermanentNode EOOExprOp -> (Info, EOOExpr)
end
lang EOOPatOpBase =
  EOOAst
  syn EOOPatOp lstyle rstyle =
  sem topAllowed_EOOPatOp : all lstyle. all rstyle. EOOPatOp lstyle rstyle -> Bool
sem topAllowed_EOOPatOp =
  | _ ->
    true
  sem leftAllowed_EOOPatOp : all lstyle. all style. all rstyle. {child: EOOPatOp lstyle rstyle, parent: EOOPatOp LOpen style} -> Bool
sem leftAllowed_EOOPatOp =
  | _ ->
    true
  sem rightAllowed_EOOPatOp : all style. all lstyle. all rstyle. {child: EOOPatOp lstyle rstyle, parent: EOOPatOp style ROpen} -> Bool
sem rightAllowed_EOOPatOp =
  | _ ->
    true
  sem groupingsAllowed_EOOPatOp : all lstyle. all rstyle. (EOOPatOp lstyle ROpen, EOOPatOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_EOOPatOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_EOOPatOp : all lstyle. all rstyle. EOOPatOp lstyle rstyle -> AllowedDirection
sem parenAllowed_EOOPatOp =
  | _ ->
    GEither
      {}
  sem getInfo_EOOPatOp : all lstyle. all rstyle. EOOPatOp lstyle rstyle -> Info
  sem getTerms_EOOPatOp : all lstyle. all rstyle. EOOPatOp lstyle rstyle -> [Info]
  sem unsplit_EOOPatOp : PermanentNode EOOPatOp -> (Info, EOOPat)
end
lang EOOTypeOpBase =
  EOOAst
  syn EOOTypeOp lstyle rstyle =
  sem topAllowed_EOOTypeOp : all lstyle. all rstyle. EOOTypeOp lstyle rstyle -> Bool
sem topAllowed_EOOTypeOp =
  | _ ->
    true
  sem leftAllowed_EOOTypeOp : all lstyle. all style. all rstyle. {child: EOOTypeOp lstyle rstyle, parent: EOOTypeOp LOpen style} -> Bool
sem leftAllowed_EOOTypeOp =
  | _ ->
    true
  sem rightAllowed_EOOTypeOp : all style. all lstyle. all rstyle. {child: EOOTypeOp lstyle rstyle, parent: EOOTypeOp style ROpen} -> Bool
sem rightAllowed_EOOTypeOp =
  | _ ->
    true
  sem groupingsAllowed_EOOTypeOp : all lstyle. all rstyle. (EOOTypeOp lstyle ROpen, EOOTypeOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_EOOTypeOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_EOOTypeOp : all lstyle. all rstyle. EOOTypeOp lstyle rstyle -> AllowedDirection
sem parenAllowed_EOOTypeOp =
  | _ ->
    GEither
      {}
  sem getInfo_EOOTypeOp : all lstyle. all rstyle. EOOTypeOp lstyle rstyle -> Info
  sem getTerms_EOOTypeOp : all lstyle. all rstyle. EOOTypeOp lstyle rstyle -> [Info]
  sem unsplit_EOOTypeOp : PermanentNode EOOTypeOp -> (Info, EOOType)
end
lang EOOModelHeadOpBase =
  EOOAst
  syn EOOModelHeadOp lstyle rstyle =
  sem topAllowed_EOOModelHeadOp : all lstyle. all rstyle. EOOModelHeadOp lstyle rstyle -> Bool
sem topAllowed_EOOModelHeadOp =
  | _ ->
    true
  sem leftAllowed_EOOModelHeadOp : all lstyle. all style. all rstyle. {child: EOOModelHeadOp lstyle rstyle, parent: EOOModelHeadOp LOpen style} -> Bool
sem leftAllowed_EOOModelHeadOp =
  | _ ->
    true
  sem rightAllowed_EOOModelHeadOp : all style. all lstyle. all rstyle. {child: EOOModelHeadOp lstyle rstyle, parent: EOOModelHeadOp style ROpen} -> Bool
sem rightAllowed_EOOModelHeadOp =
  | _ ->
    true
  sem groupingsAllowed_EOOModelHeadOp : all lstyle. all rstyle. (EOOModelHeadOp lstyle ROpen, EOOModelHeadOp LOpen rstyle) -> AllowedDirection
sem groupingsAllowed_EOOModelHeadOp =
  | _ ->
    GEither
      {}
  sem parenAllowed_EOOModelHeadOp : all lstyle. all rstyle. EOOModelHeadOp lstyle rstyle -> AllowedDirection
sem parenAllowed_EOOModelHeadOp =
  | _ ->
    GEither
      {}
  sem getInfo_EOOModelHeadOp : all lstyle. all rstyle. EOOModelHeadOp lstyle rstyle -> Info
  sem getTerms_EOOModelHeadOp : all lstyle. all rstyle. EOOModelHeadOp lstyle rstyle -> [Info]
  sem unsplit_EOOModelHeadOp : PermanentNode EOOModelHeadOp -> (Info, EOOModelHead)
end
lang ProgEOOProgOp =
  EOOProgOpBase
  + ProgEOOProgAst
  syn EOOProgOp lstyle rstyle =
  | ProgEOOProgOp {eqn: [EOOExpr], tops: [EOOTop], eqnkw: [Info], heads: [EOOModelHead], output: [EOOExpr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOProgOp =
  | ProgEOOProgOp x ->
    x.__br_info
  sem getTerms_EOOProgOp =
  | ProgEOOProgOp x ->
    x.__br_terms
  sem unsplit_EOOProgOp =
  | AtomP {self = ProgEOOProgOp x} ->
    (x.__br_info, ProgEOOProg
      { tops =
          x.tops,
        info =
          x.__br_info,
        heads =
          x.heads,
        eqn =
          match
            x.eqn
          with
            [ x1 ] ++ _ ++ ""
          in
          x1,
        eqnkw =
          match
            x.eqnkw
          with
            [ x2 ] ++ _ ++ ""
          in
          x2,
        output =
          match
            x.output
          with
            [ x3 ] ++ _ ++ ""
          in
          x3 })
end
lang DefEOOTopOp =
  EOOTopOpBase
  + DefEOOTopAst
  syn EOOTopOp lstyle rstyle =
  | DefEOOTopOp {binding: [EOODefBinding], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOTopOp =
  | DefEOOTopOp x ->
    x.__br_info
  sem getTerms_EOOTopOp =
  | DefEOOTopOp x ->
    x.__br_terms
  sem unsplit_EOOTopOp =
  | AtomP {self = DefEOOTopOp x} ->
    (x.__br_info, DefEOOTop
      { info =
          x.__br_info,
        binding =
          match
            x.binding
          with
            [ x1 ] ++ _ ++ ""
          in
          x1 })
end
lang ModelEOOTopOp =
  EOOTopOpBase
  + ModelEOOTopAst
  syn EOOTopOp lstyle rstyle =
  | ModelEOOTopOp {binding: [EOOModelBinding], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOTopOp =
  | ModelEOOTopOp x ->
    x.__br_info
  sem getTerms_EOOTopOp =
  | ModelEOOTopOp x ->
    x.__br_terms
  sem unsplit_EOOTopOp =
  | AtomP {self = ModelEOOTopOp x} ->
    (x.__br_info, ModelEOOTop
      { info =
          x.__br_info,
        binding =
          match
            x.binding
          with
            [ x1 ] ++ _ ++ ""
          in
          x1 })
end
lang TypeEOOTopOp =
  EOOTopOpBase
  + TypeEOOTopAst
  syn EOOTopOp lstyle rstyle =
  | TypeEOOTopOp {n: [{i: Info, v: Name}], rhs: [EOOType], params: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOTopOp =
  | TypeEOOTopOp x ->
    x.__br_info
  sem getTerms_EOOTopOp =
  | TypeEOOTopOp x ->
    x.__br_terms
  sem unsplit_EOOTopOp =
  | AtomP {self = TypeEOOTopOp x} ->
    (x.__br_info, TypeEOOTop
      { rhs =
          match
            x.rhs
          with
            [ x1 ] ++ _ ++ ""
          in
          x1,
        info =
          x.__br_info,
        params =
          x.params,
        n =
          match
            x.n
          with
            [ x2 ] ++ _ ++ ""
          in
          x2 })
end
lang PatEOOParamOp =
  EOOParamOpBase
  + PatEOOParamAst
  syn EOOParamOp lstyle rstyle =
  | PatEOOParamOp {pat: [EOOPat], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOParamOp =
  | PatEOOParamOp x ->
    x.__br_info
  sem getTerms_EOOParamOp =
  | PatEOOParamOp x ->
    x.__br_terms
  sem unsplit_EOOParamOp =
  | AtomP {self = PatEOOParamOp x} ->
    (x.__br_info, PatEOOParam
      { info =
          x.__br_info,
        pat =
          match
            x.pat
          with
            [ x1 ] ++ _ ++ ""
          in
          x1 })
end
lang NameEOOParamOp =
  EOOParamOpBase
  + NameEOOParamAst
  syn EOOParamOp lstyle rstyle =
  | NameEOOParamOp {n: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOParamOp =
  | NameEOOParamOp x ->
    x.__br_info
  sem getTerms_EOOParamOp =
  | NameEOOParamOp x ->
    x.__br_terms
  sem unsplit_EOOParamOp =
  | AtomP {self = NameEOOParamOp x} ->
    (x.__br_info, NameEOOParam
      { info =
          x.__br_info,
        n =
          match
            x.n
          with
            [ x1 ] ++ _ ++ ""
          in
          x1 })
end
lang IgnoreEOOParamOp =
  EOOParamOpBase
  + IgnoreEOOParamAst
  syn EOOParamOp lstyle rstyle =
  | IgnoreEOOParamOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOParamOp =
  | IgnoreEOOParamOp x ->
    x.__br_info
  sem getTerms_EOOParamOp =
  | IgnoreEOOParamOp x ->
    x.__br_terms
  sem unsplit_EOOParamOp =
  | AtomP {self = IgnoreEOOParamOp x} ->
    (x.__br_info, IgnoreEOOParam
      { info =
          x.__br_info })
end
lang SimpleEOODefBindingOp =
  EOODefBindingOpBase
  + SimpleEOODefBindingAst
  syn EOODefBindingOp lstyle rstyle =
  | SimpleEOODefBindingOp {n: [{i: Info, v: Name}], n2: [{i: Info, v: Name}], ty: [EOOType], rhs: [EOOExpr], params: [EOOParam], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOODefBindingOp =
  | SimpleEOODefBindingOp x ->
    x.__br_info
  sem getTerms_EOODefBindingOp =
  | SimpleEOODefBindingOp x ->
    x.__br_terms
  sem unsplit_EOODefBindingOp =
  | AtomP {self = SimpleEOODefBindingOp x} ->
    (x.__br_info, SimpleEOODefBinding
      { rhs =
          match
            x.rhs
          with
            [ x1 ] ++ _ ++ ""
          in
          x1,
        info =
          x.__br_info,
        params =
          x.params,
        n =
          match
            x.n
          with
            [ x2 ] ++ _ ++ ""
          in
          x2,
        n2 =
          match
            x.n2
          with
            [ x3 ] ++ _ ++ ""
          then
            Some
              x3
          else
            None
              {},
        ty =
          match
            x.ty
          with
            [ x4 ] ++ _ ++ ""
          then
            Some
              x4
          else
            None
              {} })
end
lang SimpleEOOModelBindingOp =
  EOOModelBindingOpBase
  + SimpleEOOModelBindingAst
  syn EOOModelBindingOp lstyle rstyle =
  | SimpleEOOModelBindingOp {n: [{i: Info, v: Name}], n2: [{i: Info, v: Name}], ty: [EOOType], eqn: [EOOExpr], eqnkw: [Info], heads: [EOOModelHead], params: [EOOParam], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOModelBindingOp =
  | SimpleEOOModelBindingOp x ->
    x.__br_info
  sem getTerms_EOOModelBindingOp =
  | SimpleEOOModelBindingOp x ->
    x.__br_terms
  sem unsplit_EOOModelBindingOp =
  | AtomP {self = SimpleEOOModelBindingOp x} ->
    (x.__br_info, SimpleEOOModelBinding
      { info =
          x.__br_info,
        heads =
          x.heads,
        eqn =
          match
            x.eqn
          with
            [ x1 ] ++ _ ++ ""
          in
          x1,
        eqnkw =
          match
            x.eqnkw
          with
            [ x2 ] ++ _ ++ ""
          in
          x2,
        params =
          x.params,
        n =
          match
            x.n
          with
            [ x3 ] ++ _ ++ ""
          in
          x3,
        n2 =
          match
            x.n2
          with
            [ x4 ] ++ _ ++ ""
          then
            Some
              x4
          else
            None
              {},
        ty =
          match
            x.ty
          with
            [ x5 ] ++ _ ++ ""
          then
            Some
              x5
          else
            None
              {} })
end
lang VarEOOExprOp =
  EOOExprOpBase
  + VarEOOExprAst
  syn EOOExprOp lstyle rstyle =
  | VarEOOExprOp {n: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | VarEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | VarEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | AtomP {self = VarEOOExprOp x} ->
    (x.__br_info, VarEOOExpr
      { info =
          x.__br_info,
        n =
          match
            x.n
          with
            [ x1 ] ++ _ ++ ""
          in
          x1 })
end
lang AbsEOOExprOp =
  EOOExprOpBase
  + AbsEOOExprAst
  syn EOOExprOp lstyle rstyle =
  | AbsEOOExprOp {ty: [EOOType], param: [EOOParam], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | AbsEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | AbsEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | PrefixP {self = AbsEOOExprOp x, rightChildAlts = [ r ] ++ _ ++ ""} ->
    match
      unsplit_EOOExprOp
        r
    with
      (rinfo, r)
    in
    let info =
        mergeInfo
          x.__br_info
          rinfo
      in
      (info, AbsEOOExpr
        { info =
            info,
          ty =
            match
              x.ty
            with
              [ x1 ] ++ _ ++ ""
            then
              Some
                x1
            else
              None
                {},
          param =
            match
              x.param
            with
              [ x2 ] ++ _ ++ ""
            in
            x2,
          body =
            match
              [ r ]
            with
              [ x3 ] ++ _ ++ ""
            in
            x3 })
end
lang AppEOOExprOp =
  EOOExprOpBase
  + AppEOOExprAst
  sem groupingsAllowed_EOOExprOp =
  | (AppEOOExprOp _, AppEOOExprOp _) ->
    GLeft
      {}
  syn EOOExprOp lstyle rstyle =
  | AppEOOExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | AppEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | AppEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | InfixP {self = AppEOOExprOp x, rightChildAlts = [ r ] ++ _ ++ "", leftChildAlts = [ l ] ++ _ ++ ""} ->
    match
      (unsplit_EOOExprOp
        l, unsplit_EOOExprOp
        r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, AppEOOExpr
        { info =
            info,
          left =
            match
              [ l ]
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          right =
            match
              [ r ]
            with
              [ x2 ] ++ _ ++ ""
            in
            x2 })
end
lang NumEOOExprOp =
  EOOExprOpBase
  + NumEOOExprAst
  syn EOOExprOp lstyle rstyle =
  | NumEOOExprOp {f: [{i: Info, v: Float}], i: [{i: Info, v: Int}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | NumEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | NumEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | AtomP {self = NumEOOExprOp x} ->
    (x.__br_info, NumEOOExpr
      { info =
          x.__br_info,
        i =
          match
            x.i
          with
            [ x1 ] ++ _ ++ ""
          then
            Some
              x1
          else
            None
              {},
        f =
          match
            x.f
          with
            [ x2 ] ++ _ ++ ""
          then
            Some
              x2
          else
            None
              {} })
end
lang StringEOOExprOp =
  EOOExprOpBase
  + StringEOOExprAst
  syn EOOExprOp lstyle rstyle =
  | StringEOOExprOp {v: [{i: Info, v: String}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | StringEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | StringEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | AtomP {self = StringEOOExprOp x} ->
    (x.__br_info, StringEOOExpr
      { info =
          x.__br_info,
        v =
          match
            x.v
          with
            [ x1 ] ++ _ ++ ""
          in
          x1 })
end
lang TrueEOOExprOp =
  EOOExprOpBase
  + TrueEOOExprAst
  syn EOOExprOp lstyle rstyle =
  | TrueEOOExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | TrueEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | TrueEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | AtomP {self = TrueEOOExprOp x} ->
    (x.__br_info, TrueEOOExpr
      { info =
          x.__br_info })
end
lang FalseEOOExprOp =
  EOOExprOpBase
  + FalseEOOExprAst
  syn EOOExprOp lstyle rstyle =
  | FalseEOOExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | FalseEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | FalseEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | AtomP {self = FalseEOOExprOp x} ->
    (x.__br_info, FalseEOOExpr
      { info =
          x.__br_info })
end
lang UnitEOOExprOp =
  EOOExprOpBase
  + UnitEOOExprAst
  syn EOOExprOp lstyle rstyle =
  | UnitEOOExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | UnitEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | UnitEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | AtomP {self = UnitEOOExprOp x} ->
    (x.__br_info, UnitEOOExpr
      { info =
          x.__br_info })
end
lang TupEOOExprOp =
  EOOExprOpBase
  + TupEOOExprAst
  sem groupingsAllowed_EOOExprOp =
  | (TupEOOExprOp _, TupEOOExprOp _) ->
    GLeft
      {}
  syn EOOExprOp lstyle rstyle =
  | TupEOOExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | TupEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | TupEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | InfixP {self = TupEOOExprOp x, rightChildAlts = [ r ] ++ _ ++ "", leftChildAlts = [ l ] ++ _ ++ ""} ->
    match
      (unsplit_EOOExprOp
        l, unsplit_EOOExprOp
        r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, TupEOOExpr
        { info =
            info,
          left =
            match
              [ l ]
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          right =
            match
              [ r ]
            with
              [ x2 ] ++ _ ++ ""
            in
            x2 })
end
lang ConcatEOOExprOp =
  EOOExprOpBase
  + ConcatEOOExprAst
  sem groupingsAllowed_EOOExprOp =
  | (ConcatEOOExprOp _, ConcatEOOExprOp _) ->
    GRight
      {}
  syn EOOExprOp lstyle rstyle =
  | ConcatEOOExprOp {op: [Info], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | ConcatEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | ConcatEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | InfixP {self = ConcatEOOExprOp x, rightChildAlts = [ r ] ++ _ ++ "", leftChildAlts = [ l ] ++ _ ++ ""} ->
    match
      (unsplit_EOOExprOp
        l, unsplit_EOOExprOp
        r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, ConcatEOOExpr
        { info =
            info,
          op =
            match
              x.op
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          left =
            match
              [ l ]
            with
              [ x2 ] ++ _ ++ ""
            in
            x2,
          right =
            match
              [ r ]
            with
              [ x3 ] ++ _ ++ ""
            in
            x3 })
end
lang ConsEOOExprOp =
  EOOExprOpBase
  + ConsEOOExprAst
  sem groupingsAllowed_EOOExprOp =
  | (ConsEOOExprOp _, ConsEOOExprOp _) ->
    GRight
      {}
  syn EOOExprOp lstyle rstyle =
  | ConsEOOExprOp {op: [Info], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | ConsEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | ConsEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | InfixP {self = ConsEOOExprOp x, rightChildAlts = [ r ] ++ _ ++ "", leftChildAlts = [ l ] ++ _ ++ ""} ->
    match
      (unsplit_EOOExprOp
        l, unsplit_EOOExprOp
        r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, ConsEOOExpr
        { info =
            info,
          op =
            match
              x.op
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          left =
            match
              [ l ]
            with
              [ x2 ] ++ _ ++ ""
            in
            x2,
          right =
            match
              [ r ]
            with
              [ x3 ] ++ _ ++ ""
            in
            x3 })
end
lang SeqEOOExprOp =
  EOOExprOpBase
  + SeqEOOExprAst
  syn EOOExprOp lstyle rstyle =
  | SeqEOOExprOp {__br_info: Info, __br_terms: [Info], unbrokenElems: [EOOExpr]}
  sem getInfo_EOOExprOp =
  | SeqEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | SeqEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | AtomP {self = SeqEOOExprOp x} ->
    (x.__br_info, SeqEOOExpr
      { info =
          x.__br_info,
        unbrokenElems =
          match
            x.unbrokenElems
          with
            [ x1 ] ++ _ ++ ""
          then
            Some
              x1
          else
            None
              {} })
end
lang LetEOOExprOp =
  EOOExprOpBase
  + LetEOOExprAst
  syn EOOExprOp lstyle rstyle =
  | LetEOOExprOp {ty: [EOOType], pat: [EOOPat], rhs: [EOOExpr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | LetEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | LetEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | PrefixP {self = LetEOOExprOp x, rightChildAlts = [ r ] ++ _ ++ ""} ->
    match
      unsplit_EOOExprOp
        r
    with
      (rinfo, r)
    in
    let info =
        mergeInfo
          x.__br_info
          rinfo
      in
      (info, LetEOOExpr
        { rhs =
            match
              x.rhs
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          info =
            info,
          pat =
            match
              x.pat
            with
              [ x2 ] ++ _ ++ ""
            in
            x2,
          ty =
            match
              x.ty
            with
              [ x3 ] ++ _ ++ ""
            then
              Some
                x3
            else
              None
                {},
          body =
            match
              [ r ]
            with
              [ x4 ] ++ _ ++ ""
            in
            x4 })
end
lang DefEOOExprOp =
  EOOExprOpBase
  + DefEOOExprAst
  syn EOOExprOp lstyle rstyle =
  | DefEOOExprOp {binding: [EOODefBinding], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | DefEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | DefEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | PrefixP {self = DefEOOExprOp x, rightChildAlts = [ r ] ++ _ ++ ""} ->
    match
      unsplit_EOOExprOp
        r
    with
      (rinfo, r)
    in
    let info =
        mergeInfo
          x.__br_info
          rinfo
      in
      (info, DefEOOExpr
        { info =
            info,
          binding =
            match
              x.binding
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          body =
            match
              [ r ]
            with
              [ x2 ] ++ _ ++ ""
            in
            x2 })
end
lang IfEOOExprOp =
  EOOExprOpBase
  + IfEOOExprAst
  syn EOOExprOp lstyle rstyle =
  | IfEOOExprOp {c: [EOOExpr], t: [EOOExpr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | IfEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | IfEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | PrefixP {self = IfEOOExprOp x, rightChildAlts = [ r ] ++ _ ++ ""} ->
    match
      unsplit_EOOExprOp
        r
    with
      (rinfo, r)
    in
    let info =
        mergeInfo
          x.__br_info
          rinfo
      in
      (info, IfEOOExpr
        { info =
            info,
          c =
            match
              x.c
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          t =
            match
              x.t
            with
              [ x2 ] ++ _ ++ ""
            in
            x2,
          e =
            match
              [ r ]
            with
              [ x3 ] ++ _ ++ ""
            in
            x3 })
end
lang MatchingEOOExprOp =
  EOOExprOpBase
  + MatchingEOOExprAst
  syn EOOExprOp lstyle rstyle =
  | MatchingEOOExprOp {arms: [{pat: EOOPat, body: EOOExpr}], scrut: [EOOExpr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | MatchingEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | MatchingEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | AtomP {self = MatchingEOOExprOp x} ->
    (x.__br_info, MatchingEOOExpr
      { info =
          x.__br_info,
        arms =
          x.arms,
        scrut =
          match
            x.scrut
          with
            [ x1 ] ++ _ ++ ""
          in
          x1 })
end
lang AddfEOOExprOp =
  EOOExprOpBase
  + AddfEOOExprAst
  sem groupingsAllowed_EOOExprOp =
  | (AddfEOOExprOp _, AddfEOOExprOp _) ->
    GLeft
      {}
  syn EOOExprOp lstyle rstyle =
  | AddfEOOExprOp {op: [Info], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | AddfEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | AddfEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | InfixP {self = AddfEOOExprOp x, rightChildAlts = [ r ] ++ _ ++ "", leftChildAlts = [ l ] ++ _ ++ ""} ->
    match
      (unsplit_EOOExprOp
        l, unsplit_EOOExprOp
        r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, AddfEOOExpr
        { info =
            info,
          op =
            match
              x.op
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          left =
            match
              [ l ]
            with
              [ x2 ] ++ _ ++ ""
            in
            x2,
          right =
            match
              [ r ]
            with
              [ x3 ] ++ _ ++ ""
            in
            x3 })
end
lang SubfEOOExprOp =
  EOOExprOpBase
  + SubfEOOExprAst
  sem groupingsAllowed_EOOExprOp =
  | (SubfEOOExprOp _, SubfEOOExprOp _) ->
    GLeft
      {}
  syn EOOExprOp lstyle rstyle =
  | SubfEOOExprOp {op: [Info], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | SubfEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | SubfEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | InfixP {self = SubfEOOExprOp x, rightChildAlts = [ r ] ++ _ ++ "", leftChildAlts = [ l ] ++ _ ++ ""} ->
    match
      (unsplit_EOOExprOp
        l, unsplit_EOOExprOp
        r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, SubfEOOExpr
        { info =
            info,
          op =
            match
              x.op
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          left =
            match
              [ l ]
            with
              [ x2 ] ++ _ ++ ""
            in
            x2,
          right =
            match
              [ r ]
            with
              [ x3 ] ++ _ ++ ""
            in
            x3 })
end
lang MulfEOOExprOp =
  EOOExprOpBase
  + MulfEOOExprAst
  sem groupingsAllowed_EOOExprOp =
  | (MulfEOOExprOp _, MulfEOOExprOp _) ->
    GLeft
      {}
  syn EOOExprOp lstyle rstyle =
  | MulfEOOExprOp {op: [Info], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | MulfEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | MulfEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | InfixP {self = MulfEOOExprOp x, rightChildAlts = [ r ] ++ _ ++ "", leftChildAlts = [ l ] ++ _ ++ ""} ->
    match
      (unsplit_EOOExprOp
        l, unsplit_EOOExprOp
        r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, MulfEOOExpr
        { info =
            info,
          op =
            match
              x.op
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          left =
            match
              [ l ]
            with
              [ x2 ] ++ _ ++ ""
            in
            x2,
          right =
            match
              [ r ]
            with
              [ x3 ] ++ _ ++ ""
            in
            x3 })
end
lang DivfEOOExprOp =
  EOOExprOpBase
  + DivfEOOExprAst
  sem groupingsAllowed_EOOExprOp =
  | (DivfEOOExprOp _, DivfEOOExprOp _) ->
    GLeft
      {}
  syn EOOExprOp lstyle rstyle =
  | DivfEOOExprOp {op: [Info], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | DivfEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | DivfEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | InfixP {self = DivfEOOExprOp x, rightChildAlts = [ r ] ++ _ ++ "", leftChildAlts = [ l ] ++ _ ++ ""} ->
    match
      (unsplit_EOOExprOp
        l, unsplit_EOOExprOp
        r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, DivfEOOExpr
        { info =
            info,
          op =
            match
              x.op
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          left =
            match
              [ l ]
            with
              [ x2 ] ++ _ ++ ""
            in
            x2,
          right =
            match
              [ r ]
            with
              [ x3 ] ++ _ ++ ""
            in
            x3 })
end
lang PrimEOOExprOp =
  EOOExprOpBase
  + PrimEOOExprAst
  syn EOOExprOp lstyle rstyle =
  | PrimEOOExprOp {op: [Info], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | PrimEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | PrimEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | PostfixP {self = PrimEOOExprOp x, leftChildAlts = [ l ] ++ _ ++ ""} ->
    match
      unsplit_EOOExprOp
        l
    with
      (linfo, l)
    in
    let info =
        mergeInfo
          linfo
          x.__br_info
      in
      (info, PrimEOOExpr
        { info =
            info,
          op =
            match
              x.op
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          left =
            match
              [ l ]
            with
              [ x2 ] ++ _ ++ ""
            in
            x2 })
end
lang EqnEOOExprOp =
  EOOExprOpBase
  + EqnEOOExprAst
  syn EOOExprOp lstyle rstyle =
  | EqnEOOExprOp {op: [Info], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | EqnEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | EqnEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | InfixP {self = EqnEOOExprOp x, rightChildAlts = [ r ] ++ _ ++ "", leftChildAlts = [ l ] ++ _ ++ ""} ->
    match
      (unsplit_EOOExprOp
        l, unsplit_EOOExprOp
        r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, EqnEOOExpr
        { info =
            info,
          op =
            match
              x.op
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          left =
            match
              [ l ]
            with
              [ x2 ] ++ _ ++ ""
            in
            x2,
          right =
            match
              [ r ]
            with
              [ x3 ] ++ _ ++ ""
            in
            x3 })
end
lang InitEOOExprOp =
  EOOExprOpBase
  + InitEOOExprAst
  syn EOOExprOp lstyle rstyle =
  | InitEOOExprOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | InitEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | InitEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | PrefixP {self = InitEOOExprOp x, rightChildAlts = [ r ] ++ _ ++ ""} ->
    match
      unsplit_EOOExprOp
        r
    with
      (rinfo, r)
    in
    let info =
        mergeInfo
          x.__br_info
          rinfo
      in
      (info, InitEOOExpr
        { info =
            info,
          right =
            match
              [ r ]
            with
              [ x1 ] ++ _ ++ ""
            in
            x1 })
end
lang ConnectEOOExprOp =
  EOOExprOpBase
  + ConnectEOOExprAst
  syn EOOExprOp lstyle rstyle =
  | ConnectEOOExprOp {to: [EOOExpr], dom: [EOOExpr], from: [EOOExpr], across: [EOOExpr], through: [EOOExpr], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | ConnectEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | ConnectEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | AtomP {self = ConnectEOOExprOp x} ->
    (x.__br_info, ConnectEOOExpr
      { info =
          x.__br_info,
        to =
          match
            x.to
          with
            [ x1 ] ++ _ ++ ""
          in
          x1,
        dom =
          match
            x.dom
          with
            [ x2 ] ++ _ ++ ""
          in
          x2,
        from =
          match
            x.from
          with
            [ x3 ] ++ _ ++ ""
          in
          x3,
        across =
          match
            x.across
          with
            [ x4 ] ++ _ ++ ""
          in
          x4,
        through =
          match
            x.through
          with
            [ x5 ] ++ _ ++ ""
          in
          x5 })
end
lang ModelEOOExprOp =
  EOOExprOpBase
  + ModelEOOExprAst
  syn EOOExprOp lstyle rstyle =
  | ModelEOOExprOp {eqnkw: [Info], heads: [EOOModelHead], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | ModelEOOExprOp x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | ModelEOOExprOp x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | PrefixP {self = ModelEOOExprOp x, rightChildAlts = [ r ] ++ _ ++ ""} ->
    match
      unsplit_EOOExprOp
        r
    with
      (rinfo, r)
    in
    let info =
        mergeInfo
          x.__br_info
          rinfo
      in
      (info, ModelEOOExpr
        { info =
            info,
          heads =
            x.heads,
          eqn =
            match
              [ r ]
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          eqnkw =
            match
              x.eqnkw
            with
              [ x2 ] ++ _ ++ ""
            in
            x2 })
end
lang VarEOOModelHeadOp =
  EOOModelHeadOpBase
  + VarEOOModelHeadAst
  syn EOOModelHeadOp lstyle rstyle =
  | VarEOOModelHeadOp {ns: [{i: Info, v: Name}], ty: [EOOType], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOModelHeadOp =
  | VarEOOModelHeadOp x ->
    x.__br_info
  sem getTerms_EOOModelHeadOp =
  | VarEOOModelHeadOp x ->
    x.__br_terms
  sem unsplit_EOOModelHeadOp =
  | AtomP {self = VarEOOModelHeadOp x} ->
    (x.__br_info, VarEOOModelHead
      { info =
          x.__br_info,
        ty =
          match
            x.ty
          with
            [ x1 ] ++ _ ++ ""
          in
          x1,
        ns =
          x.ns })
end
lang NodeEOOModelHeadOp =
  EOOModelHeadOpBase
  + NodeEOOModelHeadAst
  syn EOOModelHeadOp lstyle rstyle =
  | NodeEOOModelHeadOp {ns: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOModelHeadOp =
  | NodeEOOModelHeadOp x ->
    x.__br_info
  sem getTerms_EOOModelHeadOp =
  | NodeEOOModelHeadOp x ->
    x.__br_terms
  sem unsplit_EOOModelHeadOp =
  | AtomP {self = NodeEOOModelHeadOp x} ->
    (x.__br_info, NodeEOOModelHead
      { info =
          x.__br_info,
        ns =
          x.ns })
end
lang DefEOOModelHeadOp =
  EOOModelHeadOpBase
  + DefEOOModelHeadAst
  syn EOOModelHeadOp lstyle rstyle =
  | DefEOOModelHeadOp {binding: [EOODefBinding], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOModelHeadOp =
  | DefEOOModelHeadOp x ->
    x.__br_info
  sem getTerms_EOOModelHeadOp =
  | DefEOOModelHeadOp x ->
    x.__br_terms
  sem unsplit_EOOModelHeadOp =
  | AtomP {self = DefEOOModelHeadOp x} ->
    (x.__br_info, DefEOOModelHead
      { info =
          x.__br_info,
        binding =
          match
            x.binding
          with
            [ x1 ] ++ _ ++ ""
          in
          x1 })
end
lang ModelEOOModelHeadOp =
  EOOModelHeadOpBase
  + ModelEOOModelHeadAst
  syn EOOModelHeadOp lstyle rstyle =
  | ModelEOOModelHeadOp {binding: [EOOModelBinding], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOModelHeadOp =
  | ModelEOOModelHeadOp x ->
    x.__br_info
  sem getTerms_EOOModelHeadOp =
  | ModelEOOModelHeadOp x ->
    x.__br_terms
  sem unsplit_EOOModelHeadOp =
  | AtomP {self = ModelEOOModelHeadOp x} ->
    (x.__br_info, ModelEOOModelHead
      { info =
          x.__br_info,
        binding =
          match
            x.binding
          with
            [ x1 ] ++ _ ++ ""
          in
          x1 })
end
lang WildEOOPatOp =
  EOOPatOpBase
  + WildEOOPatAst
  syn EOOPatOp lstyle rstyle =
  | WildEOOPatOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOPatOp =
  | WildEOOPatOp x ->
    x.__br_info
  sem getTerms_EOOPatOp =
  | WildEOOPatOp x ->
    x.__br_terms
  sem unsplit_EOOPatOp =
  | AtomP {self = WildEOOPatOp x} ->
    (x.__br_info, WildEOOPat
      { info =
          x.__br_info })
end
lang BindEOOPatOp =
  EOOPatOpBase
  + BindEOOPatAst
  syn EOOPatOp lstyle rstyle =
  | BindEOOPatOp {n: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOPatOp =
  | BindEOOPatOp x ->
    x.__br_info
  sem getTerms_EOOPatOp =
  | BindEOOPatOp x ->
    x.__br_terms
  sem unsplit_EOOPatOp =
  | AtomP {self = BindEOOPatOp x} ->
    (x.__br_info, BindEOOPat
      { info =
          x.__br_info,
        n =
          match
            x.n
          with
            [ x1 ] ++ _ ++ ""
          in
          x1 })
end
lang UnitEOOPatOp =
  EOOPatOpBase
  + UnitEOOPatAst
  syn EOOPatOp lstyle rstyle =
  | UnitEOOPatOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOPatOp =
  | UnitEOOPatOp x ->
    x.__br_info
  sem getTerms_EOOPatOp =
  | UnitEOOPatOp x ->
    x.__br_terms
  sem unsplit_EOOPatOp =
  | AtomP {self = UnitEOOPatOp x} ->
    (x.__br_info, UnitEOOPat
      { info =
          x.__br_info })
end
lang TupEOOPatOp =
  EOOPatOpBase
  + TupEOOPatAst
  sem groupingsAllowed_EOOPatOp =
  | (TupEOOPatOp _, TupEOOPatOp _) ->
    GLeft
      {}
  syn EOOPatOp lstyle rstyle =
  | TupEOOPatOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOPatOp =
  | TupEOOPatOp x ->
    x.__br_info
  sem getTerms_EOOPatOp =
  | TupEOOPatOp x ->
    x.__br_terms
  sem unsplit_EOOPatOp =
  | InfixP {self = TupEOOPatOp x, rightChildAlts = [ r ] ++ _ ++ "", leftChildAlts = [ l ] ++ _ ++ ""} ->
    match
      (unsplit_EOOPatOp
        l, unsplit_EOOPatOp
        r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, TupEOOPat
        { info =
            info,
          left =
            match
              [ l ]
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          right =
            match
              [ r ]
            with
              [ x2 ] ++ _ ++ ""
            in
            x2 })
end
lang ConsEOOPatOp =
  EOOPatOpBase
  + ConsEOOPatAst
  sem groupingsAllowed_EOOPatOp =
  | (ConsEOOPatOp _, ConsEOOPatOp _) ->
    GRight
      {}
  syn EOOPatOp lstyle rstyle =
  | ConsEOOPatOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOPatOp =
  | ConsEOOPatOp x ->
    x.__br_info
  sem getTerms_EOOPatOp =
  | ConsEOOPatOp x ->
    x.__br_terms
  sem unsplit_EOOPatOp =
  | InfixP {self = ConsEOOPatOp x, rightChildAlts = [ r ] ++ _ ++ "", leftChildAlts = [ l ] ++ _ ++ ""} ->
    match
      (unsplit_EOOPatOp
        l, unsplit_EOOPatOp
        r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, ConsEOOPat
        { info =
            info,
          left =
            match
              [ l ]
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          right =
            match
              [ r ]
            with
              [ x2 ] ++ _ ++ ""
            in
            x2 })
end
lang SeqEOOPatOp =
  EOOPatOpBase
  + SeqEOOPatAst
  syn EOOPatOp lstyle rstyle =
  | SeqEOOPatOp {elems: [EOOPat], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOPatOp =
  | SeqEOOPatOp x ->
    x.__br_info
  sem getTerms_EOOPatOp =
  | SeqEOOPatOp x ->
    x.__br_terms
  sem unsplit_EOOPatOp =
  | AtomP {self = SeqEOOPatOp x} ->
    (x.__br_info, SeqEOOPat
      { info =
          x.__br_info,
        elems =
          match
            x.elems
          with
            [ x1 ] ++ _ ++ ""
          in
          x1 })
end
lang VarEOOTypeOp =
  EOOTypeOpBase
  + VarEOOTypeAst
  syn EOOTypeOp lstyle rstyle =
  | VarEOOTypeOp {n: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOTypeOp =
  | VarEOOTypeOp x ->
    x.__br_info
  sem getTerms_EOOTypeOp =
  | VarEOOTypeOp x ->
    x.__br_terms
  sem unsplit_EOOTypeOp =
  | AtomP {self = VarEOOTypeOp x} ->
    (x.__br_info, VarEOOType
      { info =
          x.__br_info,
        n =
          match
            x.n
          with
            [ x1 ] ++ _ ++ ""
          in
          x1 })
end
lang ConEOOTypeOp =
  EOOTypeOpBase
  + ConEOOTypeAst
  syn EOOTypeOp lstyle rstyle =
  | ConEOOTypeOp {n: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOTypeOp =
  | ConEOOTypeOp x ->
    x.__br_info
  sem getTerms_EOOTypeOp =
  | ConEOOTypeOp x ->
    x.__br_terms
  sem unsplit_EOOTypeOp =
  | AtomP {self = ConEOOTypeOp x} ->
    (x.__br_info, ConEOOType
      { info =
          x.__br_info,
        n =
          match
            x.n
          with
            [ x1 ] ++ _ ++ ""
          in
          x1 })
end
lang AppEOOTypeOp =
  EOOTypeOpBase
  + AppEOOTypeAst
  sem groupingsAllowed_EOOTypeOp =
  | (AppEOOTypeOp _, AppEOOTypeOp _) ->
    GLeft
      {}
  syn EOOTypeOp lstyle rstyle =
  | AppEOOTypeOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOTypeOp =
  | AppEOOTypeOp x ->
    x.__br_info
  sem getTerms_EOOTypeOp =
  | AppEOOTypeOp x ->
    x.__br_terms
  sem unsplit_EOOTypeOp =
  | InfixP {self = AppEOOTypeOp x, rightChildAlts = [ r ] ++ _ ++ "", leftChildAlts = [ l ] ++ _ ++ ""} ->
    match
      (unsplit_EOOTypeOp
        l, unsplit_EOOTypeOp
        r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, AppEOOType
        { info =
            info,
          left =
            match
              [ l ]
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          right =
            match
              [ r ]
            with
              [ x2 ] ++ _ ++ ""
            in
            x2 })
end
lang TupEOOTypeOp =
  EOOTypeOpBase
  + TupEOOTypeAst
  sem groupingsAllowed_EOOTypeOp =
  | (TupEOOTypeOp _, TupEOOTypeOp _) ->
    GLeft
      {}
  syn EOOTypeOp lstyle rstyle =
  | TupEOOTypeOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOTypeOp =
  | TupEOOTypeOp x ->
    x.__br_info
  sem getTerms_EOOTypeOp =
  | TupEOOTypeOp x ->
    x.__br_terms
  sem unsplit_EOOTypeOp =
  | InfixP {self = TupEOOTypeOp x, rightChildAlts = [ r ] ++ _ ++ "", leftChildAlts = [ l ] ++ _ ++ ""} ->
    match
      (unsplit_EOOTypeOp
        l, unsplit_EOOTypeOp
        r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, TupEOOType
        { info =
            info,
          left =
            match
              [ l ]
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          right =
            match
              [ r ]
            with
              [ x2 ] ++ _ ++ ""
            in
            x2 })
end
lang SeqEOOTypeOp =
  EOOTypeOpBase
  + SeqEOOTypeAst
  syn EOOTypeOp lstyle rstyle =
  | SeqEOOTypeOp {ty: [EOOType], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOTypeOp =
  | SeqEOOTypeOp x ->
    x.__br_info
  sem getTerms_EOOTypeOp =
  | SeqEOOTypeOp x ->
    x.__br_terms
  sem unsplit_EOOTypeOp =
  | AtomP {self = SeqEOOTypeOp x} ->
    (x.__br_info, SeqEOOType
      { info =
          x.__br_info,
        ty =
          match
            x.ty
          with
            [ x1 ] ++ _ ++ ""
          in
          x1 })
end
lang ArrowEOOTypeOp =
  EOOTypeOpBase
  + ArrowEOOTypeAst
  sem groupingsAllowed_EOOTypeOp =
  | (ArrowEOOTypeOp _, ArrowEOOTypeOp _) ->
    GRight
      {}
  syn EOOTypeOp lstyle rstyle =
  | ArrowEOOTypeOp {__br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOTypeOp =
  | ArrowEOOTypeOp x ->
    x.__br_info
  sem getTerms_EOOTypeOp =
  | ArrowEOOTypeOp x ->
    x.__br_terms
  sem unsplit_EOOTypeOp =
  | InfixP {self = ArrowEOOTypeOp x, rightChildAlts = [ r ] ++ _ ++ "", leftChildAlts = [ l ] ++ _ ++ ""} ->
    match
      (unsplit_EOOTypeOp
        l, unsplit_EOOTypeOp
        r)
    with
      ((linfo, l), (rinfo, r))
    in
    let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, ArrowEOOType
        { info =
            info,
          left =
            match
              [ l ]
            with
              [ x1 ] ++ _ ++ ""
            in
            x1,
          right =
            match
              [ r ]
            with
              [ x2 ] ++ _ ++ ""
            in
            x2 })
end
lang ForallEOOTypeOp =
  EOOTypeOpBase
  + ForallEOOTypeAst
  syn EOOTypeOp lstyle rstyle =
  | ForallEOOTypeOp {ns: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOTypeOp =
  | ForallEOOTypeOp x ->
    x.__br_info
  sem getTerms_EOOTypeOp =
  | ForallEOOTypeOp x ->
    x.__br_terms
  sem unsplit_EOOTypeOp =
  | PrefixP {self = ForallEOOTypeOp x, rightChildAlts = [ r ] ++ _ ++ ""} ->
    match
      unsplit_EOOTypeOp
        r
    with
      (rinfo, r)
    in
    let info =
        mergeInfo
          x.__br_info
          rinfo
      in
      (info, ForallEOOType
        { info =
            info,
          ns =
            x.ns,
          right =
            match
              [ r ]
            with
              [ x1 ] ++ _ ++ ""
            in
            x1 })
end
lang EOOExprGrouping =
  EOOExprOpBase
  syn EOOExprOp lstyle rstyle =
  | EOOExprGrouping {inner: EOOExpr, __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOExprOp =
  | EOOExprGrouping x ->
    x.__br_info
  sem getTerms_EOOExprOp =
  | EOOExprGrouping x ->
    x.__br_terms
  sem unsplit_EOOExprOp =
  | AtomP {self = EOOExprGrouping x} ->
    (x.__br_info, x.inner)
end
lang EOOPatGrouping =
  EOOPatOpBase
  syn EOOPatOp lstyle rstyle =
  | EOOPatGrouping {inner: EOOPat, __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOPatOp =
  | EOOPatGrouping x ->
    x.__br_info
  sem getTerms_EOOPatOp =
  | EOOPatGrouping x ->
    x.__br_terms
  sem unsplit_EOOPatOp =
  | AtomP {self = EOOPatGrouping x} ->
    (x.__br_info, x.inner)
end
lang EOOTypeGrouping =
  EOOTypeOpBase
  syn EOOTypeOp lstyle rstyle =
  | EOOTypeGrouping {inner: EOOType, __br_info: Info, __br_terms: [Info]}
  sem getInfo_EOOTypeOp =
  | EOOTypeGrouping x ->
    x.__br_info
  sem getTerms_EOOTypeOp =
  | EOOTypeGrouping x ->
    x.__br_terms
  sem unsplit_EOOTypeOp =
  | AtomP {self = EOOTypeGrouping x} ->
    (x.__br_info, x.inner)
end
lang ParseEOO =
  ProgEOOProgOp
  + DefEOOTopOp
  + ModelEOOTopOp
  + TypeEOOTopOp
  + PatEOOParamOp
  + NameEOOParamOp
  + IgnoreEOOParamOp
  + SimpleEOODefBindingOp
  + SimpleEOOModelBindingOp
  + VarEOOExprOp
  + AbsEOOExprOp
  + AppEOOExprOp
  + NumEOOExprOp
  + StringEOOExprOp
  + TrueEOOExprOp
  + FalseEOOExprOp
  + UnitEOOExprOp
  + TupEOOExprOp
  + ConcatEOOExprOp
  + ConsEOOExprOp
  + SeqEOOExprOp
  + LetEOOExprOp
  + DefEOOExprOp
  + IfEOOExprOp
  + MatchingEOOExprOp
  + AddfEOOExprOp
  + SubfEOOExprOp
  + MulfEOOExprOp
  + DivfEOOExprOp
  + PrimEOOExprOp
  + EqnEOOExprOp
  + InitEOOExprOp
  + ConnectEOOExprOp
  + ModelEOOExprOp
  + VarEOOModelHeadOp
  + NodeEOOModelHeadOp
  + DefEOOModelHeadOp
  + ModelEOOModelHeadOp
  + WildEOOPatOp
  + BindEOOPatOp
  + UnitEOOPatOp
  + TupEOOPatOp
  + ConsEOOPatOp
  + SeqEOOPatOp
  + VarEOOTypeOp
  + ConEOOTypeOp
  + AppEOOTypeOp
  + TupEOOTypeOp
  + SeqEOOTypeOp
  + ArrowEOOTypeOp
  + ForallEOOTypeOp
  + EOOExprGrouping
  + EOOPatGrouping
  + EOOTypeGrouping
  + BadEOOProgAst
  + BadEOOTopAst
  + BadEOOParamAst
  + BadEOODefBindingAst
  + BadEOOModelBindingAst
  + BadEOOExprAst
  + BadEOOPatAst
  + BadEOOTypeAst
  + BadEOOModelHeadAst
  + LL1Parser
  + SemiTokenParser
  + UIntTokenParser
  + UnitTokenParser
  + CommaTokenParser
  + PrimeTokenParser
  + WhitespaceParser
  + LIdentTokenParser
  + LineCommentParser
  + StringTokenParser
  + UFloatTokenParser
  + UIdentTokenParser
  + BracketTokenParser
  + OperatorTokenParser
  + BackslashTokenParser
  + MultilineCommentParser
  
  
  
  
  
  sem groupingsAllowed_EOOExprOp =
  | (AbsEOOExprOp _, AppEOOExprOp _) ->
    GRight
      {}
  | (AbsEOOExprOp _, TupEOOExprOp _) ->
    GRight
      {}
  | (AbsEOOExprOp _, ConcatEOOExprOp _) ->
    GRight
      {}
  | (AbsEOOExprOp _, ConsEOOExprOp _) ->
    GRight
      {}
  | (AbsEOOExprOp _, AddfEOOExprOp _) ->
    GRight
      {}
  | (AbsEOOExprOp _, SubfEOOExprOp _) ->
    GRight
      {}
  | (AbsEOOExprOp _, MulfEOOExprOp _) ->
    GRight
      {}
  | (AbsEOOExprOp _, DivfEOOExprOp _) ->
    GRight
      {}
  | (AbsEOOExprOp _, PrimEOOExprOp _) ->
    GRight
      {}
  | (AbsEOOExprOp _, EqnEOOExprOp _) ->
    GRight
      {}
  | (AppEOOExprOp _, TupEOOExprOp _) ->
    GLeft
      {}
  | (TupEOOExprOp _, AppEOOExprOp _) ->
    GRight
      {}
  | (AppEOOExprOp _, ConcatEOOExprOp _) ->
    GLeft
      {}
  | (ConcatEOOExprOp _, AppEOOExprOp _) ->
    GRight
      {}
  | (AppEOOExprOp _, ConsEOOExprOp _) ->
    GLeft
      {}
  | (ConsEOOExprOp _, AppEOOExprOp _) ->
    GRight
      {}
  | (LetEOOExprOp _, AppEOOExprOp _) ->
    GRight
      {}
  | (DefEOOExprOp _, AppEOOExprOp _) ->
    GRight
      {}
  | (IfEOOExprOp _, AppEOOExprOp _) ->
    GRight
      {}
  | (AppEOOExprOp _, AddfEOOExprOp _) ->
    GLeft
      {}
  | (AddfEOOExprOp _, AppEOOExprOp _) ->
    GRight
      {}
  | (AppEOOExprOp _, SubfEOOExprOp _) ->
    GLeft
      {}
  | (SubfEOOExprOp _, AppEOOExprOp _) ->
    GRight
      {}
  | (AppEOOExprOp _, MulfEOOExprOp _) ->
    GLeft
      {}
  | (MulfEOOExprOp _, AppEOOExprOp _) ->
    GRight
      {}
  | (AppEOOExprOp _, DivfEOOExprOp _) ->
    GLeft
      {}
  | (DivfEOOExprOp _, AppEOOExprOp _) ->
    GRight
      {}
  | (AppEOOExprOp _, PrimEOOExprOp _) ->
    GRight
      {}
  | (AppEOOExprOp _, EqnEOOExprOp _) ->
    GLeft
      {}
  | (EqnEOOExprOp _, AppEOOExprOp _) ->
    GRight
      {}
  | (InitEOOExprOp _, AppEOOExprOp _) ->
    GRight
      {}
  | (ModelEOOExprOp _, AppEOOExprOp _) ->
    GRight
      {}
  | (TupEOOExprOp _, ConcatEOOExprOp _) ->
    GRight
      {}
  | (ConcatEOOExprOp _, TupEOOExprOp _) ->
    GLeft
      {}
  | (TupEOOExprOp _, ConsEOOExprOp _) ->
    GRight
      {}
  | (ConsEOOExprOp _, TupEOOExprOp _) ->
    GLeft
      {}
  | (LetEOOExprOp _, TupEOOExprOp _) ->
    GRight
      {}
  | (DefEOOExprOp _, TupEOOExprOp _) ->
    GRight
      {}
  | (IfEOOExprOp _, TupEOOExprOp _) ->
    GRight
      {}
  | (TupEOOExprOp _, AddfEOOExprOp _) ->
    GRight
      {}
  | (AddfEOOExprOp _, TupEOOExprOp _) ->
    GLeft
      {}
  | (TupEOOExprOp _, SubfEOOExprOp _) ->
    GRight
      {}
  | (SubfEOOExprOp _, TupEOOExprOp _) ->
    GLeft
      {}
  | (TupEOOExprOp _, MulfEOOExprOp _) ->
    GRight
      {}
  | (MulfEOOExprOp _, TupEOOExprOp _) ->
    GLeft
      {}
  | (TupEOOExprOp _, DivfEOOExprOp _) ->
    GRight
      {}
  | (DivfEOOExprOp _, TupEOOExprOp _) ->
    GLeft
      {}
  | (TupEOOExprOp _, PrimEOOExprOp _) ->
    GRight
      {}
  | (TupEOOExprOp _, EqnEOOExprOp _) ->
    GRight
      {}
  | (EqnEOOExprOp _, TupEOOExprOp _) ->
    GLeft
      {}
  | (InitEOOExprOp _, TupEOOExprOp _) ->
    GLeft
      {}
  | (ModelEOOExprOp _, TupEOOExprOp _) ->
    GRight
      {}
  | (ConcatEOOExprOp _, ConsEOOExprOp _) ->
    GRight
      {}
  | (ConsEOOExprOp _, ConcatEOOExprOp _) ->
    GRight
      {}
  | (LetEOOExprOp _, ConcatEOOExprOp _) ->
    GRight
      {}
  | (DefEOOExprOp _, ConcatEOOExprOp _) ->
    GRight
      {}
  | (IfEOOExprOp _, ConcatEOOExprOp _) ->
    GRight
      {}
  | (ConcatEOOExprOp _, AddfEOOExprOp _) ->
    GRight
      {}
  | (AddfEOOExprOp _, ConcatEOOExprOp _) ->
    GLeft
      {}
  | (ConcatEOOExprOp _, SubfEOOExprOp _) ->
    GRight
      {}
  | (SubfEOOExprOp _, ConcatEOOExprOp _) ->
    GLeft
      {}
  | (ConcatEOOExprOp _, MulfEOOExprOp _) ->
    GRight
      {}
  | (MulfEOOExprOp _, ConcatEOOExprOp _) ->
    GLeft
      {}
  | (ConcatEOOExprOp _, DivfEOOExprOp _) ->
    GRight
      {}
  | (DivfEOOExprOp _, ConcatEOOExprOp _) ->
    GLeft
      {}
  | (ConcatEOOExprOp _, PrimEOOExprOp _) ->
    GRight
      {}
  | (ConcatEOOExprOp _, EqnEOOExprOp _) ->
    GRight
      {}
  | (EqnEOOExprOp _, ConcatEOOExprOp _) ->
    GLeft
      {}
  | (InitEOOExprOp _, ConcatEOOExprOp _) ->
    GLeft
      {}
  | (ModelEOOExprOp _, ConcatEOOExprOp _) ->
    GRight
      {}
  | (LetEOOExprOp _, ConsEOOExprOp _) ->
    GRight
      {}
  | (DefEOOExprOp _, ConsEOOExprOp _) ->
    GRight
      {}
  | (IfEOOExprOp _, ConsEOOExprOp _) ->
    GRight
      {}
  | (ConsEOOExprOp _, AddfEOOExprOp _) ->
    GRight
      {}
  | (AddfEOOExprOp _, ConsEOOExprOp _) ->
    GLeft
      {}
  | (ConsEOOExprOp _, SubfEOOExprOp _) ->
    GRight
      {}
  | (SubfEOOExprOp _, ConsEOOExprOp _) ->
    GLeft
      {}
  | (ConsEOOExprOp _, MulfEOOExprOp _) ->
    GRight
      {}
  | (MulfEOOExprOp _, ConsEOOExprOp _) ->
    GLeft
      {}
  | (ConsEOOExprOp _, DivfEOOExprOp _) ->
    GRight
      {}
  | (DivfEOOExprOp _, ConsEOOExprOp _) ->
    GLeft
      {}
  | (ConsEOOExprOp _, PrimEOOExprOp _) ->
    GRight
      {}
  | (ConsEOOExprOp _, EqnEOOExprOp _) ->
    GRight
      {}
  | (EqnEOOExprOp _, ConsEOOExprOp _) ->
    GLeft
      {}
  | (InitEOOExprOp _, ConsEOOExprOp _) ->
    GLeft
      {}
  | (ModelEOOExprOp _, ConsEOOExprOp _) ->
    GRight
      {}
  | (LetEOOExprOp _, AddfEOOExprOp _) ->
    GRight
      {}
  | (LetEOOExprOp _, SubfEOOExprOp _) ->
    GRight
      {}
  | (LetEOOExprOp _, MulfEOOExprOp _) ->
    GRight
      {}
  | (LetEOOExprOp _, DivfEOOExprOp _) ->
    GRight
      {}
  | (LetEOOExprOp _, PrimEOOExprOp _) ->
    GRight
      {}
  | (LetEOOExprOp _, EqnEOOExprOp _) ->
    GRight
      {}
  | (DefEOOExprOp _, AddfEOOExprOp _) ->
    GRight
      {}
  | (DefEOOExprOp _, SubfEOOExprOp _) ->
    GRight
      {}
  | (DefEOOExprOp _, MulfEOOExprOp _) ->
    GRight
      {}
  | (DefEOOExprOp _, DivfEOOExprOp _) ->
    GRight
      {}
  | (DefEOOExprOp _, PrimEOOExprOp _) ->
    GRight
      {}
  | (DefEOOExprOp _, EqnEOOExprOp _) ->
    GRight
      {}
  | (IfEOOExprOp _, AddfEOOExprOp _) ->
    GRight
      {}
  | (IfEOOExprOp _, SubfEOOExprOp _) ->
    GRight
      {}
  | (IfEOOExprOp _, MulfEOOExprOp _) ->
    GRight
      {}
  | (IfEOOExprOp _, DivfEOOExprOp _) ->
    GRight
      {}
  | (IfEOOExprOp _, PrimEOOExprOp _) ->
    GRight
      {}
  | (IfEOOExprOp _, EqnEOOExprOp _) ->
    GRight
      {}
  | (AddfEOOExprOp _, SubfEOOExprOp _) ->
    GLeft
      {}
  | (SubfEOOExprOp _, AddfEOOExprOp _) ->
    GLeft
      {}
  | (AddfEOOExprOp _, MulfEOOExprOp _) ->
    GRight
      {}
  | (MulfEOOExprOp _, AddfEOOExprOp _) ->
    GLeft
      {}
  | (AddfEOOExprOp _, DivfEOOExprOp _) ->
    GRight
      {}
  | (DivfEOOExprOp _, AddfEOOExprOp _) ->
    GLeft
      {}
  | (AddfEOOExprOp _, PrimEOOExprOp _) ->
    GRight
      {}
  | (AddfEOOExprOp _, EqnEOOExprOp _) ->
    GLeft
      {}
  | (EqnEOOExprOp _, AddfEOOExprOp _) ->
    GRight
      {}
  | (InitEOOExprOp _, AddfEOOExprOp _) ->
    GRight
      {}
  | (ModelEOOExprOp _, AddfEOOExprOp _) ->
    GRight
      {}
  | (SubfEOOExprOp _, MulfEOOExprOp _) ->
    GRight
      {}
  | (MulfEOOExprOp _, SubfEOOExprOp _) ->
    GLeft
      {}
  | (SubfEOOExprOp _, DivfEOOExprOp _) ->
    GRight
      {}
  | (DivfEOOExprOp _, SubfEOOExprOp _) ->
    GLeft
      {}
  | (SubfEOOExprOp _, PrimEOOExprOp _) ->
    GRight
      {}
  | (SubfEOOExprOp _, EqnEOOExprOp _) ->
    GLeft
      {}
  | (EqnEOOExprOp _, SubfEOOExprOp _) ->
    GRight
      {}
  | (InitEOOExprOp _, SubfEOOExprOp _) ->
    GRight
      {}
  | (ModelEOOExprOp _, SubfEOOExprOp _) ->
    GRight
      {}
  | (MulfEOOExprOp _, DivfEOOExprOp _) ->
    GLeft
      {}
  | (DivfEOOExprOp _, MulfEOOExprOp _) ->
    GLeft
      {}
  | (MulfEOOExprOp _, PrimEOOExprOp _) ->
    GRight
      {}
  | (MulfEOOExprOp _, EqnEOOExprOp _) ->
    GLeft
      {}
  | (EqnEOOExprOp _, MulfEOOExprOp _) ->
    GRight
      {}
  | (InitEOOExprOp _, MulfEOOExprOp _) ->
    GRight
      {}
  | (ModelEOOExprOp _, MulfEOOExprOp _) ->
    GRight
      {}
  | (DivfEOOExprOp _, PrimEOOExprOp _) ->
    GRight
      {}
  | (DivfEOOExprOp _, EqnEOOExprOp _) ->
    GLeft
      {}
  | (EqnEOOExprOp _, DivfEOOExprOp _) ->
    GRight
      {}
  | (InitEOOExprOp _, DivfEOOExprOp _) ->
    GRight
      {}
  | (ModelEOOExprOp _, DivfEOOExprOp _) ->
    GRight
      {}
  | (EqnEOOExprOp _, PrimEOOExprOp _) ->
    GRight
      {}
  | (InitEOOExprOp _, PrimEOOExprOp _) ->
    GRight
      {}
  | (ModelEOOExprOp _, PrimEOOExprOp _) ->
    GRight
      {}
  | (InitEOOExprOp _, EqnEOOExprOp _) ->
    GRight
      {}
  | (ModelEOOExprOp _, EqnEOOExprOp _) ->
    GRight
      {}
  sem groupingsAllowed_EOOPatOp =
  | (TupEOOPatOp _, ConsEOOPatOp _) ->
    GRight
      {}
  | (ConsEOOPatOp _, TupEOOPatOp _) ->
    GLeft
      {}
  sem groupingsAllowed_EOOTypeOp =
  | (AppEOOTypeOp _, TupEOOTypeOp _) ->
    GLeft
      {}
  | (TupEOOTypeOp _, AppEOOTypeOp _) ->
    GRight
      {}
  | (AppEOOTypeOp _, ArrowEOOTypeOp _) ->
    GLeft
      {}
  | (ArrowEOOTypeOp _, AppEOOTypeOp _) ->
    GRight
      {}
  | (ForallEOOTypeOp _, AppEOOTypeOp _) ->
    GLeft
      {}
  | (TupEOOTypeOp _, ArrowEOOTypeOp _) ->
    GLeft
      {}
  | (ArrowEOOTypeOp _, TupEOOTypeOp _) ->
    GRight
      {}
  | (ForallEOOTypeOp _, TupEOOTypeOp _) ->
    GLeft
      {}
  | (ForallEOOTypeOp _, ArrowEOOTypeOp _) ->
    GLeft
      {}
  
end
let _table =
  use ParseEOO
  in
  let target =
    genParsingTable
      (let #var"EOOProg" =
         nameSym
           "EOOProg"
       in
       let #var"EOOTop" =
         nameSym
           "EOOTop"
       in
       let #var"EOOParam" =
         nameSym
           "EOOParam"
       in
       let #var"EOODefBinding" =
         nameSym
           "EOODefBinding"
       in
       let #var"EOOModelBinding" =
         nameSym
           "EOOModelBinding"
       in
       let #var"EOOExpr" =
         nameSym
           "EOOExpr"
       in
       let #var"EOOPat" =
         nameSym
           "EOOPat"
       in
       let #var"EOOType" =
         nameSym
           "EOOType"
       in
       let #var"EOOModelHead" =
         nameSym
           "EOOModelHead"
       in
       let #var"EOOProgPostfix" =
         nameSym
           "EOOProgPostfix"
       in
       let #var"EOOProgPrefix" =
         nameSym
           "EOOProgPrefix"
       in
       let #var"EOOProgInfix" =
         nameSym
           "EOOProgInfix"
       in
       let #var"EOOProgAtom" =
         nameSym
           "EOOProgAtom"
       in
       let #var"EOOTopPostfix" =
         nameSym
           "EOOTopPostfix"
       in
       let #var"EOOTopPrefix" =
         nameSym
           "EOOTopPrefix"
       in
       let #var"EOOTopInfix" =
         nameSym
           "EOOTopInfix"
       in
       let #var"EOOTopAtom" =
         nameSym
           "EOOTopAtom"
       in
       let #var"EOOParamPostfix" =
         nameSym
           "EOOParamPostfix"
       in
       let #var"EOOParamPrefix" =
         nameSym
           "EOOParamPrefix"
       in
       let #var"EOOParamInfix" =
         nameSym
           "EOOParamInfix"
       in
       let #var"EOOParamAtom" =
         nameSym
           "EOOParamAtom"
       in
       let #var"EOODefBindingPostfix" =
         nameSym
           "EOODefBindingPostfix"
       in
       let #var"EOODefBindingPrefix" =
         nameSym
           "EOODefBindingPrefix"
       in
       let #var"EOODefBindingInfix" =
         nameSym
           "EOODefBindingInfix"
       in
       let #var"EOODefBindingAtom" =
         nameSym
           "EOODefBindingAtom"
       in
       let #var"EOOModelBindingPostfix" =
         nameSym
           "EOOModelBindingPostfix"
       in
       let #var"EOOModelBindingPrefix" =
         nameSym
           "EOOModelBindingPrefix"
       in
       let #var"EOOModelBindingInfix" =
         nameSym
           "EOOModelBindingInfix"
       in
       let #var"EOOModelBindingAtom" =
         nameSym
           "EOOModelBindingAtom"
       in
       let #var"EOOExprPostfix" =
         nameSym
           "EOOExprPostfix"
       in
       let #var"EOOExprPrefix" =
         nameSym
           "EOOExprPrefix"
       in
       let #var"EOOExprInfix" =
         nameSym
           "EOOExprInfix"
       in
       let #var"EOOExprAtom" =
         nameSym
           "EOOExprAtom"
       in
       let #var"EOOPatPostfix" =
         nameSym
           "EOOPatPostfix"
       in
       let #var"EOOPatPrefix" =
         nameSym
           "EOOPatPrefix"
       in
       let #var"EOOPatInfix" =
         nameSym
           "EOOPatInfix"
       in
       let #var"EOOPatAtom" =
         nameSym
           "EOOPatAtom"
       in
       let #var"EOOTypePostfix" =
         nameSym
           "EOOTypePostfix"
       in
       let #var"EOOTypePrefix" =
         nameSym
           "EOOTypePrefix"
       in
       let #var"EOOTypeInfix" =
         nameSym
           "EOOTypeInfix"
       in
       let #var"EOOTypeAtom" =
         nameSym
           "EOOTypeAtom"
       in
       let #var"EOOModelHeadPostfix" =
         nameSym
           "EOOModelHeadPostfix"
       in
       let #var"EOOModelHeadPrefix" =
         nameSym
           "EOOModelHeadPrefix"
       in
       let #var"EOOModelHeadInfix" =
         nameSym
           "EOOModelHeadInfix"
       in
       let #var"EOOModelHeadAtom" =
         nameSym
           "EOOModelHeadAtom"
       in
       let kleene =
         nameSym
           "kleene"
       in
       let kleene1 =
         nameSym
           "kleene"
       in
       let kleene2 =
         nameSym
           "kleene"
       in
       let alt =
         nameSym
           "alt"
       in
       let alt1 =
         nameSym
           "alt"
       in
       let alt2 =
         nameSym
           "alt"
       in
       let alt3 =
         nameSym
           "alt"
       in
       let kleene3 =
         nameSym
           "kleene"
       in
       let alt4 =
         nameSym
           "alt"
       in
       let alt5 =
         nameSym
           "alt"
       in
       let alt6 =
         nameSym
           "alt"
       in
       let kleene4 =
         nameSym
           "kleene"
       in
       let kleene5 =
         nameSym
           "kleene"
       in
       let alt7 =
         nameSym
           "alt"
       in
       let alt8 =
         nameSym
           "alt"
       in
       let alt9 =
         nameSym
           "alt"
       in
       let alt10 =
         nameSym
           "alt"
       in
       let alt11 =
         nameSym
           "alt"
       in
       let alt12 =
         nameSym
           "alt"
       in
       let kleene6 =
         nameSym
           "kleene"
       in
       let kleene7 =
         nameSym
           "kleene"
       in
       let alt13 =
         nameSym
           "alt"
       in
       let alt14 =
         nameSym
           "alt"
       in
       let kleene8 =
         nameSym
           "kleene"
       in
       let alt15 =
         nameSym
           "alt"
       in
       let alt16 =
         nameSym
           "alt"
       in
       let kleene9 =
         nameSym
           "kleene"
       in
       let alt17 =
         nameSym
           "alt"
       in
       let kleene10 =
         nameSym
           "kleene"
       in
       let #var"EOOProg_lclosed" =
         nameSym
           "EOOProg_lclosed"
       in
       let #var"EOOProg_lopen" =
         nameSym
           "EOOProg_lopen"
       in
       let #var"EOOTop_lclosed" =
         nameSym
           "EOOTop_lclosed"
       in
       let #var"EOOTop_lopen" =
         nameSym
           "EOOTop_lopen"
       in
       let #var"EOOParam_lclosed" =
         nameSym
           "EOOParam_lclosed"
       in
       let #var"EOOParam_lopen" =
         nameSym
           "EOOParam_lopen"
       in
       let #var"EOODefBinding_lclosed" =
         nameSym
           "EOODefBinding_lclosed"
       in
       let #var"EOODefBinding_lopen" =
         nameSym
           "EOODefBinding_lopen"
       in
       let #var"EOOModelBinding_lclosed" =
         nameSym
           "EOOModelBinding_lclosed"
       in
       let #var"EOOModelBinding_lopen" =
         nameSym
           "EOOModelBinding_lopen"
       in
       let #var"EOOExpr_lclosed" =
         nameSym
           "EOOExpr_lclosed"
       in
       let #var"EOOExpr_lopen" =
         nameSym
           "EOOExpr_lopen"
       in
       let #var"EOOPat_lclosed" =
         nameSym
           "EOOPat_lclosed"
       in
       let #var"EOOPat_lopen" =
         nameSym
           "EOOPat_lopen"
       in
       let #var"EOOType_lclosed" =
         nameSym
           "EOOType_lclosed"
       in
       let #var"EOOType_lopen" =
         nameSym
           "EOOType_lopen"
       in
       let #var"EOOModelHead_lclosed" =
         nameSym
           "EOOModelHead_lclosed"
       in
       let #var"EOOModelHead_lopen" =
         nameSym
           "EOOModelHead_lopen"
       in
       { start =
           #var"EOOProg",
         productions =
           let config =
             { parenAllowed =
                 #frozen"parenAllowed_EOOProgOp",
               topAllowed =
                 #frozen"topAllowed_EOOProgOp",
               leftAllowed =
                 #frozen"leftAllowed_EOOProgOp",
               rightAllowed =
                 #frozen"rightAllowed_EOOProgOp",
               groupingsAllowed =
                 #frozen"groupingsAllowed_EOOProgOp" }
           in
           let reportConfig =
             { parenAllowed =
                 #frozen"parenAllowed_EOOProgOp",
               topAllowed =
                 #frozen"topAllowed_EOOProgOp",
               terminalInfos =
                 #frozen"getTerms_EOOProgOp",
               getInfo =
                 #frozen"getInfo_EOOProgOp",
               lpar =
                 "(",
               rpar =
                 ")" }
           in
           let addEOOProgOpAtom =
             lam #var"".
               lam x35.
                 lam st.
                   optionMap
                     (breakableAddAtom
                        config
                        x35)
                     st
           in
           let addEOOProgOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x35.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddInfix
                         config
                         x35
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_EOOProgOp
                                 x35, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addEOOProgOpPrefix =
             lam #var"".
               lam x35.
                 lam st.
                   optionMap
                     (breakableAddPrefix
                        config
                        x35)
                     st
           in
           let addEOOProgOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x35.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddPostfix
                         config
                         x35
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_EOOProgOp
                                 x35, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeEOOProgOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res109 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse
                            config
                            st
                        with
                          Some (tops & ([ top ] ++ _ ++ ""))
                        then
                          let errs =
                            breakableDefaultHighlight
                              reportConfig
                              p.content
                              tops
                          in
                          let res109 =
                            unsplit_EOOProgOp
                              top
                          in
                          match
                            null
                              errs
                          with
                            true
                          then
                            Some
                              res109
                          else
                            (modref
                                 p.errors
                                 (concat
                                    (deref
                                       p.errors)
                                    errs))
                            ; Some
                              (res109.0, BadEOOProg
                                { info =
                                    res109.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref
                                     p.errors)
                                  (NoInfo
                                    {}, "Unfinished EOOProg")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadEOOProg
                     { info =
                         NoInfo
                           {} })
                   res109
           in
           let config1 =
             { parenAllowed =
                 #frozen"parenAllowed_EOOTopOp",
               topAllowed =
                 #frozen"topAllowed_EOOTopOp",
               leftAllowed =
                 #frozen"leftAllowed_EOOTopOp",
               rightAllowed =
                 #frozen"rightAllowed_EOOTopOp",
               groupingsAllowed =
                 #frozen"groupingsAllowed_EOOTopOp" }
           in
           let reportConfig1 =
             { parenAllowed =
                 #frozen"parenAllowed_EOOTopOp",
               topAllowed =
                 #frozen"topAllowed_EOOTopOp",
               terminalInfos =
                 #frozen"getTerms_EOOTopOp",
               getInfo =
                 #frozen"getInfo_EOOTopOp",
               lpar =
                 "(",
               rpar =
                 ")" }
           in
           let addEOOTopOpAtom =
             lam #var"".
               lam x35.
                 lam st.
                   optionMap
                     (breakableAddAtom
                        config1
                        x35)
                     st
           in
           let addEOOTopOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x35.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddInfix
                         config1
                         x35
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_EOOTopOp
                                 x35, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addEOOTopOpPrefix =
             lam #var"".
               lam x35.
                 lam st.
                   optionMap
                     (breakableAddPrefix
                        config1
                        x35)
                     st
           in
           let addEOOTopOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x35.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddPostfix
                         config1
                         x35
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_EOOTopOp
                                 x35, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeEOOTopOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res109 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse
                            config1
                            st
                        with
                          Some (tops & ([ top ] ++ _ ++ ""))
                        then
                          let errs =
                            breakableDefaultHighlight
                              reportConfig1
                              p.content
                              tops
                          in
                          let res109 =
                            unsplit_EOOTopOp
                              top
                          in
                          match
                            null
                              errs
                          with
                            true
                          then
                            Some
                              res109
                          else
                            (modref
                                 p.errors
                                 (concat
                                    (deref
                                       p.errors)
                                    errs))
                            ; Some
                              (res109.0, BadEOOTop
                                { info =
                                    res109.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref
                                     p.errors)
                                  (NoInfo
                                    {}, "Unfinished EOOTop")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadEOOTop
                     { info =
                         NoInfo
                           {} })
                   res109
           in
           let config2 =
             { parenAllowed =
                 #frozen"parenAllowed_EOOParamOp",
               topAllowed =
                 #frozen"topAllowed_EOOParamOp",
               leftAllowed =
                 #frozen"leftAllowed_EOOParamOp",
               rightAllowed =
                 #frozen"rightAllowed_EOOParamOp",
               groupingsAllowed =
                 #frozen"groupingsAllowed_EOOParamOp" }
           in
           let reportConfig2 =
             { parenAllowed =
                 #frozen"parenAllowed_EOOParamOp",
               topAllowed =
                 #frozen"topAllowed_EOOParamOp",
               terminalInfos =
                 #frozen"getTerms_EOOParamOp",
               getInfo =
                 #frozen"getInfo_EOOParamOp",
               lpar =
                 "(",
               rpar =
                 ")" }
           in
           let addEOOParamOpAtom =
             lam #var"".
               lam x35.
                 lam st.
                   optionMap
                     (breakableAddAtom
                        config2
                        x35)
                     st
           in
           let addEOOParamOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x35.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddInfix
                         config2
                         x35
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_EOOParamOp
                                 x35, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addEOOParamOpPrefix =
             lam #var"".
               lam x35.
                 lam st.
                   optionMap
                     (breakableAddPrefix
                        config2
                        x35)
                     st
           in
           let addEOOParamOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x35.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddPostfix
                         config2
                         x35
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_EOOParamOp
                                 x35, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeEOOParamOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res109 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse
                            config2
                            st
                        with
                          Some (tops & ([ top ] ++ _ ++ ""))
                        then
                          let errs =
                            breakableDefaultHighlight
                              reportConfig2
                              p.content
                              tops
                          in
                          let res109 =
                            unsplit_EOOParamOp
                              top
                          in
                          match
                            null
                              errs
                          with
                            true
                          then
                            Some
                              res109
                          else
                            (modref
                                 p.errors
                                 (concat
                                    (deref
                                       p.errors)
                                    errs))
                            ; Some
                              (res109.0, BadEOOParam
                                { info =
                                    res109.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref
                                     p.errors)
                                  (NoInfo
                                    {}, "Unfinished EOOParam")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadEOOParam
                     { info =
                         NoInfo
                           {} })
                   res109
           in
           let config3 =
             { parenAllowed =
                 #frozen"parenAllowed_EOODefBindingOp",
               topAllowed =
                 #frozen"topAllowed_EOODefBindingOp",
               leftAllowed =
                 #frozen"leftAllowed_EOODefBindingOp",
               rightAllowed =
                 #frozen"rightAllowed_EOODefBindingOp",
               groupingsAllowed =
                 #frozen"groupingsAllowed_EOODefBindingOp" }
           in
           let reportConfig3 =
             { parenAllowed =
                 #frozen"parenAllowed_EOODefBindingOp",
               topAllowed =
                 #frozen"topAllowed_EOODefBindingOp",
               terminalInfos =
                 #frozen"getTerms_EOODefBindingOp",
               getInfo =
                 #frozen"getInfo_EOODefBindingOp",
               lpar =
                 "(",
               rpar =
                 ")" }
           in
           let addEOODefBindingOpAtom =
             lam #var"".
               lam x35.
                 lam st.
                   optionMap
                     (breakableAddAtom
                        config3
                        x35)
                     st
           in
           let addEOODefBindingOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x35.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddInfix
                         config3
                         x35
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_EOODefBindingOp
                                 x35, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addEOODefBindingOpPrefix =
             lam #var"".
               lam x35.
                 lam st.
                   optionMap
                     (breakableAddPrefix
                        config3
                        x35)
                     st
           in
           let addEOODefBindingOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x35.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddPostfix
                         config3
                         x35
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_EOODefBindingOp
                                 x35, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeEOODefBindingOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res109 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse
                            config3
                            st
                        with
                          Some (tops & ([ top ] ++ _ ++ ""))
                        then
                          let errs =
                            breakableDefaultHighlight
                              reportConfig3
                              p.content
                              tops
                          in
                          let res109 =
                            unsplit_EOODefBindingOp
                              top
                          in
                          match
                            null
                              errs
                          with
                            true
                          then
                            Some
                              res109
                          else
                            (modref
                                 p.errors
                                 (concat
                                    (deref
                                       p.errors)
                                    errs))
                            ; Some
                              (res109.0, BadEOODefBinding
                                { info =
                                    res109.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref
                                     p.errors)
                                  (NoInfo
                                    {}, "Unfinished EOODefBinding")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadEOODefBinding
                     { info =
                         NoInfo
                           {} })
                   res109
           in
           let config4 =
             { parenAllowed =
                 #frozen"parenAllowed_EOOModelBindingOp",
               topAllowed =
                 #frozen"topAllowed_EOOModelBindingOp",
               leftAllowed =
                 #frozen"leftAllowed_EOOModelBindingOp",
               rightAllowed =
                 #frozen"rightAllowed_EOOModelBindingOp",
               groupingsAllowed =
                 #frozen"groupingsAllowed_EOOModelBindingOp" }
           in
           let reportConfig4 =
             { parenAllowed =
                 #frozen"parenAllowed_EOOModelBindingOp",
               topAllowed =
                 #frozen"topAllowed_EOOModelBindingOp",
               terminalInfos =
                 #frozen"getTerms_EOOModelBindingOp",
               getInfo =
                 #frozen"getInfo_EOOModelBindingOp",
               lpar =
                 "(",
               rpar =
                 ")" }
           in
           let addEOOModelBindingOpAtom =
             lam #var"".
               lam x35.
                 lam st.
                   optionMap
                     (breakableAddAtom
                        config4
                        x35)
                     st
           in
           let addEOOModelBindingOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x35.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddInfix
                         config4
                         x35
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_EOOModelBindingOp
                                 x35, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addEOOModelBindingOpPrefix =
             lam #var"".
               lam x35.
                 lam st.
                   optionMap
                     (breakableAddPrefix
                        config4
                        x35)
                     st
           in
           let addEOOModelBindingOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x35.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddPostfix
                         config4
                         x35
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_EOOModelBindingOp
                                 x35, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeEOOModelBindingOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res109 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse
                            config4
                            st
                        with
                          Some (tops & ([ top ] ++ _ ++ ""))
                        then
                          let errs =
                            breakableDefaultHighlight
                              reportConfig4
                              p.content
                              tops
                          in
                          let res109 =
                            unsplit_EOOModelBindingOp
                              top
                          in
                          match
                            null
                              errs
                          with
                            true
                          then
                            Some
                              res109
                          else
                            (modref
                                 p.errors
                                 (concat
                                    (deref
                                       p.errors)
                                    errs))
                            ; Some
                              (res109.0, BadEOOModelBinding
                                { info =
                                    res109.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref
                                     p.errors)
                                  (NoInfo
                                    {}, "Unfinished EOOModelBinding")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadEOOModelBinding
                     { info =
                         NoInfo
                           {} })
                   res109
           in
           let config5 =
             { parenAllowed =
                 #frozen"parenAllowed_EOOExprOp",
               topAllowed =
                 #frozen"topAllowed_EOOExprOp",
               leftAllowed =
                 #frozen"leftAllowed_EOOExprOp",
               rightAllowed =
                 #frozen"rightAllowed_EOOExprOp",
               groupingsAllowed =
                 #frozen"groupingsAllowed_EOOExprOp" }
           in
           let reportConfig5 =
             { parenAllowed =
                 #frozen"parenAllowed_EOOExprOp",
               topAllowed =
                 #frozen"topAllowed_EOOExprOp",
               terminalInfos =
                 #frozen"getTerms_EOOExprOp",
               getInfo =
                 #frozen"getInfo_EOOExprOp",
               lpar =
                 "(",
               rpar =
                 ")" }
           in
           let addEOOExprOpAtom =
             lam #var"".
               lam x35.
                 lam st.
                   optionMap
                     (breakableAddAtom
                        config5
                        x35)
                     st
           in
           let addEOOExprOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x35.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddInfix
                         config5
                         x35
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_EOOExprOp
                                 x35, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addEOOExprOpPrefix =
             lam #var"".
               lam x35.
                 lam st.
                   optionMap
                     (breakableAddPrefix
                        config5
                        x35)
                     st
           in
           let addEOOExprOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x35.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddPostfix
                         config5
                         x35
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_EOOExprOp
                                 x35, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeEOOExprOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res109 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse
                            config5
                            st
                        with
                          Some (tops & ([ top ] ++ _ ++ ""))
                        then
                          let errs =
                            breakableDefaultHighlight
                              reportConfig5
                              p.content
                              tops
                          in
                          let res109 =
                            unsplit_EOOExprOp
                              top
                          in
                          match
                            null
                              errs
                          with
                            true
                          then
                            Some
                              res109
                          else
                            (modref
                                 p.errors
                                 (concat
                                    (deref
                                       p.errors)
                                    errs))
                            ; Some
                              (res109.0, BadEOOExpr
                                { info =
                                    res109.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref
                                     p.errors)
                                  (NoInfo
                                    {}, "Unfinished EOOExpr")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadEOOExpr
                     { info =
                         NoInfo
                           {} })
                   res109
           in
           let config6 =
             { parenAllowed =
                 #frozen"parenAllowed_EOOPatOp",
               topAllowed =
                 #frozen"topAllowed_EOOPatOp",
               leftAllowed =
                 #frozen"leftAllowed_EOOPatOp",
               rightAllowed =
                 #frozen"rightAllowed_EOOPatOp",
               groupingsAllowed =
                 #frozen"groupingsAllowed_EOOPatOp" }
           in
           let reportConfig6 =
             { parenAllowed =
                 #frozen"parenAllowed_EOOPatOp",
               topAllowed =
                 #frozen"topAllowed_EOOPatOp",
               terminalInfos =
                 #frozen"getTerms_EOOPatOp",
               getInfo =
                 #frozen"getInfo_EOOPatOp",
               lpar =
                 "(",
               rpar =
                 ")" }
           in
           let addEOOPatOpAtom =
             lam #var"".
               lam x35.
                 lam st.
                   optionMap
                     (breakableAddAtom
                        config6
                        x35)
                     st
           in
           let addEOOPatOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x35.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddInfix
                         config6
                         x35
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_EOOPatOp
                                 x35, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addEOOPatOpPrefix =
             lam #var"".
               lam x35.
                 lam st.
                   optionMap
                     (breakableAddPrefix
                        config6
                        x35)
                     st
           in
           let addEOOPatOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x35.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddPostfix
                         config6
                         x35
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_EOOPatOp
                                 x35, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeEOOPatOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res109 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse
                            config6
                            st
                        with
                          Some (tops & ([ top ] ++ _ ++ ""))
                        then
                          let errs =
                            breakableDefaultHighlight
                              reportConfig6
                              p.content
                              tops
                          in
                          let res109 =
                            unsplit_EOOPatOp
                              top
                          in
                          match
                            null
                              errs
                          with
                            true
                          then
                            Some
                              res109
                          else
                            (modref
                                 p.errors
                                 (concat
                                    (deref
                                       p.errors)
                                    errs))
                            ; Some
                              (res109.0, BadEOOPat
                                { info =
                                    res109.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref
                                     p.errors)
                                  (NoInfo
                                    {}, "Unfinished EOOPat")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadEOOPat
                     { info =
                         NoInfo
                           {} })
                   res109
           in
           let config7 =
             { parenAllowed =
                 #frozen"parenAllowed_EOOTypeOp",
               topAllowed =
                 #frozen"topAllowed_EOOTypeOp",
               leftAllowed =
                 #frozen"leftAllowed_EOOTypeOp",
               rightAllowed =
                 #frozen"rightAllowed_EOOTypeOp",
               groupingsAllowed =
                 #frozen"groupingsAllowed_EOOTypeOp" }
           in
           let reportConfig7 =
             { parenAllowed =
                 #frozen"parenAllowed_EOOTypeOp",
               topAllowed =
                 #frozen"topAllowed_EOOTypeOp",
               terminalInfos =
                 #frozen"getTerms_EOOTypeOp",
               getInfo =
                 #frozen"getInfo_EOOTypeOp",
               lpar =
                 "(",
               rpar =
                 ")" }
           in
           let addEOOTypeOpAtom =
             lam #var"".
               lam x35.
                 lam st.
                   optionMap
                     (breakableAddAtom
                        config7
                        x35)
                     st
           in
           let addEOOTypeOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x35.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddInfix
                         config7
                         x35
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_EOOTypeOp
                                 x35, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addEOOTypeOpPrefix =
             lam #var"".
               lam x35.
                 lam st.
                   optionMap
                     (breakableAddPrefix
                        config7
                        x35)
                     st
           in
           let addEOOTypeOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x35.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddPostfix
                         config7
                         x35
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_EOOTypeOp
                                 x35, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeEOOTypeOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res109 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse
                            config7
                            st
                        with
                          Some (tops & ([ top ] ++ _ ++ ""))
                        then
                          let errs =
                            breakableDefaultHighlight
                              reportConfig7
                              p.content
                              tops
                          in
                          let res109 =
                            unsplit_EOOTypeOp
                              top
                          in
                          match
                            null
                              errs
                          with
                            true
                          then
                            Some
                              res109
                          else
                            (modref
                                 p.errors
                                 (concat
                                    (deref
                                       p.errors)
                                    errs))
                            ; Some
                              (res109.0, BadEOOType
                                { info =
                                    res109.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref
                                     p.errors)
                                  (NoInfo
                                    {}, "Unfinished EOOType")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadEOOType
                     { info =
                         NoInfo
                           {} })
                   res109
           in
           let config8 =
             { parenAllowed =
                 #frozen"parenAllowed_EOOModelHeadOp",
               topAllowed =
                 #frozen"topAllowed_EOOModelHeadOp",
               leftAllowed =
                 #frozen"leftAllowed_EOOModelHeadOp",
               rightAllowed =
                 #frozen"rightAllowed_EOOModelHeadOp",
               groupingsAllowed =
                 #frozen"groupingsAllowed_EOOModelHeadOp" }
           in
           let reportConfig8 =
             { parenAllowed =
                 #frozen"parenAllowed_EOOModelHeadOp",
               topAllowed =
                 #frozen"topAllowed_EOOModelHeadOp",
               terminalInfos =
                 #frozen"getTerms_EOOModelHeadOp",
               getInfo =
                 #frozen"getInfo_EOOModelHeadOp",
               lpar =
                 "(",
               rpar =
                 ")" }
           in
           let addEOOModelHeadOpAtom =
             lam #var"".
               lam x35.
                 lam st.
                   optionMap
                     (breakableAddAtom
                        config8
                        x35)
                     st
           in
           let addEOOModelHeadOpInfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x35.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddInfix
                         config8
                         x35
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_EOOModelHeadOp
                                 x35, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let addEOOModelHeadOpPrefix =
             lam #var"".
               lam x35.
                 lam st.
                   optionMap
                     (breakableAddPrefix
                        config8
                        x35)
                     st
           in
           let addEOOModelHeadOpPostfix =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam x35.
                 lam st.
                   match
                     st
                   with
                     Some st
                   then
                     let st =
                       breakableAddPostfix
                         config8
                         x35
                         st
                     in
                     (match
                          st
                        with
                          None _
                        then
                          modref
                            p.errors
                            (snoc
                               (deref
                                  p.errors)
                               (getInfo_EOOModelHeadOp
                                 x35, "Invalid input"))
                        else
                          {})
                     ; st
                   else
                     None
                       {}
           in
           let finalizeEOOModelHeadOp =
             lam p: {errors: Ref [(Info, [Char])], content: String}.
               lam st.
                 let res109 =
                   optionBind
                     st
                     (lam st.
                        match
                          breakableFinalizeParse
                            config8
                            st
                        with
                          Some (tops & ([ top ] ++ _ ++ ""))
                        then
                          let errs =
                            breakableDefaultHighlight
                              reportConfig8
                              p.content
                              tops
                          in
                          let res109 =
                            unsplit_EOOModelHeadOp
                              top
                          in
                          match
                            null
                              errs
                          with
                            true
                          then
                            Some
                              res109
                          else
                            (modref
                                 p.errors
                                 (concat
                                    (deref
                                       p.errors)
                                    errs))
                            ; Some
                              (res109.0, BadEOOModelHead
                                { info =
                                    res109.0 })
                        else
                          (modref
                               p.errors
                               (snoc
                                  (deref
                                     p.errors)
                                  (NoInfo
                                    {}, "Unfinished EOOModelHead")))
                          ; None
                            {})
                 in
                 optionGetOr
                   (NoInfo
                     {}, BadEOOModelHead
                     { info =
                         NoInfo
                           {} })
                   res109
           in
           [ { nt =
                 kleene,
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOTop",
                   ntSym
                     kleene ],
               action =
                 lam state: {errors: Ref [(Info, [Char])], content: String}.
                   lam res.
                     match
                       res
                     with
                       [ UserSym ntVal,
                         UserSym val1 ]
                     in
                     let ntVal: (Info, EOOTop) =
                         fromDyn
                           ntVal
                       in
                       let val1: {tops: [EOOTop], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val1
                       in
                       asDyn
                         { __br_info =
                             mergeInfo
                               ntVal.0
                               val1.__br_info,
                           __br_terms =
                             val1.__br_terms,
                           tops =
                             concat
                               [ ntVal.1 ]
                               val1.tops } },
             { nt =
                 kleene,
               label =
                 {},
               rhs =
                 "",
               action =
                 lam state1: {errors: Ref [(Info, [Char])], content: String}.
                   lam res1.
                     match
                       res1
                     with
                       ""
                     in
                     asDyn
                         { __br_info =
                             NoInfo
                               {},
                           __br_terms =
                             "",
                           tops =
                             "" } },
             { nt =
                 kleene1,
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOModelHead",
                   ntSym
                     kleene1 ],
               action =
                 lam state2: {errors: Ref [(Info, [Char])], content: String}.
                   lam res2.
                     match
                       res2
                     with
                       [ UserSym ntVal1,
                         UserSym val2 ]
                     in
                     let ntVal1: (Info, EOOModelHead) =
                         fromDyn
                           ntVal1
                       in
                       let val2: {heads: [EOOModelHead], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val2
                       in
                       asDyn
                         { __br_info =
                             mergeInfo
                               ntVal1.0
                               val2.__br_info,
                           __br_terms =
                             val2.__br_terms,
                           heads =
                             concat
                               [ ntVal1.1 ]
                               val2.heads } },
             { nt =
                 kleene1,
               label =
                 {},
               rhs =
                 "",
               action =
                 lam state3: {errors: Ref [(Info, [Char])], content: String}.
                   lam res3.
                     match
                       res3
                     with
                       ""
                     in
                     asDyn
                         { __br_info =
                             NoInfo
                               {},
                           __br_terms =
                             "",
                           heads =
                             "" } },
             { nt =
                 #var"EOOProgAtom",
               label =
                 {},
               rhs =
                 [ ntSym
                     kleene,
                   litSym
                     "main",
                   litSym
                     "model",
                   ntSym
                     kleene1,
                   litSym
                     "equation",
                   ntSym
                     #var"EOOExpr",
                   litSym
                     "output",
                   ntSym
                     #var"EOOExpr",
                   litSym
                     "end" ],
               action =
                 lam state4: {errors: Ref [(Info, [Char])], content: String}.
                   lam res4.
                     match
                       res4
                     with
                       [ UserSym val1,
                         LitParsed l,
                         LitParsed l1,
                         UserSym val2,
                         LitParsed l2,
                         UserSym ntVal2,
                         LitParsed l3,
                         UserSym ntVal3,
                         LitParsed l4 ]
                     in
                     let val1: {tops: [EOOTop], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val1
                       in
                       let val2: {heads: [EOOModelHead], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val2
                       in
                       let ntVal2: (Info, EOOExpr) =
                         fromDyn
                           ntVal2
                       in
                       let ntVal3: (Info, EOOExpr) =
                         fromDyn
                           ntVal3
                       in
                       asDyn
                         (ProgEOOProgOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  val1.__br_info
                                  [ l.info,
                                    l1.info,
                                    val2.__br_info,
                                    l2.info,
                                    ntVal2.0,
                                    l3.info,
                                    ntVal3.0,
                                    l4.info ],
                              __br_terms =
                                join
                                  [ val1.__br_terms,
                                    [ l.info ],
                                    [ l1.info ],
                                    val2.__br_terms,
                                    [ l2.info ],
                                    [ l3.info ],
                                    [ l4.info ] ],
                              tops =
                                val1.tops,
                              heads =
                                val2.heads,
                              eqn =
                                [ ntVal2.1 ],
                              eqnkw =
                                [ l2.info ],
                              output =
                                [ ntVal3.1 ] }) },
             { nt =
                 #var"EOOTopAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "def",
                   ntSym
                     #var"EOODefBinding",
                   litSym
                     "end" ],
               action =
                 lam state5: {errors: Ref [(Info, [Char])], content: String}.
                   lam res5.
                     match
                       res5
                     with
                       [ LitParsed l5,
                         UserSym ntVal4,
                         LitParsed l6 ]
                     in
                     let ntVal4: (Info, EOODefBinding) =
                         fromDyn
                           ntVal4
                       in
                       asDyn
                         (DefEOOTopOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l5.info
                                  [ ntVal4.0,
                                    l6.info ],
                              __br_terms =
                                concat
                                  [ l5.info ]
                                  [ l6.info ],
                              binding =
                                [ ntVal4.1 ] }) },
             { nt =
                 #var"EOOTopAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "model",
                   ntSym
                     #var"EOOModelBinding",
                   litSym
                     "end" ],
               action =
                 lam state6: {errors: Ref [(Info, [Char])], content: String}.
                   lam res6.
                     match
                       res6
                     with
                       [ LitParsed l7,
                         UserSym ntVal5,
                         LitParsed l8 ]
                     in
                     let ntVal5: (Info, EOOModelBinding) =
                         fromDyn
                           ntVal5
                       in
                       asDyn
                         (ModelEOOTopOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l7.info
                                  [ ntVal5.0,
                                    l8.info ],
                              __br_terms =
                                concat
                                  [ l7.info ]
                                  [ l8.info ],
                              binding =
                                [ ntVal5.1 ] }) },
             { nt =
                 kleene2,
               label =
                 {},
               rhs =
                 [ tokSym
                     (LIdentRepr
                        {}),
                   ntSym
                     kleene2 ],
               action =
                 lam state7: {errors: Ref [(Info, [Char])], content: String}.
                   lam res7.
                     match
                       res7
                     with
                       [ TokParsed (LIdentTok x),
                         UserSym val3 ]
                     in
                     let val3: {params: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val3
                       in
                       asDyn
                         { __br_info =
                             mergeInfo
                               x.info
                               val3.__br_info,
                           __br_terms =
                             concat
                               [ x.info ]
                               val3.__br_terms,
                           params =
                             concat
                               [ { v =
                                     nameNoSym
                                       x.val,
                                   i =
                                     x.info } ]
                               val3.params } },
             { nt =
                 kleene2,
               label =
                 {},
               rhs =
                 "",
               action =
                 lam state8: {errors: Ref [(Info, [Char])], content: String}.
                   lam res8.
                     match
                       res8
                     with
                       ""
                     in
                     asDyn
                         { __br_info =
                             NoInfo
                               {},
                           __br_terms =
                             "",
                           params =
                             "" } },
             { nt =
                 #var"EOOTopAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "type",
                   tokSym
                     (UIdentRepr
                        {}),
                   ntSym
                     kleene2,
                   litSym
                     "=",
                   ntSym
                     #var"EOOType" ],
               action =
                 lam state9: {errors: Ref [(Info, [Char])], content: String}.
                   lam res9.
                     match
                       res9
                     with
                       [ LitParsed l9,
                         TokParsed (UIdentTok x1),
                         UserSym val3,
                         LitParsed l10,
                         UserSym ntVal6 ]
                     in
                     let val3: {params: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val3
                       in
                       let ntVal6: (Info, EOOType) =
                         fromDyn
                           ntVal6
                       in
                       asDyn
                         (TypeEOOTopOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l9.info
                                  [ x1.info,
                                    val3.__br_info,
                                    l10.info,
                                    ntVal6.0 ],
                              __br_terms =
                                join
                                  [ [ l9.info ],
                                    [ x1.info ],
                                    val3.__br_terms,
                                    [ l10.info ] ],
                              rhs =
                                [ ntVal6.1 ],
                              params =
                                val3.params,
                              n =
                                [ { v =
                                      nameNoSym
                                        x1.val,
                                    i =
                                      x1.info } ] }) },
             { nt =
                 #var"EOOParamAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "(",
                   ntSym
                     #var"EOOPat",
                   litSym
                     ")" ],
               action =
                 lam state10: {errors: Ref [(Info, [Char])], content: String}.
                   lam res10.
                     match
                       res10
                     with
                       [ LitParsed l11,
                         UserSym ntVal7,
                         LitParsed l12 ]
                     in
                     let ntVal7: (Info, EOOPat) =
                         fromDyn
                           ntVal7
                       in
                       asDyn
                         (PatEOOParamOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l11.info
                                  [ ntVal7.0,
                                    l12.info ],
                              __br_terms =
                                concat
                                  [ l11.info ]
                                  [ l12.info ],
                              pat =
                                [ ntVal7.1 ] }) },
             { nt =
                 alt,
               label =
                 {},
               rhs =
                 [ tokSym
                     (LIdentRepr
                        {}) ],
               action =
                 lam state11: {errors: Ref [(Info, [Char])], content: String}.
                   lam res11.
                     match
                       res11
                     with
                       [ TokParsed (LIdentTok x2) ]
                     in
                     asDyn
                         { __br_info =
                             x2.info,
                           __br_terms =
                             [ x2.info ],
                           n =
                             [ { v =
                                   nameNoSym
                                     x2.val,
                                 i =
                                   x2.info } ] } },
             { nt =
                 alt,
               label =
                 {},
               rhs =
                 [ tokSym
                     (UIdentRepr
                        {}) ],
               action =
                 lam state12: {errors: Ref [(Info, [Char])], content: String}.
                   lam res12.
                     match
                       res12
                     with
                       [ TokParsed (UIdentTok x3) ]
                     in
                     asDyn
                         { __br_info =
                             x3.info,
                           __br_terms =
                             [ x3.info ],
                           n =
                             [ { v =
                                   nameNoSym
                                     x3.val,
                                 i =
                                   x3.info } ] } },
             { nt =
                 #var"EOOParamAtom",
               label =
                 {},
               rhs =
                 [ ntSym
                     alt ],
               action =
                 lam state13: {errors: Ref [(Info, [Char])], content: String}.
                   lam res13.
                     match
                       res13
                     with
                       [ UserSym val4 ]
                     in
                     let val4: {n: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val4
                       in
                       asDyn
                         (NameEOOParamOp
                            { __br_info =
                                val4.__br_info,
                              __br_terms =
                                val4.__br_terms,
                              n =
                                val4.n }) },
             { nt =
                 #var"EOOParamAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "_" ],
               action =
                 lam state14: {errors: Ref [(Info, [Char])], content: String}.
                   lam res14.
                     match
                       res14
                     with
                       [ LitParsed l13 ]
                     in
                     asDyn
                         (IgnoreEOOParamOp
                            { __br_info =
                                l13.info,
                              __br_terms =
                                [ l13.info ] }) },
             { nt =
                 alt1,
               label =
                 {},
               rhs =
                 [ tokSym
                     (LIdentRepr
                        {}) ],
               action =
                 lam state15: {errors: Ref [(Info, [Char])], content: String}.
                   lam res15.
                     match
                       res15
                     with
                       [ TokParsed (LIdentTok x4) ]
                     in
                     asDyn
                         { __br_info =
                             x4.info,
                           __br_terms =
                             [ x4.info ],
                           n =
                             [ { v =
                                   nameNoSym
                                     x4.val,
                                 i =
                                   x4.info } ] } },
             { nt =
                 alt1,
               label =
                 {},
               rhs =
                 [ tokSym
                     (UIdentRepr
                        {}) ],
               action =
                 lam state16: {errors: Ref [(Info, [Char])], content: String}.
                   lam res16.
                     match
                       res16
                     with
                       [ TokParsed (UIdentTok x5) ]
                     in
                     asDyn
                         { __br_info =
                             x5.info,
                           __br_terms =
                             [ x5.info ],
                           n =
                             [ { v =
                                   nameNoSym
                                     x5.val,
                                 i =
                                   x5.info } ] } },
             { nt =
                 alt2,
               label =
                 {},
               rhs =
                 [ tokSym
                     (LIdentRepr
                        {}) ],
               action =
                 lam state17: {errors: Ref [(Info, [Char])], content: String}.
                   lam res17.
                     match
                       res17
                     with
                       [ TokParsed (LIdentTok x6) ]
                     in
                     asDyn
                         { __br_info =
                             x6.info,
                           __br_terms =
                             [ x6.info ],
                           n2 =
                             [ { v =
                                   nameNoSym
                                     x6.val,
                                 i =
                                   x6.info } ] } },
             { nt =
                 alt2,
               label =
                 {},
               rhs =
                 [ tokSym
                     (UIdentRepr
                        {}) ],
               action =
                 lam state18: {errors: Ref [(Info, [Char])], content: String}.
                   lam res18.
                     match
                       res18
                     with
                       [ TokParsed (UIdentTok x7) ]
                     in
                     asDyn
                         { __br_info =
                             x7.info,
                           __br_terms =
                             [ x7.info ],
                           n2 =
                             [ { v =
                                   nameNoSym
                                     x7.val,
                                 i =
                                   x7.info } ] } },
             { nt =
                 alt3,
               label =
                 {},
               rhs =
                 "",
               action =
                 lam state19: {errors: Ref [(Info, [Char])], content: String}.
                   lam res19.
                     match
                       res19
                     with
                       ""
                     in
                     asDyn
                         { __br_info =
                             NoInfo
                               {},
                           __br_terms =
                             "",
                           n2 =
                             "",
                           ty =
                             "" } },
             { nt =
                 alt3,
               label =
                 {},
               rhs =
                 [ litSym
                     ":",
                   ntSym
                     #var"EOOType",
                   litSym
                     "def",
                   ntSym
                     alt2 ],
               action =
                 lam state20: {errors: Ref [(Info, [Char])], content: String}.
                   lam res20.
                     match
                       res20
                     with
                       [ LitParsed l14,
                         UserSym ntVal8,
                         LitParsed l15,
                         UserSym val5 ]
                     in
                     let ntVal8: (Info, EOOType) =
                         fromDyn
                           ntVal8
                       in
                       let val5: {n2: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val5
                       in
                       asDyn
                         { __br_info =
                             foldl
                               mergeInfo
                               l14.info
                               [ ntVal8.0,
                                 l15.info,
                                 val5.__br_info ],
                           __br_terms =
                             join
                               [ [ l14.info ],
                                 [ l15.info ],
                                 val5.__br_terms ],
                           n2 =
                             val5.n2,
                           ty =
                             [ ntVal8.1 ] } },
             { nt =
                 kleene3,
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOParam",
                   ntSym
                     kleene3 ],
               action =
                 lam state21: {errors: Ref [(Info, [Char])], content: String}.
                   lam res21.
                     match
                       res21
                     with
                       [ UserSym ntVal9,
                         UserSym val6 ]
                     in
                     let ntVal9: (Info, EOOParam) =
                         fromDyn
                           ntVal9
                       in
                       let val6: {params: [EOOParam], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val6
                       in
                       asDyn
                         { __br_info =
                             mergeInfo
                               ntVal9.0
                               val6.__br_info,
                           __br_terms =
                             val6.__br_terms,
                           params =
                             concat
                               [ ntVal9.1 ]
                               val6.params } },
             { nt =
                 kleene3,
               label =
                 {},
               rhs =
                 "",
               action =
                 lam state22: {errors: Ref [(Info, [Char])], content: String}.
                   lam res22.
                     match
                       res22
                     with
                       ""
                     in
                     asDyn
                         { __br_info =
                             NoInfo
                               {},
                           __br_terms =
                             "",
                           params =
                             "" } },
             { nt =
                 #var"EOODefBindingAtom",
               label =
                 {},
               rhs =
                 [ ntSym
                     alt1,
                   ntSym
                     alt3,
                   ntSym
                     kleene3,
                   litSym
                     "=",
                   ntSym
                     #var"EOOExpr" ],
               action =
                 lam state23: {errors: Ref [(Info, [Char])], content: String}.
                   lam res23.
                     match
                       res23
                     with
                       [ UserSym val7,
                         UserSym val8,
                         UserSym val6,
                         LitParsed l16,
                         UserSym ntVal10 ]
                     in
                     let val7: {n: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val7
                       in
                       let val8: {n2: [{i: Info, v: Name}], ty: [EOOType], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val8
                       in
                       let val6: {params: [EOOParam], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val6
                       in
                       let ntVal10: (Info, EOOExpr) =
                         fromDyn
                           ntVal10
                       in
                       asDyn
                         (SimpleEOODefBindingOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  val7.__br_info
                                  [ val8.__br_info,
                                    val6.__br_info,
                                    l16.info,
                                    ntVal10.0 ],
                              __br_terms =
                                join
                                  [ val7.__br_terms,
                                    val8.__br_terms,
                                    val6.__br_terms,
                                    [ l16.info ] ],
                              rhs =
                                [ ntVal10.1 ],
                              params =
                                val6.params,
                              n =
                                val7.n,
                              n2 =
                                val8.n2,
                              ty =
                                val8.ty }) },
             { nt =
                 alt4,
               label =
                 {},
               rhs =
                 [ tokSym
                     (LIdentRepr
                        {}) ],
               action =
                 lam state24: {errors: Ref [(Info, [Char])], content: String}.
                   lam res24.
                     match
                       res24
                     with
                       [ TokParsed (LIdentTok x8) ]
                     in
                     asDyn
                         { __br_info =
                             x8.info,
                           __br_terms =
                             [ x8.info ],
                           n =
                             [ { v =
                                   nameNoSym
                                     x8.val,
                                 i =
                                   x8.info } ] } },
             { nt =
                 alt4,
               label =
                 {},
               rhs =
                 [ tokSym
                     (UIdentRepr
                        {}) ],
               action =
                 lam state25: {errors: Ref [(Info, [Char])], content: String}.
                   lam res25.
                     match
                       res25
                     with
                       [ TokParsed (UIdentTok x9) ]
                     in
                     asDyn
                         { __br_info =
                             x9.info,
                           __br_terms =
                             [ x9.info ],
                           n =
                             [ { v =
                                   nameNoSym
                                     x9.val,
                                 i =
                                   x9.info } ] } },
             { nt =
                 alt5,
               label =
                 {},
               rhs =
                 [ tokSym
                     (LIdentRepr
                        {}) ],
               action =
                 lam state26: {errors: Ref [(Info, [Char])], content: String}.
                   lam res26.
                     match
                       res26
                     with
                       [ TokParsed (LIdentTok x10) ]
                     in
                     asDyn
                         { __br_info =
                             x10.info,
                           __br_terms =
                             [ x10.info ],
                           n2 =
                             [ { v =
                                   nameNoSym
                                     x10.val,
                                 i =
                                   x10.info } ] } },
             { nt =
                 alt5,
               label =
                 {},
               rhs =
                 [ tokSym
                     (UIdentRepr
                        {}) ],
               action =
                 lam state27: {errors: Ref [(Info, [Char])], content: String}.
                   lam res27.
                     match
                       res27
                     with
                       [ TokParsed (UIdentTok x11) ]
                     in
                     asDyn
                         { __br_info =
                             x11.info,
                           __br_terms =
                             [ x11.info ],
                           n2 =
                             [ { v =
                                   nameNoSym
                                     x11.val,
                                 i =
                                   x11.info } ] } },
             { nt =
                 alt6,
               label =
                 {},
               rhs =
                 "",
               action =
                 lam state28: {errors: Ref [(Info, [Char])], content: String}.
                   lam res28.
                     match
                       res28
                     with
                       ""
                     in
                     asDyn
                         { __br_info =
                             NoInfo
                               {},
                           __br_terms =
                             "",
                           n2 =
                             "",
                           ty =
                             "" } },
             { nt =
                 alt6,
               label =
                 {},
               rhs =
                 [ litSym
                     ":",
                   ntSym
                     #var"EOOType",
                   litSym
                     "model",
                   ntSym
                     alt5 ],
               action =
                 lam state29: {errors: Ref [(Info, [Char])], content: String}.
                   lam res29.
                     match
                       res29
                     with
                       [ LitParsed l17,
                         UserSym ntVal11,
                         LitParsed l18,
                         UserSym val9 ]
                     in
                     let ntVal11: (Info, EOOType) =
                         fromDyn
                           ntVal11
                       in
                       let val9: {n2: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val9
                       in
                       asDyn
                         { __br_info =
                             foldl
                               mergeInfo
                               l17.info
                               [ ntVal11.0,
                                 l18.info,
                                 val9.__br_info ],
                           __br_terms =
                             join
                               [ [ l17.info ],
                                 [ l18.info ],
                                 val9.__br_terms ],
                           n2 =
                             val9.n2,
                           ty =
                             [ ntVal11.1 ] } },
             { nt =
                 kleene4,
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOParam",
                   ntSym
                     kleene4 ],
               action =
                 lam state30: {errors: Ref [(Info, [Char])], content: String}.
                   lam res30.
                     match
                       res30
                     with
                       [ UserSym ntVal12,
                         UserSym val10 ]
                     in
                     let ntVal12: (Info, EOOParam) =
                         fromDyn
                           ntVal12
                       in
                       let val10: {params: [EOOParam], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val10
                       in
                       asDyn
                         { __br_info =
                             mergeInfo
                               ntVal12.0
                               val10.__br_info,
                           __br_terms =
                             val10.__br_terms,
                           params =
                             concat
                               [ ntVal12.1 ]
                               val10.params } },
             { nt =
                 kleene4,
               label =
                 {},
               rhs =
                 "",
               action =
                 lam state31: {errors: Ref [(Info, [Char])], content: String}.
                   lam res31.
                     match
                       res31
                     with
                       ""
                     in
                     asDyn
                         { __br_info =
                             NoInfo
                               {},
                           __br_terms =
                             "",
                           params =
                             "" } },
             { nt =
                 kleene5,
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOModelHead",
                   ntSym
                     kleene5 ],
               action =
                 lam state32: {errors: Ref [(Info, [Char])], content: String}.
                   lam res32.
                     match
                       res32
                     with
                       [ UserSym ntVal13,
                         UserSym val11 ]
                     in
                     let ntVal13: (Info, EOOModelHead) =
                         fromDyn
                           ntVal13
                       in
                       let val11: {heads: [EOOModelHead], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val11
                       in
                       asDyn
                         { __br_info =
                             mergeInfo
                               ntVal13.0
                               val11.__br_info,
                           __br_terms =
                             val11.__br_terms,
                           heads =
                             concat
                               [ ntVal13.1 ]
                               val11.heads } },
             { nt =
                 kleene5,
               label =
                 {},
               rhs =
                 "",
               action =
                 lam state33: {errors: Ref [(Info, [Char])], content: String}.
                   lam res33.
                     match
                       res33
                     with
                       ""
                     in
                     asDyn
                         { __br_info =
                             NoInfo
                               {},
                           __br_terms =
                             "",
                           heads =
                             "" } },
             { nt =
                 #var"EOOModelBindingAtom",
               label =
                 {},
               rhs =
                 [ ntSym
                     alt4,
                   ntSym
                     alt6,
                   ntSym
                     kleene4,
                   litSym
                     "=",
                   ntSym
                     kleene5,
                   litSym
                     "equation",
                   ntSym
                     #var"EOOExpr" ],
               action =
                 lam state34: {errors: Ref [(Info, [Char])], content: String}.
                   lam res34.
                     match
                       res34
                     with
                       [ UserSym val12,
                         UserSym val13,
                         UserSym val10,
                         LitParsed l19,
                         UserSym val11,
                         LitParsed l20,
                         UserSym ntVal14 ]
                     in
                     let val12: {n: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val12
                       in
                       let val13: {n2: [{i: Info, v: Name}], ty: [EOOType], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val13
                       in
                       let val10: {params: [EOOParam], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val10
                       in
                       let val11: {heads: [EOOModelHead], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val11
                       in
                       let ntVal14: (Info, EOOExpr) =
                         fromDyn
                           ntVal14
                       in
                       asDyn
                         (SimpleEOOModelBindingOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  val12.__br_info
                                  [ val13.__br_info,
                                    val10.__br_info,
                                    l19.info,
                                    val11.__br_info,
                                    l20.info,
                                    ntVal14.0 ],
                              __br_terms =
                                join
                                  [ val12.__br_terms,
                                    val13.__br_terms,
                                    val10.__br_terms,
                                    [ l19.info ],
                                    val11.__br_terms,
                                    [ l20.info ] ],
                              heads =
                                val11.heads,
                              eqn =
                                [ ntVal14.1 ],
                              eqnkw =
                                [ l20.info ],
                              params =
                                val10.params,
                              n =
                                val12.n,
                              n2 =
                                val13.n2,
                              ty =
                                val13.ty }) },
             { nt =
                 alt7,
               label =
                 {},
               rhs =
                 [ tokSym
                     (LIdentRepr
                        {}) ],
               action =
                 lam state35: {errors: Ref [(Info, [Char])], content: String}.
                   lam res35.
                     match
                       res35
                     with
                       [ TokParsed (LIdentTok x12) ]
                     in
                     asDyn
                         { __br_info =
                             x12.info,
                           __br_terms =
                             [ x12.info ],
                           n =
                             [ { v =
                                   nameNoSym
                                     x12.val,
                                 i =
                                   x12.info } ] } },
             { nt =
                 alt7,
               label =
                 {},
               rhs =
                 [ tokSym
                     (UIdentRepr
                        {}) ],
               action =
                 lam state36: {errors: Ref [(Info, [Char])], content: String}.
                   lam res36.
                     match
                       res36
                     with
                       [ TokParsed (UIdentTok x13) ]
                     in
                     asDyn
                         { __br_info =
                             x13.info,
                           __br_terms =
                             [ x13.info ],
                           n =
                             [ { v =
                                   nameNoSym
                                     x13.val,
                                 i =
                                   x13.info } ] } },
             { nt =
                 #var"EOOExprAtom",
               label =
                 {},
               rhs =
                 [ ntSym
                     alt7 ],
               action =
                 lam state37: {errors: Ref [(Info, [Char])], content: String}.
                   lam res37.
                     match
                       res37
                     with
                       [ UserSym val14 ]
                     in
                     let val14: {n: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val14
                       in
                       asDyn
                         (VarEOOExprOp
                            { __br_info =
                                val14.__br_info,
                              __br_terms =
                                val14.__br_terms,
                              n =
                                val14.n }) },
             { nt =
                 alt8,
               label =
                 {},
               rhs =
                 "",
               action =
                 lam state38: {errors: Ref [(Info, [Char])], content: String}.
                   lam res38.
                     match
                       res38
                     with
                       ""
                     in
                     asDyn
                         { __br_info =
                             NoInfo
                               {},
                           __br_terms =
                             "",
                           ty =
                             "" } },
             { nt =
                 alt8,
               label =
                 {},
               rhs =
                 [ litSym
                     ":",
                   ntSym
                     #var"EOOType" ],
               action =
                 lam state39: {errors: Ref [(Info, [Char])], content: String}.
                   lam res39.
                     match
                       res39
                     with
                       [ LitParsed l21,
                         UserSym ntVal15 ]
                     in
                     let ntVal15: (Info, EOOType) =
                         fromDyn
                           ntVal15
                       in
                       asDyn
                         { __br_info =
                             mergeInfo
                               l21.info
                               ntVal15.0,
                           __br_terms =
                             [ l21.info ],
                           ty =
                             [ ntVal15.1 ] } },
             { nt =
                 #var"EOOExprPrefix",
               label =
                 {},
               rhs =
                 [ litSym
                     "\\",
                   ntSym
                     #var"EOOParam",
                   ntSym
                     alt8,
                   litSym
                     "." ],
               action =
                 lam state40: {errors: Ref [(Info, [Char])], content: String}.
                   lam res40.
                     match
                       res40
                     with
                       [ LitParsed l22,
                         UserSym ntVal16,
                         UserSym val15,
                         LitParsed l23 ]
                     in
                     let ntVal16: (Info, EOOParam) =
                         fromDyn
                           ntVal16
                       in
                       let val15: {ty: [EOOType], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val15
                       in
                       asDyn
                         (AbsEOOExprOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l22.info
                                  [ ntVal16.0,
                                    val15.__br_info,
                                    l23.info ],
                              __br_terms =
                                join
                                  [ [ l22.info ],
                                    val15.__br_terms,
                                    [ l23.info ] ],
                              ty =
                                val15.ty,
                              param =
                                [ ntVal16.1 ] }) },
             { nt =
                 #var"EOOExprInfix",
               label =
                 {},
               rhs =
                 "",
               action =
                 lam state41: {errors: Ref [(Info, [Char])], content: String}.
                   lam res41.
                     match
                       res41
                     with
                       ""
                     in
                     asDyn
                         (AppEOOExprOp
                            { __br_info =
                                NoInfo
                                  {},
                              __br_terms =
                                "" }) },
             { nt =
                 alt9,
               label =
                 {},
               rhs =
                 [ tokSym
                     (IntRepr
                        {}) ],
               action =
                 lam state42: {errors: Ref [(Info, [Char])], content: String}.
                   lam res42.
                     match
                       res42
                     with
                       [ TokParsed (IntTok x14) ]
                     in
                     asDyn
                         { __br_info =
                             x14.info,
                           __br_terms =
                             [ x14.info ],
                           i =
                             [ { v =
                                   x14.val,
                                 i =
                                   x14.info } ],
                           f =
                             "" } },
             { nt =
                 alt9,
               label =
                 {},
               rhs =
                 [ tokSym
                     (FloatRepr
                        {}) ],
               action =
                 lam state43: {errors: Ref [(Info, [Char])], content: String}.
                   lam res43.
                     match
                       res43
                     with
                       [ TokParsed (FloatTok x15) ]
                     in
                     asDyn
                         { __br_info =
                             x15.info,
                           __br_terms =
                             [ x15.info ],
                           i =
                             "",
                           f =
                             [ { v =
                                   x15.val,
                                 i =
                                   x15.info } ] } },
             { nt =
                 #var"EOOExprAtom",
               label =
                 {},
               rhs =
                 [ ntSym
                     alt9 ],
               action =
                 lam state44: {errors: Ref [(Info, [Char])], content: String}.
                   lam res44.
                     match
                       res44
                     with
                       [ UserSym val16 ]
                     in
                     let val16: {f: [{i: Info, v: Float}], i: [{i: Info, v: Int}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val16
                       in
                       asDyn
                         (NumEOOExprOp
                            { __br_info =
                                val16.__br_info,
                              __br_terms =
                                val16.__br_terms,
                              i =
                                val16.i,
                              f =
                                val16.f }) },
             { nt =
                 #var"EOOExprAtom",
               label =
                 {},
               rhs =
                 [ tokSym
                     (StringRepr
                        {}) ],
               action =
                 lam state45: {errors: Ref [(Info, [Char])], content: String}.
                   lam res45.
                     match
                       res45
                     with
                       [ TokParsed (StringTok x16) ]
                     in
                     asDyn
                         (StringEOOExprOp
                            { __br_info =
                                x16.info,
                              __br_terms =
                                [ x16.info ],
                              v =
                                [ { v =
                                      x16.val,
                                    i =
                                      x16.info } ] }) },
             { nt =
                 #var"EOOExprAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "true" ],
               action =
                 lam state46: {errors: Ref [(Info, [Char])], content: String}.
                   lam res46.
                     match
                       res46
                     with
                       [ LitParsed l24 ]
                     in
                     asDyn
                         (TrueEOOExprOp
                            { __br_info =
                                l24.info,
                              __br_terms =
                                [ l24.info ] }) },
             { nt =
                 #var"EOOExprAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "false" ],
               action =
                 lam state47: {errors: Ref [(Info, [Char])], content: String}.
                   lam res47.
                     match
                       res47
                     with
                       [ LitParsed l25 ]
                     in
                     asDyn
                         (FalseEOOExprOp
                            { __br_info =
                                l25.info,
                              __br_terms =
                                [ l25.info ] }) },
             { nt =
                 #var"EOOExprAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "()" ],
               action =
                 lam state48: {errors: Ref [(Info, [Char])], content: String}.
                   lam res48.
                     match
                       res48
                     with
                       [ LitParsed l26 ]
                     in
                     asDyn
                         (UnitEOOExprOp
                            { __br_info =
                                l26.info,
                              __br_terms =
                                [ l26.info ] }) },
             { nt =
                 #var"EOOExprInfix",
               label =
                 {},
               rhs =
                 [ litSym
                     "," ],
               action =
                 lam state49: {errors: Ref [(Info, [Char])], content: String}.
                   lam res49.
                     match
                       res49
                     with
                       [ LitParsed l27 ]
                     in
                     asDyn
                         (TupEOOExprOp
                            { __br_info =
                                l27.info,
                              __br_terms =
                                [ l27.info ] }) },
             { nt =
                 #var"EOOExprInfix",
               label =
                 {},
               rhs =
                 [ litSym
                     ";" ],
               action =
                 lam state50: {errors: Ref [(Info, [Char])], content: String}.
                   lam res50.
                     match
                       res50
                     with
                       [ LitParsed l28 ]
                     in
                     asDyn
                         (ConcatEOOExprOp
                            { __br_info =
                                l28.info,
                              __br_terms =
                                [ l28.info ],
                              op =
                                [ l28.info ] }) },
             { nt =
                 #var"EOOExprInfix",
               label =
                 {},
               rhs =
                 [ litSym
                     "::" ],
               action =
                 lam state51: {errors: Ref [(Info, [Char])], content: String}.
                   lam res51.
                     match
                       res51
                     with
                       [ LitParsed l29 ]
                     in
                     asDyn
                         (ConsEOOExprOp
                            { __br_info =
                                l29.info,
                              __br_terms =
                                [ l29.info ],
                              op =
                                [ l29.info ] }) },
             { nt =
                 alt10,
               label =
                 {},
               rhs =
                 "",
               action =
                 lam state52: {errors: Ref [(Info, [Char])], content: String}.
                   lam res52.
                     match
                       res52
                     with
                       ""
                     in
                     asDyn
                         { __br_info =
                             NoInfo
                               {},
                           __br_terms =
                             "",
                           unbrokenElems =
                             "" } },
             { nt =
                 alt10,
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOExpr" ],
               action =
                 lam state53: {errors: Ref [(Info, [Char])], content: String}.
                   lam res53.
                     match
                       res53
                     with
                       [ UserSym ntVal17 ]
                     in
                     let ntVal17: (Info, EOOExpr) =
                         fromDyn
                           ntVal17
                       in
                       asDyn
                         { __br_info =
                             ntVal17.0,
                           __br_terms =
                             "",
                           unbrokenElems =
                             [ ntVal17.1 ] } },
             { nt =
                 #var"EOOExprAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "[",
                   ntSym
                     alt10,
                   litSym
                     "]" ],
               action =
                 lam state54: {errors: Ref [(Info, [Char])], content: String}.
                   lam res54.
                     match
                       res54
                     with
                       [ LitParsed l30,
                         UserSym val17,
                         LitParsed l31 ]
                     in
                     let val17: {__br_info: Info, __br_terms: [Info], unbrokenElems: [EOOExpr]} =
                         fromDyn
                           val17
                       in
                       asDyn
                         (SeqEOOExprOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l30.info
                                  [ val17.__br_info,
                                    l31.info ],
                              __br_terms =
                                join
                                  [ [ l30.info ],
                                    val17.__br_terms,
                                    [ l31.info ] ],
                              unbrokenElems =
                                val17.unbrokenElems }) },
             { nt =
                 alt11,
               label =
                 {},
               rhs =
                 "",
               action =
                 lam state55: {errors: Ref [(Info, [Char])], content: String}.
                   lam res55.
                     match
                       res55
                     with
                       ""
                     in
                     asDyn
                         { __br_info =
                             NoInfo
                               {},
                           __br_terms =
                             "",
                           ty =
                             "" } },
             { nt =
                 alt11,
               label =
                 {},
               rhs =
                 [ litSym
                     ":",
                   ntSym
                     #var"EOOType" ],
               action =
                 lam state56: {errors: Ref [(Info, [Char])], content: String}.
                   lam res56.
                     match
                       res56
                     with
                       [ LitParsed l32,
                         UserSym ntVal18 ]
                     in
                     let ntVal18: (Info, EOOType) =
                         fromDyn
                           ntVal18
                       in
                       asDyn
                         { __br_info =
                             mergeInfo
                               l32.info
                               ntVal18.0,
                           __br_terms =
                             [ l32.info ],
                           ty =
                             [ ntVal18.1 ] } },
             { nt =
                 #var"EOOExprPrefix",
               label =
                 {},
               rhs =
                 [ litSym
                     "let",
                   ntSym
                     #var"EOOPat",
                   ntSym
                     alt11,
                   litSym
                     "=",
                   ntSym
                     #var"EOOExpr",
                   litSym
                     "in" ],
               action =
                 lam state57: {errors: Ref [(Info, [Char])], content: String}.
                   lam res57.
                     match
                       res57
                     with
                       [ LitParsed l33,
                         UserSym ntVal19,
                         UserSym val18,
                         LitParsed l34,
                         UserSym ntVal20,
                         LitParsed l35 ]
                     in
                     let ntVal19: (Info, EOOPat) =
                         fromDyn
                           ntVal19
                       in
                       let val18: {ty: [EOOType], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val18
                       in
                       let ntVal20: (Info, EOOExpr) =
                         fromDyn
                           ntVal20
                       in
                       asDyn
                         (LetEOOExprOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l33.info
                                  [ ntVal19.0,
                                    val18.__br_info,
                                    l34.info,
                                    ntVal20.0,
                                    l35.info ],
                              __br_terms =
                                join
                                  [ [ l33.info ],
                                    val18.__br_terms,
                                    [ l34.info ],
                                    [ l35.info ] ],
                              rhs =
                                [ ntVal20.1 ],
                              pat =
                                [ ntVal19.1 ],
                              ty =
                                val18.ty }) },
             { nt =
                 #var"EOOExprPrefix",
               label =
                 {},
               rhs =
                 [ litSym
                     "def",
                   ntSym
                     #var"EOODefBinding",
                   litSym
                     "in" ],
               action =
                 lam state58: {errors: Ref [(Info, [Char])], content: String}.
                   lam res58.
                     match
                       res58
                     with
                       [ LitParsed l36,
                         UserSym ntVal21,
                         LitParsed l37 ]
                     in
                     let ntVal21: (Info, EOODefBinding) =
                         fromDyn
                           ntVal21
                       in
                       asDyn
                         (DefEOOExprOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l36.info
                                  [ ntVal21.0,
                                    l37.info ],
                              __br_terms =
                                concat
                                  [ l36.info ]
                                  [ l37.info ],
                              binding =
                                [ ntVal21.1 ] }) },
             { nt =
                 #var"EOOExprPrefix",
               label =
                 {},
               rhs =
                 [ litSym
                     "if",
                   ntSym
                     #var"EOOExpr",
                   litSym
                     "then",
                   ntSym
                     #var"EOOExpr",
                   litSym
                     "else" ],
               action =
                 lam state59: {errors: Ref [(Info, [Char])], content: String}.
                   lam res59.
                     match
                       res59
                     with
                       [ LitParsed l38,
                         UserSym ntVal22,
                         LitParsed l39,
                         UserSym ntVal23,
                         LitParsed l40 ]
                     in
                     let ntVal22: (Info, EOOExpr) =
                         fromDyn
                           ntVal22
                       in
                       let ntVal23: (Info, EOOExpr) =
                         fromDyn
                           ntVal23
                       in
                       asDyn
                         (IfEOOExprOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l38.info
                                  [ ntVal22.0,
                                    l39.info,
                                    ntVal23.0,
                                    l40.info ],
                              __br_terms =
                                join
                                  [ [ l38.info ],
                                    [ l39.info ],
                                    [ l40.info ] ],
                              c =
                                [ ntVal22.1 ],
                              t =
                                [ ntVal23.1 ] }) },
             { nt =
                 alt12,
               label =
                 {},
               rhs =
                 "",
               action =
                 lam state60: {errors: Ref [(Info, [Char])], content: String}.
                   lam res60.
                     match
                       res60
                     with
                       ""
                     in
                     asDyn
                         { __br_info =
                             NoInfo
                               {},
                           __br_terms =
                             "" } },
             { nt =
                 alt12,
               label =
                 {},
               rhs =
                 [ litSym
                     "|" ],
               action =
                 lam state61: {errors: Ref [(Info, [Char])], content: String}.
                   lam res61.
                     match
                       res61
                     with
                       [ LitParsed l41 ]
                     in
                     asDyn
                         { __br_info =
                             l41.info,
                           __br_terms =
                             [ l41.info ] } },
             { nt =
                 kleene6,
               label =
                 {},
               rhs =
                 [ litSym
                     "|",
                   ntSym
                     #var"EOOPat",
                   litSym
                     "->",
                   ntSym
                     #var"EOOExpr",
                   ntSym
                     kleene6 ],
               action =
                 lam state62: {errors: Ref [(Info, [Char])], content: String}.
                   lam res62.
                     match
                       res62
                     with
                       [ LitParsed l42,
                         UserSym ntVal24,
                         LitParsed l43,
                         UserSym ntVal25,
                         UserSym val19 ]
                     in
                     let ntVal24: (Info, EOOPat) =
                         fromDyn
                           ntVal24
                       in
                       let ntVal25: (Info, EOOExpr) =
                         fromDyn
                           ntVal25
                       in
                       let val19: {arms: [{pat: EOOPat, body: EOOExpr}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val19
                       in
                       asDyn
                         { __br_info =
                             foldl
                               mergeInfo
                               l42.info
                               [ ntVal24.0,
                                 l43.info,
                                 ntVal25.0,
                                 val19.__br_info ],
                           __br_terms =
                             join
                               [ [ l42.info ],
                                 [ l43.info ],
                                 val19.__br_terms ],
                           arms =
                             concat
                               [ { pat =
                                     match
                                       [ ntVal24.1 ]
                                     with
                                       [ x17 ] ++ _ ++ ""
                                     in
                                     x17,
                                   body =
                                     match
                                       [ ntVal25.1 ]
                                     with
                                       [ x18 ] ++ _ ++ ""
                                     in
                                     x18 } ]
                               val19.arms } },
             { nt =
                 kleene6,
               label =
                 {},
               rhs =
                 "",
               action =
                 lam state63: {errors: Ref [(Info, [Char])], content: String}.
                   lam res63.
                     match
                       res63
                     with
                       ""
                     in
                     asDyn
                         { __br_info =
                             NoInfo
                               {},
                           __br_terms =
                             "",
                           arms =
                             "" } },
             { nt =
                 #var"EOOExprAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "begin",
                   litSym
                     "match",
                   ntSym
                     #var"EOOExpr",
                   litSym
                     "with",
                   ntSym
                     alt12,
                   ntSym
                     #var"EOOPat",
                   litSym
                     "->",
                   ntSym
                     #var"EOOExpr",
                   ntSym
                     kleene6,
                   litSym
                     "end" ],
               action =
                 lam state64: {errors: Ref [(Info, [Char])], content: String}.
                   lam res64.
                     match
                       res64
                     with
                       [ LitParsed l44,
                         LitParsed l45,
                         UserSym ntVal26,
                         LitParsed l46,
                         UserSym val20,
                         UserSym ntVal27,
                         LitParsed l47,
                         UserSym ntVal28,
                         UserSym val19,
                         LitParsed l48 ]
                     in
                     let ntVal26: (Info, EOOExpr) =
                         fromDyn
                           ntVal26
                       in
                       let val20: {__br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val20
                       in
                       let ntVal27: (Info, EOOPat) =
                         fromDyn
                           ntVal27
                       in
                       let ntVal28: (Info, EOOExpr) =
                         fromDyn
                           ntVal28
                       in
                       let val19: {arms: [{pat: EOOPat, body: EOOExpr}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val19
                       in
                       asDyn
                         (MatchingEOOExprOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l44.info
                                  [ l45.info,
                                    ntVal26.0,
                                    l46.info,
                                    val20.__br_info,
                                    ntVal27.0,
                                    l47.info,
                                    ntVal28.0,
                                    val19.__br_info,
                                    l48.info ],
                              __br_terms =
                                join
                                  [ [ l44.info ],
                                    [ l45.info ],
                                    [ l46.info ],
                                    val20.__br_terms,
                                    [ l47.info ],
                                    val19.__br_terms,
                                    [ l48.info ] ],
                              arms =
                                concat
                                  [ { pat =
                                        match
                                          [ ntVal27.1 ]
                                        with
                                          [ x19 ] ++ _ ++ ""
                                        in
                                        x19,
                                      body =
                                        match
                                          [ ntVal28.1 ]
                                        with
                                          [ x20 ] ++ _ ++ ""
                                        in
                                        x20 } ]
                                  val19.arms,
                              scrut =
                                [ ntVal26.1 ] }) },
             { nt =
                 #var"EOOExprInfix",
               label =
                 {},
               rhs =
                 [ litSym
                     "+" ],
               action =
                 lam state65: {errors: Ref [(Info, [Char])], content: String}.
                   lam res65.
                     match
                       res65
                     with
                       [ LitParsed l49 ]
                     in
                     asDyn
                         (AddfEOOExprOp
                            { __br_info =
                                l49.info,
                              __br_terms =
                                [ l49.info ],
                              op =
                                [ l49.info ] }) },
             { nt =
                 #var"EOOExprInfix",
               label =
                 {},
               rhs =
                 [ litSym
                     "-" ],
               action =
                 lam state66: {errors: Ref [(Info, [Char])], content: String}.
                   lam res66.
                     match
                       res66
                     with
                       [ LitParsed l50 ]
                     in
                     asDyn
                         (SubfEOOExprOp
                            { __br_info =
                                l50.info,
                              __br_terms =
                                [ l50.info ],
                              op =
                                [ l50.info ] }) },
             { nt =
                 #var"EOOExprInfix",
               label =
                 {},
               rhs =
                 [ litSym
                     "*" ],
               action =
                 lam state67: {errors: Ref [(Info, [Char])], content: String}.
                   lam res67.
                     match
                       res67
                     with
                       [ LitParsed l51 ]
                     in
                     asDyn
                         (MulfEOOExprOp
                            { __br_info =
                                l51.info,
                              __br_terms =
                                [ l51.info ],
                              op =
                                [ l51.info ] }) },
             { nt =
                 #var"EOOExprInfix",
               label =
                 {},
               rhs =
                 [ litSym
                     "/" ],
               action =
                 lam state68: {errors: Ref [(Info, [Char])], content: String}.
                   lam res68.
                     match
                       res68
                     with
                       [ LitParsed l52 ]
                     in
                     asDyn
                         (DivfEOOExprOp
                            { __br_info =
                                l52.info,
                              __br_terms =
                                [ l52.info ],
                              op =
                                [ l52.info ] }) },
             { nt =
                 #var"EOOExprPostfix",
               label =
                 {},
               rhs =
                 [ litSym
                     "\'" ],
               action =
                 lam state69: {errors: Ref [(Info, [Char])], content: String}.
                   lam res69.
                     match
                       res69
                     with
                       [ LitParsed l53 ]
                     in
                     asDyn
                         (PrimEOOExprOp
                            { __br_info =
                                l53.info,
                              __br_terms =
                                [ l53.info ],
                              op =
                                [ l53.info ] }) },
             { nt =
                 #var"EOOExprInfix",
               label =
                 {},
               rhs =
                 [ litSym
                     "=" ],
               action =
                 lam state70: {errors: Ref [(Info, [Char])], content: String}.
                   lam res70.
                     match
                       res70
                     with
                       [ LitParsed l54 ]
                     in
                     asDyn
                         (EqnEOOExprOp
                            { __br_info =
                                l54.info,
                              __br_terms =
                                [ l54.info ],
                              op =
                                [ l54.info ] }) },
             { nt =
                 #var"EOOExprPrefix",
               label =
                 {},
               rhs =
                 [ litSym
                     "initial" ],
               action =
                 lam state71: {errors: Ref [(Info, [Char])], content: String}.
                   lam res71.
                     match
                       res71
                     with
                       [ LitParsed l55 ]
                     in
                     asDyn
                         (InitEOOExprOp
                            { __br_info =
                                l55.info,
                              __br_terms =
                                [ l55.info ] }) },
             { nt =
                 #var"EOOExprAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "connect",
                   ntSym
                     #var"EOOExpr",
                   litSym
                     "to",
                   ntSym
                     #var"EOOExpr",
                   litSym
                     "in",
                   ntSym
                     #var"EOOExpr",
                   litSym
                     "with",
                   ntSym
                     #var"EOOExpr",
                   litSym
                     "across",
                   ntSym
                     #var"EOOExpr",
                   litSym
                     "through" ],
               action =
                 lam state72: {errors: Ref [(Info, [Char])], content: String}.
                   lam res72.
                     match
                       res72
                     with
                       [ LitParsed l56,
                         UserSym ntVal29,
                         LitParsed l57,
                         UserSym ntVal30,
                         LitParsed l58,
                         UserSym ntVal31,
                         LitParsed l59,
                         UserSym ntVal32,
                         LitParsed l60,
                         UserSym ntVal33,
                         LitParsed l61 ]
                     in
                     let ntVal29: (Info, EOOExpr) =
                         fromDyn
                           ntVal29
                       in
                       let ntVal30: (Info, EOOExpr) =
                         fromDyn
                           ntVal30
                       in
                       let ntVal31: (Info, EOOExpr) =
                         fromDyn
                           ntVal31
                       in
                       let ntVal32: (Info, EOOExpr) =
                         fromDyn
                           ntVal32
                       in
                       let ntVal33: (Info, EOOExpr) =
                         fromDyn
                           ntVal33
                       in
                       asDyn
                         (ConnectEOOExprOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l56.info
                                  [ ntVal29.0,
                                    l57.info,
                                    ntVal30.0,
                                    l58.info,
                                    ntVal31.0,
                                    l59.info,
                                    ntVal32.0,
                                    l60.info,
                                    ntVal33.0,
                                    l61.info ],
                              __br_terms =
                                join
                                  [ [ l56.info ],
                                    [ l57.info ],
                                    [ l58.info ],
                                    [ l59.info ],
                                    [ l60.info ],
                                    [ l61.info ] ],
                              to =
                                [ ntVal30.1 ],
                              dom =
                                [ ntVal31.1 ],
                              from =
                                [ ntVal29.1 ],
                              across =
                                [ ntVal32.1 ],
                              through =
                                [ ntVal33.1 ] }) },
             { nt =
                 kleene7,
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOModelHead",
                   ntSym
                     kleene7 ],
               action =
                 lam state73: {errors: Ref [(Info, [Char])], content: String}.
                   lam res73.
                     match
                       res73
                     with
                       [ UserSym ntVal34,
                         UserSym val21 ]
                     in
                     let ntVal34: (Info, EOOModelHead) =
                         fromDyn
                           ntVal34
                       in
                       let val21: {heads: [EOOModelHead], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val21
                       in
                       asDyn
                         { __br_info =
                             mergeInfo
                               ntVal34.0
                               val21.__br_info,
                           __br_terms =
                             val21.__br_terms,
                           heads =
                             concat
                               [ ntVal34.1 ]
                               val21.heads } },
             { nt =
                 kleene7,
               label =
                 {},
               rhs =
                 "",
               action =
                 lam state74: {errors: Ref [(Info, [Char])], content: String}.
                   lam res74.
                     match
                       res74
                     with
                       ""
                     in
                     asDyn
                         { __br_info =
                             NoInfo
                               {},
                           __br_terms =
                             "",
                           heads =
                             "" } },
             { nt =
                 #var"EOOExprPrefix",
               label =
                 {},
               rhs =
                 [ litSym
                     "model",
                   ntSym
                     kleene7,
                   litSym
                     "equation" ],
               action =
                 lam state75: {errors: Ref [(Info, [Char])], content: String}.
                   lam res75.
                     match
                       res75
                     with
                       [ LitParsed l62,
                         UserSym val21,
                         LitParsed l63 ]
                     in
                     let val21: {heads: [EOOModelHead], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val21
                       in
                       asDyn
                         (ModelEOOExprOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l62.info
                                  [ val21.__br_info,
                                    l63.info ],
                              __br_terms =
                                join
                                  [ [ l62.info ],
                                    val21.__br_terms,
                                    [ l63.info ] ],
                              heads =
                                val21.heads,
                              eqnkw =
                                [ l63.info ] }) },
             { nt =
                 alt13,
               label =
                 {},
               rhs =
                 [ tokSym
                     (LIdentRepr
                        {}) ],
               action =
                 lam state76: {errors: Ref [(Info, [Char])], content: String}.
                   lam res76.
                     match
                       res76
                     with
                       [ TokParsed (LIdentTok x21) ]
                     in
                     asDyn
                         { __br_info =
                             x21.info,
                           __br_terms =
                             [ x21.info ],
                           ns =
                             [ { v =
                                   nameNoSym
                                     x21.val,
                                 i =
                                   x21.info } ] } },
             { nt =
                 alt13,
               label =
                 {},
               rhs =
                 [ tokSym
                     (UIdentRepr
                        {}) ],
               action =
                 lam state77: {errors: Ref [(Info, [Char])], content: String}.
                   lam res77.
                     match
                       res77
                     with
                       [ TokParsed (UIdentTok x22) ]
                     in
                     asDyn
                         { __br_info =
                             x22.info,
                           __br_terms =
                             [ x22.info ],
                           ns =
                             [ { v =
                                   nameNoSym
                                     x22.val,
                                 i =
                                   x22.info } ] } },
             { nt =
                 alt14,
               label =
                 {},
               rhs =
                 [ litSym
                     ",",
                   tokSym
                     (LIdentRepr
                        {}) ],
               action =
                 lam state78: {errors: Ref [(Info, [Char])], content: String}.
                   lam res78.
                     match
                       res78
                     with
                       [ LitParsed l64,
                         TokParsed (LIdentTok x23) ]
                     in
                     asDyn
                         { __br_info =
                             mergeInfo
                               l64.info
                               x23.info,
                           __br_terms =
                             concat
                               [ l64.info ]
                               [ x23.info ],
                           ns =
                             [ { v =
                                   nameNoSym
                                     x23.val,
                                 i =
                                   x23.info } ] } },
             { nt =
                 alt14,
               label =
                 {},
               rhs =
                 [ tokSym
                     (UIdentRepr
                        {}) ],
               action =
                 lam state79: {errors: Ref [(Info, [Char])], content: String}.
                   lam res79.
                     match
                       res79
                     with
                       [ TokParsed (UIdentTok x24) ]
                     in
                     asDyn
                         { __br_info =
                             x24.info,
                           __br_terms =
                             [ x24.info ],
                           ns =
                             [ { v =
                                   nameNoSym
                                     x24.val,
                                 i =
                                   x24.info } ] } },
             { nt =
                 kleene8,
               label =
                 {},
               rhs =
                 [ ntSym
                     alt14,
                   ntSym
                     kleene8 ],
               action =
                 lam state80: {errors: Ref [(Info, [Char])], content: String}.
                   lam res80.
                     match
                       res80
                     with
                       [ UserSym val22,
                         UserSym val23 ]
                     in
                     let val22: {ns: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val22
                       in
                       let val23: {ns: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val23
                       in
                       asDyn
                         { __br_info =
                             mergeInfo
                               val22.__br_info
                               val23.__br_info,
                           __br_terms =
                             concat
                               val22.__br_terms
                               val23.__br_terms,
                           ns =
                             concat
                               val22.ns
                               val23.ns } },
             { nt =
                 kleene8,
               label =
                 {},
               rhs =
                 "",
               action =
                 lam state81: {errors: Ref [(Info, [Char])], content: String}.
                   lam res81.
                     match
                       res81
                     with
                       ""
                     in
                     asDyn
                         { __br_info =
                             NoInfo
                               {},
                           __br_terms =
                             "",
                           ns =
                             "" } },
             { nt =
                 #var"EOOModelHeadAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "var",
                   ntSym
                     alt13,
                   ntSym
                     kleene8,
                   litSym
                     ":",
                   ntSym
                     #var"EOOType" ],
               action =
                 lam state82: {errors: Ref [(Info, [Char])], content: String}.
                   lam res82.
                     match
                       res82
                     with
                       [ LitParsed l65,
                         UserSym val24,
                         UserSym val23,
                         LitParsed l66,
                         UserSym ntVal35 ]
                     in
                     let val24: {ns: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val24
                       in
                       let val23: {ns: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val23
                       in
                       let ntVal35: (Info, EOOType) =
                         fromDyn
                           ntVal35
                       in
                       asDyn
                         (VarEOOModelHeadOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l65.info
                                  [ val24.__br_info,
                                    val23.__br_info,
                                    l66.info,
                                    ntVal35.0 ],
                              __br_terms =
                                join
                                  [ [ l65.info ],
                                    val24.__br_terms,
                                    val23.__br_terms,
                                    [ l66.info ] ],
                              ty =
                                [ ntVal35.1 ],
                              ns =
                                concat
                                  val24.ns
                                  val23.ns }) },
             { nt =
                 alt15,
               label =
                 {},
               rhs =
                 [ tokSym
                     (LIdentRepr
                        {}) ],
               action =
                 lam state83: {errors: Ref [(Info, [Char])], content: String}.
                   lam res83.
                     match
                       res83
                     with
                       [ TokParsed (LIdentTok x25) ]
                     in
                     asDyn
                         { __br_info =
                             x25.info,
                           __br_terms =
                             [ x25.info ],
                           ns =
                             [ { v =
                                   nameNoSym
                                     x25.val,
                                 i =
                                   x25.info } ] } },
             { nt =
                 alt15,
               label =
                 {},
               rhs =
                 [ tokSym
                     (UIdentRepr
                        {}) ],
               action =
                 lam state84: {errors: Ref [(Info, [Char])], content: String}.
                   lam res84.
                     match
                       res84
                     with
                       [ TokParsed (UIdentTok x26) ]
                     in
                     asDyn
                         { __br_info =
                             x26.info,
                           __br_terms =
                             [ x26.info ],
                           ns =
                             [ { v =
                                   nameNoSym
                                     x26.val,
                                 i =
                                   x26.info } ] } },
             { nt =
                 alt16,
               label =
                 {},
               rhs =
                 [ litSym
                     ",",
                   tokSym
                     (LIdentRepr
                        {}) ],
               action =
                 lam state85: {errors: Ref [(Info, [Char])], content: String}.
                   lam res85.
                     match
                       res85
                     with
                       [ LitParsed l67,
                         TokParsed (LIdentTok x27) ]
                     in
                     asDyn
                         { __br_info =
                             mergeInfo
                               l67.info
                               x27.info,
                           __br_terms =
                             concat
                               [ l67.info ]
                               [ x27.info ],
                           ns =
                             [ { v =
                                   nameNoSym
                                     x27.val,
                                 i =
                                   x27.info } ] } },
             { nt =
                 alt16,
               label =
                 {},
               rhs =
                 [ tokSym
                     (UIdentRepr
                        {}) ],
               action =
                 lam state86: {errors: Ref [(Info, [Char])], content: String}.
                   lam res86.
                     match
                       res86
                     with
                       [ TokParsed (UIdentTok x28) ]
                     in
                     asDyn
                         { __br_info =
                             x28.info,
                           __br_terms =
                             [ x28.info ],
                           ns =
                             [ { v =
                                   nameNoSym
                                     x28.val,
                                 i =
                                   x28.info } ] } },
             { nt =
                 kleene9,
               label =
                 {},
               rhs =
                 [ ntSym
                     alt16,
                   ntSym
                     kleene9 ],
               action =
                 lam state87: {errors: Ref [(Info, [Char])], content: String}.
                   lam res87.
                     match
                       res87
                     with
                       [ UserSym val25,
                         UserSym val26 ]
                     in
                     let val25: {ns: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val25
                       in
                       let val26: {ns: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val26
                       in
                       asDyn
                         { __br_info =
                             mergeInfo
                               val25.__br_info
                               val26.__br_info,
                           __br_terms =
                             concat
                               val25.__br_terms
                               val26.__br_terms,
                           ns =
                             concat
                               val25.ns
                               val26.ns } },
             { nt =
                 kleene9,
               label =
                 {},
               rhs =
                 "",
               action =
                 lam state88: {errors: Ref [(Info, [Char])], content: String}.
                   lam res88.
                     match
                       res88
                     with
                       ""
                     in
                     asDyn
                         { __br_info =
                             NoInfo
                               {},
                           __br_terms =
                             "",
                           ns =
                             "" } },
             { nt =
                 #var"EOOModelHeadAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "node",
                   ntSym
                     alt15,
                   ntSym
                     kleene9 ],
               action =
                 lam state89: {errors: Ref [(Info, [Char])], content: String}.
                   lam res89.
                     match
                       res89
                     with
                       [ LitParsed l68,
                         UserSym val27,
                         UserSym val26 ]
                     in
                     let val27: {ns: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val27
                       in
                       let val26: {ns: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val26
                       in
                       asDyn
                         (NodeEOOModelHeadOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l68.info
                                  [ val27.__br_info,
                                    val26.__br_info ],
                              __br_terms =
                                join
                                  [ [ l68.info ],
                                    val27.__br_terms,
                                    val26.__br_terms ],
                              ns =
                                concat
                                  val27.ns
                                  val26.ns }) },
             { nt =
                 #var"EOOModelHeadAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "def",
                   ntSym
                     #var"EOODefBinding",
                   litSym
                     "end" ],
               action =
                 lam state90: {errors: Ref [(Info, [Char])], content: String}.
                   lam res90.
                     match
                       res90
                     with
                       [ LitParsed l69,
                         UserSym ntVal36,
                         LitParsed l70 ]
                     in
                     let ntVal36: (Info, EOODefBinding) =
                         fromDyn
                           ntVal36
                       in
                       asDyn
                         (DefEOOModelHeadOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l69.info
                                  [ ntVal36.0,
                                    l70.info ],
                              __br_terms =
                                concat
                                  [ l69.info ]
                                  [ l70.info ],
                              binding =
                                [ ntVal36.1 ] }) },
             { nt =
                 #var"EOOModelHeadAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "model",
                   ntSym
                     #var"EOOModelBinding",
                   litSym
                     "end" ],
               action =
                 lam state91: {errors: Ref [(Info, [Char])], content: String}.
                   lam res91.
                     match
                       res91
                     with
                       [ LitParsed l71,
                         UserSym ntVal37,
                         LitParsed l72 ]
                     in
                     let ntVal37: (Info, EOOModelBinding) =
                         fromDyn
                           ntVal37
                       in
                       asDyn
                         (ModelEOOModelHeadOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l71.info
                                  [ ntVal37.0,
                                    l72.info ],
                              __br_terms =
                                concat
                                  [ l71.info ]
                                  [ l72.info ],
                              binding =
                                [ ntVal37.1 ] }) },
             { nt =
                 #var"EOOPatAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "_" ],
               action =
                 lam state92: {errors: Ref [(Info, [Char])], content: String}.
                   lam res92.
                     match
                       res92
                     with
                       [ LitParsed l73 ]
                     in
                     asDyn
                         (WildEOOPatOp
                            { __br_info =
                                l73.info,
                              __br_terms =
                                [ l73.info ] }) },
             { nt =
                 alt17,
               label =
                 {},
               rhs =
                 [ tokSym
                     (LIdentRepr
                        {}) ],
               action =
                 lam state93: {errors: Ref [(Info, [Char])], content: String}.
                   lam res93.
                     match
                       res93
                     with
                       [ TokParsed (LIdentTok x29) ]
                     in
                     asDyn
                         { __br_info =
                             x29.info,
                           __br_terms =
                             [ x29.info ],
                           n =
                             [ { v =
                                   nameNoSym
                                     x29.val,
                                 i =
                                   x29.info } ] } },
             { nt =
                 alt17,
               label =
                 {},
               rhs =
                 [ tokSym
                     (UIdentRepr
                        {}) ],
               action =
                 lam state94: {errors: Ref [(Info, [Char])], content: String}.
                   lam res94.
                     match
                       res94
                     with
                       [ TokParsed (UIdentTok x30) ]
                     in
                     asDyn
                         { __br_info =
                             x30.info,
                           __br_terms =
                             [ x30.info ],
                           n =
                             [ { v =
                                   nameNoSym
                                     x30.val,
                                 i =
                                   x30.info } ] } },
             { nt =
                 #var"EOOPatAtom",
               label =
                 {},
               rhs =
                 [ ntSym
                     alt17 ],
               action =
                 lam state95: {errors: Ref [(Info, [Char])], content: String}.
                   lam res95.
                     match
                       res95
                     with
                       [ UserSym val28 ]
                     in
                     let val28: {n: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val28
                       in
                       asDyn
                         (BindEOOPatOp
                            { __br_info =
                                val28.__br_info,
                              __br_terms =
                                val28.__br_terms,
                              n =
                                val28.n }) },
             { nt =
                 #var"EOOPatAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "()" ],
               action =
                 lam state96: {errors: Ref [(Info, [Char])], content: String}.
                   lam res96.
                     match
                       res96
                     with
                       [ LitParsed l74 ]
                     in
                     asDyn
                         (UnitEOOPatOp
                            { __br_info =
                                l74.info,
                              __br_terms =
                                [ l74.info ] }) },
             { nt =
                 #var"EOOPatInfix",
               label =
                 {},
               rhs =
                 [ litSym
                     "," ],
               action =
                 lam state97: {errors: Ref [(Info, [Char])], content: String}.
                   lam res97.
                     match
                       res97
                     with
                       [ LitParsed l75 ]
                     in
                     asDyn
                         (TupEOOPatOp
                            { __br_info =
                                l75.info,
                              __br_terms =
                                [ l75.info ] }) },
             { nt =
                 #var"EOOPatInfix",
               label =
                 {},
               rhs =
                 [ litSym
                     "::" ],
               action =
                 lam state98: {errors: Ref [(Info, [Char])], content: String}.
                   lam res98.
                     match
                       res98
                     with
                       [ LitParsed l76 ]
                     in
                     asDyn
                         (ConsEOOPatOp
                            { __br_info =
                                l76.info,
                              __br_terms =
                                [ l76.info ] }) },
             { nt =
                 #var"EOOPatAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "[",
                   ntSym
                     #var"EOOPat",
                   litSym
                     "]" ],
               action =
                 lam state99: {errors: Ref [(Info, [Char])], content: String}.
                   lam res99.
                     match
                       res99
                     with
                       [ LitParsed l77,
                         UserSym ntVal38,
                         LitParsed l78 ]
                     in
                     let ntVal38: (Info, EOOPat) =
                         fromDyn
                           ntVal38
                       in
                       asDyn
                         (SeqEOOPatOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l77.info
                                  [ ntVal38.0,
                                    l78.info ],
                              __br_terms =
                                concat
                                  [ l77.info ]
                                  [ l78.info ],
                              elems =
                                [ ntVal38.1 ] }) },
             { nt =
                 #var"EOOTypeAtom",
               label =
                 {},
               rhs =
                 [ tokSym
                     (LIdentRepr
                        {}) ],
               action =
                 lam state100: {errors: Ref [(Info, [Char])], content: String}.
                   lam res100.
                     match
                       res100
                     with
                       [ TokParsed (LIdentTok x31) ]
                     in
                     asDyn
                         (VarEOOTypeOp
                            { __br_info =
                                x31.info,
                              __br_terms =
                                [ x31.info ],
                              n =
                                [ { v =
                                      nameNoSym
                                        x31.val,
                                    i =
                                      x31.info } ] }) },
             { nt =
                 #var"EOOTypeAtom",
               label =
                 {},
               rhs =
                 [ tokSym
                     (UIdentRepr
                        {}) ],
               action =
                 lam state101: {errors: Ref [(Info, [Char])], content: String}.
                   lam res101.
                     match
                       res101
                     with
                       [ TokParsed (UIdentTok x32) ]
                     in
                     asDyn
                         (ConEOOTypeOp
                            { __br_info =
                                x32.info,
                              __br_terms =
                                [ x32.info ],
                              n =
                                [ { v =
                                      nameNoSym
                                        x32.val,
                                    i =
                                      x32.info } ] }) },
             { nt =
                 #var"EOOTypeInfix",
               label =
                 {},
               rhs =
                 "",
               action =
                 lam state102: {errors: Ref [(Info, [Char])], content: String}.
                   lam res102.
                     match
                       res102
                     with
                       ""
                     in
                     asDyn
                         (AppEOOTypeOp
                            { __br_info =
                                NoInfo
                                  {},
                              __br_terms =
                                "" }) },
             { nt =
                 #var"EOOTypeInfix",
               label =
                 {},
               rhs =
                 [ litSym
                     "*" ],
               action =
                 lam state103: {errors: Ref [(Info, [Char])], content: String}.
                   lam res103.
                     match
                       res103
                     with
                       [ LitParsed l79 ]
                     in
                     asDyn
                         (TupEOOTypeOp
                            { __br_info =
                                l79.info,
                              __br_terms =
                                [ l79.info ] }) },
             { nt =
                 #var"EOOTypeAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "[",
                   ntSym
                     #var"EOOType",
                   litSym
                     "]" ],
               action =
                 lam state104: {errors: Ref [(Info, [Char])], content: String}.
                   lam res104.
                     match
                       res104
                     with
                       [ LitParsed l80,
                         UserSym ntVal39,
                         LitParsed l81 ]
                     in
                     let ntVal39: (Info, EOOType) =
                         fromDyn
                           ntVal39
                       in
                       asDyn
                         (SeqEOOTypeOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l80.info
                                  [ ntVal39.0,
                                    l81.info ],
                              __br_terms =
                                concat
                                  [ l80.info ]
                                  [ l81.info ],
                              ty =
                                [ ntVal39.1 ] }) },
             { nt =
                 #var"EOOTypeInfix",
               label =
                 {},
               rhs =
                 [ litSym
                     "->" ],
               action =
                 lam state105: {errors: Ref [(Info, [Char])], content: String}.
                   lam res105.
                     match
                       res105
                     with
                       [ LitParsed l82 ]
                     in
                     asDyn
                         (ArrowEOOTypeOp
                            { __br_info =
                                l82.info,
                              __br_terms =
                                [ l82.info ] }) },
             { nt =
                 kleene10,
               label =
                 {},
               rhs =
                 [ tokSym
                     (LIdentRepr
                        {}),
                   ntSym
                     kleene10 ],
               action =
                 lam state106: {errors: Ref [(Info, [Char])], content: String}.
                   lam res106.
                     match
                       res106
                     with
                       [ TokParsed (LIdentTok x33),
                         UserSym val29 ]
                     in
                     let val29: {ns: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val29
                       in
                       asDyn
                         { __br_info =
                             mergeInfo
                               x33.info
                               val29.__br_info,
                           __br_terms =
                             concat
                               [ x33.info ]
                               val29.__br_terms,
                           ns =
                             concat
                               [ { v =
                                     nameNoSym
                                       x33.val,
                                   i =
                                     x33.info } ]
                               val29.ns } },
             { nt =
                 kleene10,
               label =
                 {},
               rhs =
                 "",
               action =
                 lam state107: {errors: Ref [(Info, [Char])], content: String}.
                   lam res107.
                     match
                       res107
                     with
                       ""
                     in
                     asDyn
                         { __br_info =
                             NoInfo
                               {},
                           __br_terms =
                             "",
                           ns =
                             "" } },
             { nt =
                 #var"EOOTypePrefix",
               label =
                 {},
               rhs =
                 [ litSym
                     "forall",
                   tokSym
                     (LIdentRepr
                        {}),
                   ntSym
                     kleene10,
                   litSym
                     "." ],
               action =
                 lam state108: {errors: Ref [(Info, [Char])], content: String}.
                   lam res108.
                     match
                       res108
                     with
                       [ LitParsed l83,
                         TokParsed (LIdentTok x34),
                         UserSym val29,
                         LitParsed l84 ]
                     in
                     let val29: {ns: [{i: Info, v: Name}], __br_info: Info, __br_terms: [Info]} =
                         fromDyn
                           val29
                       in
                       asDyn
                         (ForallEOOTypeOp
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l83.info
                                  [ x34.info,
                                    val29.__br_info,
                                    l84.info ],
                              __br_terms =
                                join
                                  [ [ l83.info ],
                                    [ x34.info ],
                                    val29.__br_terms,
                                    [ l84.info ] ],
                              ns =
                                concat
                                  [ { v =
                                        nameNoSym
                                          x34.val,
                                      i =
                                        x34.info } ]
                                  val29.ns }) },
             { nt =
                 #var"EOOExprAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "(",
                   ntSym
                     #var"EOOExpr",
                   litSym
                     ")" ],
               action =
                 lam #var"".
                   lam seq.
                     match
                       seq
                     with
                       [ LitParsed l85,
                         UserSym ntVal40,
                         LitParsed l86 ]
                     in
                     let ntVal40: (Info, EOOExpr) =
                         fromDyn
                           ntVal40
                       in
                       asDyn
                         (EOOExprGrouping
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l85.info
                                  [ ntVal40.0,
                                    l86.info ],
                              __br_terms =
                                [ l85.info,
                                  l86.info ],
                              inner =
                                match
                                  [ ntVal40.1 ]
                                with
                                  [ x35 ]
                                in
                                x35 }) },
             { nt =
                 #var"EOOPatAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "(",
                   ntSym
                     #var"EOOPat",
                   litSym
                     ")" ],
               action =
                 lam #var"".
                   lam seq1.
                     match
                       seq1
                     with
                       [ LitParsed l87,
                         UserSym ntVal41,
                         LitParsed l88 ]
                     in
                     let ntVal41: (Info, EOOPat) =
                         fromDyn
                           ntVal41
                       in
                       asDyn
                         (EOOPatGrouping
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l87.info
                                  [ ntVal41.0,
                                    l88.info ],
                              __br_terms =
                                [ l87.info,
                                  l88.info ],
                              inner =
                                match
                                  [ ntVal41.1 ]
                                with
                                  [ x35 ]
                                in
                                x35 }) },
             { nt =
                 #var"EOOTypeAtom",
               label =
                 {},
               rhs =
                 [ litSym
                     "(",
                   ntSym
                     #var"EOOType",
                   litSym
                     ")" ],
               action =
                 lam #var"".
                   lam seq2.
                     match
                       seq2
                     with
                       [ LitParsed l89,
                         UserSym ntVal42,
                         LitParsed l90 ]
                     in
                     let ntVal42: (Info, EOOType) =
                         fromDyn
                           ntVal42
                       in
                       asDyn
                         (EOOTypeGrouping
                            { __br_info =
                                foldl
                                  mergeInfo
                                  l89.info
                                  [ ntVal42.0,
                                    l90.info ],
                              __br_terms =
                                [ l89.info,
                                  l90.info ],
                              inner =
                                match
                                  [ ntVal42.1 ]
                                with
                                  [ x35 ]
                                in
                                x35 }) },
             { nt =
                 #var"EOOProg",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOProg_lclosed" ],
               action =
                 lam #var"".
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState
                               {})) },
             { nt =
                 #var"EOOProg_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOProgAtom",
                   ntSym
                     #var"EOOProg_lopen" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOProgOpAtom
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOProg_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOProgInfix",
                   ntSym
                     #var"EOOProg_lclosed" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOProgOpInfix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOProg_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOProgPrefix",
                   ntSym
                     #var"EOOProg_lclosed" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOProgOpPrefix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOProg_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOProgPostfix",
                   ntSym
                     #var"EOOProg_lopen" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOProgOpPostfix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOProg_lopen",
               label =
                 {},
               rhs =
                 "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeEOOProgOp
                            p
                            st) },
             { nt =
                 #var"EOOTop",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOTop_lclosed" ],
               action =
                 lam #var"".
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState
                               {})) },
             { nt =
                 #var"EOOTop_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOTopAtom",
                   ntSym
                     #var"EOOTop_lopen" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOTopOpAtom
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOTop_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOTopInfix",
                   ntSym
                     #var"EOOTop_lclosed" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOTopOpInfix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOTop_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOTopPrefix",
                   ntSym
                     #var"EOOTop_lclosed" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOTopOpPrefix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOTop_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOTopPostfix",
                   ntSym
                     #var"EOOTop_lopen" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOTopOpPostfix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOTop_lopen",
               label =
                 {},
               rhs =
                 "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeEOOTopOp
                            p
                            st) },
             { nt =
                 #var"EOOParam",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOParam_lclosed" ],
               action =
                 lam #var"".
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState
                               {})) },
             { nt =
                 #var"EOOParam_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOParamAtom",
                   ntSym
                     #var"EOOParam_lopen" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOParamOpAtom
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOParam_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOParamInfix",
                   ntSym
                     #var"EOOParam_lclosed" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOParamOpInfix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOParam_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOParamPrefix",
                   ntSym
                     #var"EOOParam_lclosed" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOParamOpPrefix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOParam_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOParamPostfix",
                   ntSym
                     #var"EOOParam_lopen" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOParamOpPostfix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOParam_lopen",
               label =
                 {},
               rhs =
                 "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeEOOParamOp
                            p
                            st) },
             { nt =
                 #var"EOODefBinding",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOODefBinding_lclosed" ],
               action =
                 lam #var"".
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState
                               {})) },
             { nt =
                 #var"EOODefBinding_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOODefBindingAtom",
                   ntSym
                     #var"EOODefBinding_lopen" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOODefBindingOpAtom
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOODefBinding_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOODefBindingInfix",
                   ntSym
                     #var"EOODefBinding_lclosed" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOODefBindingOpInfix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOODefBinding_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOODefBindingPrefix",
                   ntSym
                     #var"EOODefBinding_lclosed" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOODefBindingOpPrefix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOODefBinding_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOODefBindingPostfix",
                   ntSym
                     #var"EOODefBinding_lopen" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOODefBindingOpPostfix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOODefBinding_lopen",
               label =
                 {},
               rhs =
                 "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeEOODefBindingOp
                            p
                            st) },
             { nt =
                 #var"EOOModelBinding",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOModelBinding_lclosed" ],
               action =
                 lam #var"".
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState
                               {})) },
             { nt =
                 #var"EOOModelBinding_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOModelBindingAtom",
                   ntSym
                     #var"EOOModelBinding_lopen" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOModelBindingOpAtom
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOModelBinding_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOModelBindingInfix",
                   ntSym
                     #var"EOOModelBinding_lclosed" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOModelBindingOpInfix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOModelBinding_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOModelBindingPrefix",
                   ntSym
                     #var"EOOModelBinding_lclosed" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOModelBindingOpPrefix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOModelBinding_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOModelBindingPostfix",
                   ntSym
                     #var"EOOModelBinding_lopen" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOModelBindingOpPostfix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOModelBinding_lopen",
               label =
                 {},
               rhs =
                 "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeEOOModelBindingOp
                            p
                            st) },
             { nt =
                 #var"EOOExpr",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOExpr_lclosed" ],
               action =
                 lam #var"".
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState
                               {})) },
             { nt =
                 #var"EOOExpr_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOExprAtom",
                   ntSym
                     #var"EOOExpr_lopen" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOExprOpAtom
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOExpr_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOExprInfix",
                   ntSym
                     #var"EOOExpr_lclosed" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOExprOpInfix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOExpr_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOExprPrefix",
                   ntSym
                     #var"EOOExpr_lclosed" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOExprOpPrefix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOExpr_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOExprPostfix",
                   ntSym
                     #var"EOOExpr_lopen" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOExprOpPostfix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOExpr_lopen",
               label =
                 {},
               rhs =
                 "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeEOOExprOp
                            p
                            st) },
             { nt =
                 #var"EOOPat",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOPat_lclosed" ],
               action =
                 lam #var"".
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState
                               {})) },
             { nt =
                 #var"EOOPat_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOPatAtom",
                   ntSym
                     #var"EOOPat_lopen" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOPatOpAtom
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOPat_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOPatInfix",
                   ntSym
                     #var"EOOPat_lclosed" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOPatOpInfix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOPat_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOPatPrefix",
                   ntSym
                     #var"EOOPat_lclosed" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOPatOpPrefix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOPat_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOPatPostfix",
                   ntSym
                     #var"EOOPat_lopen" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOPatOpPostfix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOPat_lopen",
               label =
                 {},
               rhs =
                 "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeEOOPatOp
                            p
                            st) },
             { nt =
                 #var"EOOType",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOType_lclosed" ],
               action =
                 lam #var"".
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState
                               {})) },
             { nt =
                 #var"EOOType_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOTypeAtom",
                   ntSym
                     #var"EOOType_lopen" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOTypeOpAtom
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOType_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOTypeInfix",
                   ntSym
                     #var"EOOType_lclosed" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOTypeOpInfix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOType_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOTypePrefix",
                   ntSym
                     #var"EOOType_lclosed" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOTypeOpPrefix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOType_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOTypePostfix",
                   ntSym
                     #var"EOOType_lopen" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOTypeOpPostfix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOType_lopen",
               label =
                 {},
               rhs =
                 "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeEOOTypeOp
                            p
                            st) },
             { nt =
                 #var"EOOModelHead",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOModelHead_lclosed" ],
               action =
                 lam #var"".
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym cont ]
                     in
                     fromDyn
                         cont
                         (Some
                            (breakableInitState
                               {})) },
             { nt =
                 #var"EOOModelHead_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOModelHeadAtom",
                   ntSym
                     #var"EOOModelHead_lopen" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOModelHeadOpAtom
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOModelHead_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOModelHeadInfix",
                   ntSym
                     #var"EOOModelHead_lclosed" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOModelHeadOpInfix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOModelHead_lclosed",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOModelHeadPrefix",
                   ntSym
                     #var"EOOModelHead_lclosed" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOModelHeadOpPrefix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOModelHead_lopen",
               label =
                 {},
               rhs =
                 [ ntSym
                     #var"EOOModelHeadPostfix",
                   ntSym
                     #var"EOOModelHead_lopen" ],
               action =
                 lam p.
                   lam seq3.
                     match
                       seq3
                     with
                       [ UserSym x35,
                         UserSym cont ]
                     in
                     asDyn
                         (lam st.
                            fromDyn
                              cont
                              (addEOOModelHeadOpPostfix
                                 p
                                 (fromDyn
                                    x35)
                                 st)) },
             { nt =
                 #var"EOOModelHead_lopen",
               label =
                 {},
               rhs =
                 "",
               action =
                 lam p.
                   lam #var"".
                     asDyn
                       (lam st.
                          finalizeEOOModelHeadOp
                            p
                            st) } ] })
  in
  match
    target
  with
    Right table
  in
  table
let parseEOO =
  lam filename.
    lam content.
      use ParseEOO
      in
      let config9 =
        { errors =
            ref
              "",
          content =
            content }
      in
      let res109 =
        parseWithTable
          _table
          filename
          config9
          content
      in
      let #var"X" =
        (res109, deref
          config9.errors)
      in
      match
        #var"X"
      with
        (Right dyn, "")
      then
        match
          fromDyn
            dyn
        with
          (_, res109)
        in
        Right
            res109
      else
        match
          #var"X"
        with
          (Left err, errors)
        then
          let err =
            ll1DefaultHighlight
              content
              (ll1ToErrorHighlightSpec
                 err)
          in
          Left
            (snoc
               errors
               err)
        else
          match
            #var"X"
          with
            (_, errors)
          in
          Left
              errors
let parseEOOExn =
  lam filename.
    lam content.
      let #var"X" =
        parseEOO
          filename
          content
      in
      match
        #var"X"
      with
        Left errors
      then
        (for_
             errors
             (lam x35.
                match
                  x35
                with
                  (info, msg)
                in
                printLn
                    (infoErrorString
                       info
                       msg)))
        ; exit
          1
      else
        match
          #var"X"
        with
          Right file
        in
        file
mexpr
{}