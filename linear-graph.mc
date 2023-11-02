-- This file implements the linear-graph theoretical method for representing
-- topologies of physical systems and finding the fundamental cutset and
-- circuitset. E.g., see Gordon C Andrews, Marc J Richard, Ronald J Anderson, A
-- general vector-network formulation for dynamic systems with kinematic
-- constraints and/or McPhee, J.J. On the use of linear graph theory in
-- multibody system dynamics.

include "utest.mc"
include "math.mc"
include "map.mc"
include "option.mc"

type LGEdge = Int
-- Elements in the incidence matrix has one of these three values:
let lgNoEdge = 0                -- missing edge.
let lgInEdge = negi 1           -- incomming edge.
let lgOutEdge = 1               -- outgoing edge.

-- Sparse representation of the incidence matrix of the graph
type LGIMMatrixRow = (Map Int LGEdge, Int)
type LGIMatrix = (Map Int (Map Int LGEdge), (Int, Int))

-- Equality of two incidence matrix rows.
let lgIMRowEq : LGIMMatrixRow -> LGIMMatrixRow -> Bool
  = lam l. lam r. and (mapEq eqi l.0 r.0) (eqi l.1 r.1)

-- Creates a n x m matrix with no incidence.
let lgIMEmpty : Int -> Int -> LGIMatrix = lam n. lam m. (mapEmpty subi, (n, m))

-- Returns the dimenstions of an incidence matrix.
let lgIMDim : LGIMatrix -> (Int, Int) = lam mat. mat.1

-- Equality of two incidence matrices.
let lgIMEq : LGIMatrix -> LGIMatrix -> Bool
  = lam l. lam r.
    match (l, r) with ((ldata, ldim), (rdata, rdim)) in
    if and (eqi ldim.0 rdim.0) (eqi ldim.1 rdim.1)  then
      mapEq (mapEq eqi) ldata rdata
    else false

let _lgIMIdxInRange : LGIMatrix -> Int -> Int -> Bool
  = lam mat. lam i. lam j.
    let dim = lgIMDim mat in
    and
      (and (geqi i 0) (geqi j 0))
      (and (lti i dim.0) (lti i dim.1))

-- Returns an element in an incidence matrix.
let lgIMGet : LGIMatrix -> Int -> Int -> LGEdge
  = lam mat. lam i. lam j.
    if _lgIMIdxInRange mat i j then
      optionMapOr lgNoEdge
        (lam row. mapFindOrElse (lam. lgNoEdge) j row)
        (mapLookup i mat.0)
    else error "lgIMGet: index outside dimensions"

utest
  let mat = lgIMEmpty 2 3 in
  utest lgIMGet mat 0 0 with lgNoEdge in
  utest lgIMGet mat 0 1 with lgNoEdge in
  utest lgIMGet mat 0 2 with lgNoEdge in
  utest lgIMGet mat 1 0 with lgNoEdge in
  utest lgIMGet mat 1 1 with lgNoEdge in
  utest lgIMGet mat 1 2 with lgNoEdge in
  ()
  with ()

-- Sets an element in an incidence matrix.
let lgIMSet : LGIMatrix -> Int -> Int -> LGEdge -> LGIMatrix
  = lam mat. lam i. lam j. lam v.
    if _lgIMIdxInRange mat i j then
      (mapUpdate i
         (lam row.
           switch row
           case Some row then
             Some (mapUpdate j
                     (lam. if eqi v lgNoEdge then None () else Some v)
                     row)
           case None _ then
             if eqi v lgNoEdge then None ()
             else Some (mapFromSeq subi [(j, v)])
           end)
         mat.0,
       mat.1)
    else error "lgIMSet: index outside dimensions"

utest
  let mat = lgIMEmpty 2 3 in
  let mat = lgIMSet mat 0 0 lgInEdge in
  let mat = lgIMSet mat 0 0 lgNoEdge in
  let mat = lgIMSet mat 0 2 lgInEdge in
  let mat = lgIMSet mat 1 2 lgOutEdge in
  utest lgIMGet mat 0 0 with lgNoEdge in
  utest lgIMGet mat 0 1 with lgNoEdge in
  utest lgIMGet mat 0 2 with lgInEdge in
  utest lgIMGet mat 1 0 with lgNoEdge in
  utest lgIMGet mat 1 1 with lgNoEdge in
  utest lgIMGet mat 1 2 with lgOutEdge in
  ()
  with ()

-- Creates an incidence matrix row, where a negative number corresponds to an
-- in-edge, a positive number an out-edge, and zero no edge.
let lgIMRowOfIntSeq : [Int] -> LGIMMatrixRow
  = lam seq.
    (foldli
       (lam acc. lam j. lam e.
         if eqi e 0 then acc
         else
           mapInsert j
             (if lti e 0 then lgInEdge else lgOutEdge)
             acc)
       (mapEmpty subi)
       seq
    , length seq)

let _lgIMSeqIsRegular : all a. [[a]] -> Bool =
  lam seq.
    match seq with [] then false
    else forAll (lam row. eqi (length row) (length (head seq))) seq

-- Creates an incidence matrix from a regular seqence of sequences, where a
-- negative number corresponds to an in-edge, a positive number an out-edge, and
-- zero no edge.
let lgIMOfIntSeq : [[Int]] -> LGIMatrix
  = lam seq.
    if _lgIMSeqIsRegular seq then
      (mapFromSeq subi (mapi (lam i. lam seq. (i, (lgIMRowOfIntSeq seq).0)) seq),
       (length seq, length (head seq)))
    else error "lgIMOfSeq: Irregular sequence of sequence"

utest
  let mat = lgIMOfIntSeq [
    [0, 0, negi 2],
    [0, 0, 2]
  ] in
  utest lgIMGet mat 0 0 with lgNoEdge in
  utest lgIMGet mat 0 1 with lgNoEdge in
  utest lgIMGet mat 0 2 with lgInEdge in
  utest lgIMGet mat 1 0 with lgNoEdge in
  utest lgIMGet mat 1 1 with lgNoEdge in
  utest lgIMGet mat 1 2 with lgOutEdge in
  ()
  with ()

-- Creates a regular sequence of sequences from a incidence matrix.
let lgIMToIntSeq : LGIMatrix -> [[LGEdge]]
  = lam mat. create (mat.1).0 (lam i. create (mat.1).1 (lam j. lgIMGet mat i j))

utest
  let mat = lgIMOfIntSeq [
    [0, 0, negi 2],
    [0, 0, 2]
  ] in
  utest lgIMToIntSeq mat with [
    [lgNoEdge, lgNoEdge, lgInEdge],
    [lgNoEdge, lgNoEdge, lgOutEdge]
  ] in
  ()
  with ()

-- String representation of an incidence matrix.
let lgIMToString : LGIMatrix -> String
  = lam mat.
    strJoin "\n"
      (map
         (lam row.
           strJoin ","
             (map
                (lam e. if lti e 0 then int2string e else cons ' ' (int2string e))
                row))
         (lgIMToIntSeq mat))

-- String representation of failing utests between incidence matrices.
let lgIMUtestToString : LGIMatrix -> LGIMatrix -> String
  = utestDefaultToString lgIMToString lgIMToString

-- Returns the index and value of the first non-zero column on the i'th row.
let lgIMLeadingCol : LGIMatrix -> Int -> Option (Int, LGEdge)
  = lam mat. lam i.
    optionBind (mapLookup i mat.0) mapGetMin

utest
  let mat = lgIMOfIntSeq [
    [0, 0, negi 2],
    [0, 2, 0],
    [0, 0, 0]
  ] in
  utest lgIMLeadingCol mat 0 with Some (2, lgInEdge) in
  utest lgIMLeadingCol mat 1 with Some (1, lgOutEdge) in
  utest lgIMLeadingCol mat 2 with None () in
  ()
  with ()

-- Adds two rows, elementwise.
let lgIMAddRows : LGIMMatrixRow -> LGIMMatrixRow -> LGIMMatrixRow
  = lam lhs. lam rhs.
    match (lhs, rhs) with ((ldata, ln), (rdata, rn)) in
    if eqi ln rn then
      (mapMerge
         (lam lhs. lam rhs.
           switch (lhs, rhs)
           case (Some lhs, Some rhs) then
             let v = maxi (mini (addi lhs rhs) 1) (negi 1) in
             if eqi v 0 then None () else Some v
           case (Some e, _) | (_, Some e) then Some e
           case _ then lhs
           end)
         ldata
         rdata
      , ln)
    else error "lgIMAddRows: length missmatch"

utest
  let lhs = lgIMRowOfIntSeq [0, 1, 0, negi 1] in
  let rhs = lgIMRowOfIntSeq [0, 1, 1, 1] in
  utest lgIMAddRows lhs rhs with lgIMRowOfIntSeq [0, 1, 1, 0]
    using lgIMRowEq
  in
  let lhs = lgIMRowOfIntSeq [0, 1, 0, negi 1] in
  let rhs = lgIMRowOfIntSeq [0, 1, 1, negi 1] in
  utest lgIMAddRows lhs rhs with lgIMRowOfIntSeq [0, 1, 1, negi 1]
    using lgIMRowEq
  in
  ()
  with ()

-- Subtracts two rows, elementwise.
let lgIMSubRows : LGIMMatrixRow -> LGIMMatrixRow -> LGIMMatrixRow
  = lam lhs. lam rhs.
    match (lhs, rhs) with ((ldata, ln), (rdata, rn)) in
    if eqi ln rn then
      (mapMerge
         (lam lhs. lam rhs.
           switch (lhs, rhs)
           case (Some lhs, Some rhs) then
             let v = maxi (mini (subi lhs rhs) 1) (negi 1) in
             if eqi v 0 then None () else Some v
           case (Some e, _) then Some e
           case (_, Some e) then Some (negi e)
           case _ then lhs
           end)
         ldata
         rdata
      , ln)
    else error "lgIMSubRows: length missmatch"

utest
  let lhs = lgIMRowOfIntSeq [0, 1, 0, negi 1] in
  let rhs = lgIMRowOfIntSeq [0, 1, 1, 1] in
  utest lgIMSubRows lhs rhs with lgIMRowOfIntSeq [0, 0, negi 1, negi 1]
    using lgIMRowEq
  in
  let lhs = lgIMRowOfIntSeq [0, 1, 0, negi 1] in
  let rhs = lgIMRowOfIntSeq [0, 1, 1, negi 1] in
  utest lgIMSubRows lhs rhs with lgIMRowOfIntSeq [0, 0, negi 1, 0]
    using lgIMRowEq
  in
  ()
  with ()

-- Negates one row, elementwise.
let lgIMNegRow : LGIMMatrixRow -> LGIMMatrixRow
  = lam row.
    (mapMap
       (lam e.
         if lti e 0 then lgOutEdge
         else
           if gti e 0 then lgInEdge
           else error "lgIMNegRow: Illformed row") row.0
    , row.1)

utest
  let row = lgIMRowOfIntSeq [0, 1, 0, negi 1] in
  utest lgIMNegRow row with lgIMRowOfIntSeq [0, negi 1, 0, 1]
    using lgIMRowEq
  in
  let row = lgIMRowOfIntSeq [0, 1, 1, negi 1] in
  utest lgIMNegRow row with lgIMRowOfIntSeq [0, negi 1, negi 1, 1]
    using lgIMRowEq
  in
  ()
  with ()

-- Row-reduces an incidence matrix and trims zero-rows.
let lgIMRowReduce : LGIMatrix -> LGIMatrix
  = lam mat.
    match mat with (data, dim) in
    recursive let recur = lam i. lam j. lam data.
      if or (geqi i dim.0) (geqi j dim.1) then data
      else
        if mapMem i data then
          -- Finds pivot row candidates
          recursive let findPivotRows = lam acc. lam i.
            if geqi i dim.0 then acc
            else
              optionMapOrElse
                (lam. findPivotRows acc (succ i))
                (lam row.
                  optionMapOrElse
                    (lam. findPivotRows acc (succ i))
                    (lam c.
                      match c with (k, v) in
                      let acc =
                        if eqi k j then
                          -- Has a leading non-zero element on the j'th column
                          snoc acc ((i, v), (row, dim.0))
                        else
                          if lti k j then
                            -- The algorithm should not reach this state.
                            error "impossible"
                          else acc
                      in
                      findPivotRows acc (succ i))
                    (mapGetMin row))
                (mapLookup i data)
          in
          -- find pivot rows
          switch findPivotRows [] i
          case [] then
            -- The remining elements in this column are already zero, move to
            -- the next column.
            recur i (succ j) data
          case [((k, v), pivotRow)] ++ rows then
            -- Change sign if leading element is negative
            let pivotRow =
              if lti v 0 then lgIMNegRow pivotRow else pivotRow
            in
            -- Pivot
            let tmpRow = mapFindExn i data in
            let data = mapInsert i pivotRow.0 (mapInsert k tmpRow data) in
            -- zero all elements in the j'th column, below the i'th row
            let data =
              foldl
                (lam data. lam row.
                  match row with ((k, v), row) in
                  let row =
                    if lti v 0 then lgIMAddRows row pivotRow
                    else
                      if gti v 0 then lgIMSubRows row pivotRow
                      else
                        -- The algorithm should not reach this state.
                        error "impossible"
                  in
                  mapInsert k row.0 data)
                data
                rows
            in
            recur (succ i) (succ j) data
          end
        else recur (succ i) (succ j) data
    in
    let data = recur 0 0 data in
    -- trim zero rows
    match dim with (_, m) in
    let rows = filter (lam row. not (mapIsEmpty row)) (mapValues data) in
    let n = length rows in
    let data = mapFromSeq subi (mapi (lam i. lam row. (i, row)) rows) in
    (data, (n, m))

utest
  -- Example from A general vector-network formulation for dynamic systems with
  -- kinematic constraints and/or McPhee, J.J.
  let mat = lgIMOfIntSeq [
    [negi 1, 0, 0, 0, 0, 0, 1],
    [1, 0, 1, 1, 0, 0, 0],
    [0, 0, negi 1, 0, 0, negi 1, 0],
    [0, 1, 0, 0, 0, 1, 0],
    [0, negi 1, 0, negi 1, 1, 0, 0],
    [0, 0, 0, 0, negi 1, 0, negi 1]
  ] in
  let expected = lgIMOfIntSeq [
    [1, 0, 0, 0, 0, 0, negi 1],
    [0, 1, 0, 0, 0, 1, 0],
    [0, 0, 1, 0, 0, 1, 0],
    [0, 0, 0, 1, 0, negi 1, 1],
    [0, 0, 0, 0, 1, 0, 1]
  ] in
  utest lgIMRowReduce mat with expected using lgIMEq else lgIMUtestToString in
  -- Permutation of the above columns so that the leading columns does not form
  -- a tree.
  let mat = lgIMOfIntSeq [
    [negi 1, 0, 0, 0, 0, 0, 1],
    [1, 0, 1, 1, 0, 0, 0],
    [0, 0, negi 1, 0, negi 1, 0, 0],
    [0, 1, 0, 0, 1, 0, 0],
    [0, negi 1, 0, negi 1, 0, 1, 0],
    [0, 0, 0, 0, 0, negi 1, negi 1]
  ] in
  let expected = lgIMOfIntSeq [
    [1, 0, 0, 0, 0, 0, negi 1],
    [0, 1, 0, 0, 1, 0, 0],
    [0, 0, 1, 0, 1, 0, 0],
    [0, 0, 0, 1, negi 1, 0, 1],
    [0, 0, 0, 0, 0, 1, 1]
  ] in
  utest lgIMRowReduce mat with expected using lgIMEq else lgIMUtestToString in
  ()
  with ()

-- Retrieves the fundamental cutset and fundamenta circuit det from an incidence
-- matrix.
let lgFCutCircSet
  : all a. LGIMatrix -> [a] -> ([[LGEdge]], [[LGEdge]], [a])
  = lam mat. lam columnLabels.
    match mat with (_, (n, m)) in
    if eqi (length columnLabels) m then
      match lgIMRowReduce mat with mat & (_, (n, m)) in
      let columnLabelsMap =
        mapFromSeq subi (mapi (lam i. lam l. (i, l)) columnLabels)
      in
      -- Ensures that the leading columns forms a tree by permutating columns.
      recursive let findTree = lam acc. lam i.
        match acc with (mat, columnLabels) in
        match lgIMDim mat with (n, m) in
        if geqi i n then acc
        else
          match lgIMLeadingCol mat i with Some (j, _) then
            if eqi i j then findTree acc (succ i) -- All good, continue
            else
              -- We need to swap theese columns, before we continue
              recursive let swap = lam acc. lam k.
                match acc with (mat, columnLabelsMap) in
                if geqi k n then acc
                else
                  let tmp = lgIMGet mat k j in
                  let mat =
                    lgIMSet (lgIMSet mat k j (lgIMGet mat k i)) k i tmp
                  in
                  let tmp = mapFindExn j columnLabelsMap in
                  let columnLabelsMap =
                    mapInsert i tmp
                      (mapInsert j (mapFindExn i columnLabelsMap)
                         columnLabelsMap)
                  in
                  swap (mat, columnLabelsMap) (succ k)
              in
              swap (mat, columnLabelsMap) 0
          else
            -- We assume that there are no zero rows in the input matrix
            error "impossible"
      in
      match findTree (mat, columnLabelsMap) 0 with (mat, columnLabelsMap) in
      -- Retrieve the permuted column labels
      let columnLabels = mapValues columnLabelsMap in
      let cut =
        create n (lam i. create (subi m n) (lam j. lgIMGet mat i (addi j n)))
      in
      let circ =
        create (subi m n) (lam j. create n (lam i. negi (lgIMGet mat i (addi j n))))
      in
      (cut, circ, columnLabels)
    else
      error "lgFCutCircSet: missmatch between column labels and matrix columns"

utest
  let mat = lgIMOfIntSeq [
    [negi 1, 0, 0, 0, 0, 0, 1],
    [1, 0, 1, 1, 0, 0, 0],
    [0, 0, negi 1, 0, 0, negi 1, 0],
    [0, 1, 0, 0, 0, 1, 0],
    [0, negi 1, 0, negi 1, 1, 0, 0],
    [0, 0, 0, 0, negi 1, 0, negi 1]
  ] in
  utest lgFCutCircSet mat ["L1", "L2", "R3", "R4", "R5", "C6", "V7"] with ([
    [0, negi 1],
    [1, 0],
    [1, 0],
    [negi 1, 1],
    [0, 1]
  ], [
    [0, negi 1, negi 1, 1, 0],
    [1, 0, 0, negi 1, negi 1]
  ], [
    "L1", "L2", "R3", "R4", "R5", "C6", "V7"
  ]) in
  let mat = lgIMOfIntSeq [
    [negi 1, 0, 0, 0, 0, 0, 1],
    [1, 0, 1, 1, 0, 0, 0],
    [0, 0, negi 1, 0, negi 1, 0, 0],
    [0, 1, 0, 0, 1, 0, 0],
    [0, negi 1, 0, negi 1, 0, 1, 0],
    [0, 0, 0, 0, 0, negi 1, negi 1]
  ] in
  utest lgFCutCircSet mat ["L1", "L2", "R3", "R4", "C6", "R5", "V7"] with ([
    [0, negi 1],
    [1, 0],
    [1, 0],
    [negi 1, 1],
    [0, 1]
  ], [
    [0, negi 1, negi 1, 1, 0],
    [1, 0, 0, negi 1, negi 1]
  ], [
    "L1", "L2", "R3", "R4", "R5", "C6", "V7"
  ]) in
  ()
  with ()
