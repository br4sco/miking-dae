mexpr
external externalExp : Float -> Float
in
let exp =
  lam x16: Float.
    externalExp
      x16
in
external externalSin : Float -> Float
in
let sin =
  lam x15: Float.
    externalSin
      x15
in
external externalCos : Float -> Float
in
let cos =
  lam x14: Float.
    externalCos
      x14
in
external externalPow : Float -> Float -> Float
in
let pow =
  lam x10: Float.
    lam y12: Float.
      externalPow
        x10
        y12
in
external externalSqrt : Float -> Float
in
let sqrt: Float -> Float =
  lam x9.
    externalSqrt
      x9
in
let and: Bool -> Bool -> Bool =
  lam a48.
    lam b6.
      match
        a48
      with
        true
      then
        b6
      else
        false
in
let maxi =
  lam r5.
    lam l.
      match
        gti
          r5
          l
      with
        true
      then
        r5
      else
        l
in
let succ =
  lam x8.
    addi
      x8
      1
in
type Option a
in
con Some: all a1. a1 -> Option a1 in
con None: all a2. () -> Option a2 in
let make: all a47. Int -> a47 -> [a47] =
  lam n14.
    lam v8.
      create
        n14
        (lam #var"84".
           v8)
in
let eqSeq: all a46. all b5. (a46 -> b5 -> Bool) -> [a46] -> [b5] -> Bool =
  lam eq1.
    lam s112.
      lam s26.
        recursive
          let work1 =
            lam s113.
              lam s27.
                match
                  (s113, s27)
                with
                  ([ h1 ] ++ t110 ++ "", [ h2 ] ++ t25 ++ "")
                then
                  match
                    eq1
                      h1
                      h2
                  with
                    true
                  then
                    work1
                      t110
                      t25
                  else
                    false
                else
                  true
        in
        let n13 =
          length
            s112
        in
        let n21 =
          length
            s26
        in
        let ndiff =
          subi
            n13
            n21
        in
        match
          eqi
            ndiff
            0
        with
          true
        then
          work1
            s112
            s26
        else
          false
in
recursive
  let foldl2: all a45. all b4. all c4. (a45 -> b4 -> c4 -> a45) -> a45 -> [b4] -> [c4] -> a45 =
    lam f14.
      lam acc9.
        lam seq11.
          lam seq21.
            let g1 =
              lam acc12: (a45, [b4]).
                lam x23.
                  match
                    acc12
                  with
                    (acc13, [ x13 ] ++ xs11 ++ "")
                  in
                  (f14
                      acc13
                      x13
                      x23, xs11)
            in
            match
              geqi
                (length
                   seq11)
                (length
                   seq21)
            with
              true
            then
              match
                foldl
                  g1
                  (acc9, seq11)
                  seq21
              with
                (acc10, _)
              in
              acc10
            else
              foldl2
                (lam acc11.
                   lam x12.
                     lam x22.
                       f14
                         acc11
                         x22
                         x12)
                acc9
                seq21
                seq11
in
let zipWith: all a44. all b3. all c3. (a44 -> b3 -> c3) -> [a44] -> [b3] -> [c3] =
  lam f13.
    foldl2
      (lam acc8.
         lam x11.
           lam x21.
             snoc
               acc8
               (f13
                  x11
                  x21))
      ""
in
recursive
  let any: all a43. (a43 -> Bool) -> [a43] -> Bool =
    lam p14.
      lam seq3.
        match
          null
            seq3
        with
          true
        then
          false
        else
          match
            p14
              (head
                 seq3)
          with
            true
          then
            true
          else
            any
              p14
              (tail
                 seq3)
in
recursive
  let forAll: all a42. (a42 -> Bool) -> [a42] -> Bool =
    lam p13.
      lam seq2.
        match
          null
            seq2
        with
          true
        then
          true
        else
          match
            p13
              (head
                 seq2)
          with
            true
          then
            forAll
              p13
              (tail
                 seq2)
          else
            false
in
let join: all a41. [[a41]] -> [a41] =
  lam seqs.
    foldl
      concat
      ""
      seqs
in
recursive
  let find: all a40. (a40 -> Bool) -> [a40] -> Option a40 =
    lam p12.
      lam seq1.
        match
          null
            seq1
        with
          true
        then
          None
            {}
        else
          match
            p12
              (head
                 seq1)
          with
            true
          then
            Some
              (head
                 seq1)
          else
            find
              p12
              (tail
                 seq1)
in
recursive
  let isPrefix: all a39. all b2. (a39 -> b2 -> Bool) -> [a39] -> [b2] -> Bool =
    lam eq.
      lam s111.
        lam s25.
          match
            null
              s111
          with
            true
          then
            true
          else
            match
              null
                s25
            with
              true
            then
              false
            else
              and
                (eq
                   (head
                      s111)
                   (head
                      s25))
                (isPrefix
                   eq
                   (tail
                      s111)
                   (tail
                      s25))
in
recursive
  let seqJoin: all a38. [a38] -> [[a38]] -> [a38] =
    lam delim.
      lam ss.
        match
          null
            ss
        with
          true
        then
          ""
        else
          match
            eqi
              (length
                 ss)
              1
          with
            true
          then
            head
              ss
          else
            concat
              (concat
                 (head
                    ss)
                 delim)
              (seqJoin
                 delim
                 (tail
                    ss))
in
let eqChar =
  lam c11.
    lam c2.
      eqc
        c11
        c2
in
let isDigit =
  lam c1.
    let i16 =
      char2int
        c1
    in
    match
      leqi
        (char2int
           '0')
        i16
    with
      true
    then
      leqi
        i16
        (char2int
           '9')
    else
      false
in
let eqString =
  lam s110.
    lam s24.
      eqSeq
        eqc
        s110
        s24
in
let string2int =
  lam s18.
    recursive
      let string2int_rechelper =
        lam s19.
          lam acc7.
            match
              null
                s19
            with
              true
            then
              acc7
            else
              let fsd =
                subi
                  (char2int
                     (head
                        s19))
                  (char2int
                     '0')
              in
              string2int_rechelper
                (tail
                   s19)
                (addi
                   (muli
                      10
                      acc7)
                   fsd)
    in
    match
      s18
    with
      ""
    then
      0
    else
      match
        eqChar
          '-'
          (head
             s18)
      with
        true
      then
        negi
          (string2int_rechelper
             (tail
                s18)
             0)
      else
        string2int_rechelper
          s18
          0
in
let digit2char =
  lam d.
    int2char
      (addi
         d
         (char2int
            '0'))
in
let int2string =
  lam n11.
    recursive
      let int2string_rechelper =
        lam n12.
          lam acc6.
            match
              lti
                n12
                10
            with
              true
            then
              cons
                (digit2char
                   n12)
                acc6
            else
              int2string_rechelper
                (divi
                   n12
                   10)
                (cons
                   (digit2char
                      (modi
                         n12
                         10))
                   acc6)
    in
    match
      lti
        n11
        0
    with
      true
    then
      cons
        '-'
        (int2string_rechelper
           (negi
              n11)
           "")
    else
      int2string_rechelper
        n11
        ""
in
let stringIsInt =
  lam s16.
    match
      null
        s16
    with
      true
    then
      false
    else
      let s17 =
        match
          eqChar
            (get
               s16
               0)
            '-'
        with
          true
        then
          tail
            s16
        else
          s16
      in
      forAll
        isDigit
        s17
in
let strJoin: [Char] -> [[Char]] -> [Char] =
  seqJoin
in
let _prod =
  foldl
    muli
    1
in
type TCreate a3 =
  [Int] -> ([Int] -> a3) -> Tensor[a3]
in
let tensorHasSameShape: all a37. all b1. Tensor[a37] -> Tensor[b1] -> Bool =
  lam t19.
    lam t24.
      eqSeq
        eqi
        (tensorShape
           t19)
        (tensorShape
           t24)
in
let tensorCreate =
  tensorCreateDense
in
let tensorOfSeqOrElse: all a36. (() -> Tensor[a36]) -> ([Int] -> ([Int] -> a36) -> Tensor[a36]) -> [Int] -> [a36] -> Tensor[a36] =
  lam f12.
    lam tcreate.
      lam shape.
        lam seq.
          let n10 =
            length
              seq
          in
          match
            neqi
              n10
              (_prod
                 shape)
          with
            true
          then
            f12
              {}
          else
            let t18 =
              tcreate
                [ n10 ]
                (lam idx1.
                   get
                     seq
                     (get
                        idx1
                        0))
            in
            tensorReshapeExn
              t18
              shape
in
let tensorOfSeqExn: all a35. ([Int] -> ([Int] -> a35) -> Tensor[a35]) -> [Int] -> [a35] -> Tensor[a35] =
  lam x7.
    tensorOfSeqOrElse
      (lam #var"83".
         error
           "Empty seq in tensorOfSeqExn")
      x7
in
let tensorSize: all a34. Tensor[a34] -> Int =
  lam t17.
    _prod
      (tensorShape
         t17)
in
let tensorMapOrElse: all a33. all b. (() -> ()) -> (a33 -> b -> b) -> Tensor[a33] -> Tensor[b] -> () =
  lam f11.
    lam g.
      lam t16.
        lam t23.
          match
            tensorHasSameShape
              t16
              t23
          with
            true
          then
            let n9 =
              tensorSize
                t16
            in
            let v11 =
              tensorReshapeExn
                t16
                [ n9 ]
            in
            let v21 =
              tensorReshapeExn
                t23
                [ n9 ]
            in
            tensorIterSlice
              (lam i15.
                 lam e2.
                   tensorSetExn
                     v21
                     [ i15 ]
                     (g
                        (tensorGetExn
                           e2
                           "")
                        (tensorGetExn
                           v21
                           [ i15 ])))
              v11
          else
            f11
              {}
in
let tensorMapExn =
  lam x6.
    tensorMapOrElse
      (lam #var"82".
         error
           "Tensor shape mismatch in tensorMap")
      x6
in
let tensorBlitExn: all a32. Tensor[a32] -> Tensor[a32] -> () =
  lam t15.
    lam t22.
      match
        tensorHasSameShape
          t15
          t22
      with
        true
      then
        tensorMapExn
          (lam x5.
             lam #var"81".
               x5)
          t15
          t22
      else
        error
          "Invalid Argument: tensor.tensorBlitExn"
in
let test =
  let t14 =
    tensorOfSeqExn
      tensorCreateDense
      [ 3 ]
      [ 1,
        2,
        3 ]
  in
  let t21 =
    tensorCreateDense
      [ 3 ]
      (lam #var"80".
         0)
  in
  utest let #var"79" =
    tensorBlitExn
      t14
      t21
  in
  t21
  with t14
  using tensorEq
    eqi
  in
  {}
in
type NvectorSerial
in
type SundialsMatrixDense
in
type SundialsRealArray
in
type SundialsNonlinearSolver
in
external nvectorSerialWrap : Tensor[Float] -> NvectorSerial
in
let nvectorSerialWrap1 =
  lam t13.
    nvectorSerialWrap
      t13
in
external sundialsMatrixDense : Int -> SundialsMatrixDense
in
let sundialsMatrixDense1 =
  lam n8.
    sundialsMatrixDense
      n8
in
external sundialsMatrixDenseSet! : SundialsMatrixDense -> Int -> Int -> Float -> ()
in
let sundialsMatrixDenseSet1 =
  lam m4.
    lam i14.
      lam j3.
        lam v7.
          sundialsMatrixDenseSet
            m4
            i14
            j3
            v7
in
external sundialsMatrixDenseUpdate! : SundialsMatrixDense -> Int -> Int -> (Float -> Float) -> ()
in
let sundialsMatrixDenseUpdate1 =
  lam m3.
    lam i13.
      lam j2.
        lam f10.
          sundialsMatrixDenseUpdate
            m3
            i13
            j2
            f10
in
external sundialsNonlinearSolverNewtonMake : NvectorSerial -> SundialsNonlinearSolver
in
let sundialsNonlinearSolverNewtonMake1 =
  lam y11.
    sundialsNonlinearSolverNewtonMake
      y11
in
type IdaDlsDense
in
type IdaDlsSolver
in
type IdaDlsSolverSession
in
type IdaTolerance
in
type IdaSession
in
type IdaSolverResultInternal
in
type IdaCalcICResult
in
con IdaCalcICOK: () -> IdaCalcICResult in
con IdaVarIdNotSet: () -> IdaCalcICResult in
con IdaCalcICError: () -> IdaCalcICResult in
let _idaCalcICRetCodeToResult =
  lam rc2.
    let #var"X6" =
      rc2
    in
    match
      #var"X6"
    with
      0
    then
      IdaCalcICOK
        {}
    else
      match
        #var"X6"
      with
        1
      then
        IdaVarIdNotSet
          {}
      else
        match
          #var"X6"
        with
          _
        in
        IdaCalcICError
            {}
in
type IdaSolveError
in
con IdaIllInput: () -> IdaSolveError in
con IdaTooMuchWork: () -> IdaSolveError in
con IdaTooMuchAccuracy: () -> IdaSolveError in
con IdaErrFailure: () -> IdaSolveError in
con IdaConvergenceFailure: () -> IdaSolveError in
con IdaLinearInitFailure: () -> IdaSolveError in
con IdaLinearSetupFailure: () -> IdaSolveError in
con IdaLinearSolveFailure: () -> IdaSolveError in
con IdaConstraintFailure: () -> IdaSolveError in
con IdaFirstResFuncFailure: () -> IdaSolveError in
con IdaRepeatedResFuncFailure: () -> IdaSolveError in
con IdaResFuncFailure: () -> IdaSolveError in
con IdaRootFuncFailure: () -> IdaSolveError in
con IdaErrUnspecified: () -> IdaSolveError in
let _idaErrorCodeToError: Int -> IdaSolveError =
  lam ec1.
    let #var"X5" =
      ec1
    in
    match
      #var"X5"
    with
      0
    then
      IdaIllInput
        {}
    else
      match
        #var"X5"
      with
        1
      then
        IdaTooMuchWork
          {}
      else
        match
          #var"X5"
        with
          2
        then
          IdaTooMuchAccuracy
            {}
        else
          match
            #var"X5"
          with
            3
          then
            IdaErrFailure
              {}
          else
            match
              #var"X5"
            with
              4
            then
              IdaConvergenceFailure
                {}
            else
              match
                #var"X5"
              with
                5
              then
                IdaLinearInitFailure
                  {}
              else
                match
                  #var"X5"
                with
                  6
                then
                  IdaLinearSetupFailure
                    {}
                else
                  match
                    #var"X5"
                  with
                    7
                  then
                    IdaLinearSolveFailure
                      {}
                  else
                    match
                      #var"X5"
                    with
                      8
                    then
                      IdaConstraintFailure
                        {}
                    else
                      match
                        #var"X5"
                      with
                        9
                      then
                        IdaFirstResFuncFailure
                          {}
                      else
                        match
                          #var"X5"
                        with
                          10
                        then
                          IdaRepeatedResFuncFailure
                            {}
                        else
                          match
                            #var"X5"
                          with
                            11
                          then
                            IdaResFuncFailure
                              {}
                          else
                            match
                              #var"X5"
                            with
                              12
                            then
                              IdaRootFuncFailure
                                {}
                            else
                              match
                                #var"X5"
                              with
                                _
                              in
                              IdaErrUnspecified
                                  {}
in
type IdaSolverResult
in
con IdaSuccess: () -> IdaSolverResult in
con IdaStopTimeReached: () -> IdaSolverResult in
con IdaRootsFound: () -> IdaSolverResult in
con IdaSolveError1: IdaSolveError -> IdaSolverResult in
let _idaSolverCodeToSolverResult: (Int, Int) -> IdaSolverResult =
  lam rc1.
    let #var"X4" =
      rc1
    in
    match
      #var"X4"
    with
      (0, _)
    then
      IdaSuccess
        {}
    else
      match
        #var"X4"
      with
        (1, _)
      then
        IdaRootsFound
          {}
      else
        match
          #var"X4"
        with
          (2, _)
        then
          IdaStopTimeReached
            {}
        else
          match
            #var"X4"
          with
            (3, ec)
          in
          IdaSolveError1
              (_idaErrorCodeToError
                 ec)
in
type IdaJacArgs =
  {c: Float, t: Float, y: Tensor[Float], yp: Tensor[Float], res: Tensor[Float], tmp: (Tensor[Float], Tensor[Float], Tensor[Float])}
in
type IdaJacFn =
  IdaJacArgs -> SundialsMatrixDense -> ()
in
type IdaResFn =
  Float -> Tensor[Float] -> Tensor[Float] -> Tensor[Float] -> ()
in
type IdaRootFn =
  IdaResFn
in
let idaNoRoots =
  (0, lam #var"75".
    lam #var"76".
      lam #var"77".
        lam #var"78".
          error
            "not implemented")
in
external idaDlsDense! : NvectorSerial -> SundialsMatrixDense -> IdaDlsDense
in
let idaDlsDense1 =
  lam y10.
    lam m2.
      idaDlsDense
        y10
        m2
in
external idaDlsSolver! : IdaDlsDense -> IdaDlsSolverSession
in
let idaDlsSolver1 =
  lam solver1.
    idaDlsSolver
      solver1
in
external idaDlsSolverJacf! : IdaJacFn -> IdaDlsDense -> IdaDlsSolverSession
in
let idaDlsSolverJacf1 =
  lam jacf1.
    lam solver.
      idaDlsSolverJacf
        jacf1
        solver
in
external idaVarIdAlgebraic : Float
in
external idaVarIdDifferential : Float
in
external idaSSTolerances : Float -> Float -> IdaTolerance
in
let idaSSTolerances1 =
  lam rtol.
    lam atol.
      idaSSTolerances
        rtol
        atol
in
external idaInit! : IdaTolerance -> SundialsNonlinearSolver -> IdaDlsSolverSession -> IdaResFn -> NvectorSerial -> (Int, IdaRootFn) -> Float -> NvectorSerial -> NvectorSerial -> IdaSession
in
type IdaInitArg =
  {t: Float, y: NvectorSerial, yp: NvectorSerial, tol: IdaTolerance, resf: IdaResFn, roots: (Int, IdaRootFn), varid: NvectorSerial, lsolver: IdaDlsSolverSession, nlsolver: SundialsNonlinearSolver}
in
let idaInit1 =
  lam arg2: IdaInitArg.
    match
      arg2
    with
      {t = t12, y = y9, yp = yp6, tol = tol1, resf = resf4, roots = roots, varid = varid1, lsolver = lsolver1, nlsolver = nlsolver1}
    in
    idaInit
        tol1
        nlsolver1
        lsolver1
        resf4
        varid1
        roots
        t12
        y9
        yp6
in
external idaCalcICYaYd! : IdaSession -> Float -> NvectorSerial -> NvectorSerial -> Int
in
type IdaCalcICYaYdArg =
  {y: NvectorSerial, yp: NvectorSerial, tend: Float}
in
let idaCalcICYaYd1: IdaSession -> IdaCalcICYaYdArg -> IdaCalcICResult =
  lam s15.
    lam arg1: IdaCalcICYaYdArg.
      _idaCalcICRetCodeToResult
        (idaCalcICYaYd
           s15
           arg1.tend
           arg1.y
           arg1.yp)
in
external idaSolveNormal : IdaSession -> Float -> NvectorSerial -> NvectorSerial -> (Float, (Int, Int))
in
type IdaSolveNormalArg =
  {y: NvectorSerial, yp: NvectorSerial, tend: Float}
in
let idaSolveNormal1: IdaSession -> IdaSolveNormalArg -> (Float, IdaSolverResult) =
  lam s14.
    lam arg.
      match
        idaSolveNormal
          s14
          arg.tend
          arg.y
          arg.yp
      with
        (tend4, rc)
      in
      (tend4, _idaSolverCodeToSolverResult
          rc)
in
type IdaReinitArg =
  {t: Float, y: NvectorSerial, yp: NvectorSerial, roots: (Int, IdaRootFn)}
in
external idaSetStopTime : IdaSession -> Float -> ()
in
let idaSetStopTime1 =
  lam s13.
    lam tend3.
      idaSetStopTime
        s13
        tend3
in
type ArgResult a4 =
  {options: a4, strings: [[Char]]}
in
type ParseType
in
con ParseTypeInt: [Char] -> ParseType in
con ParseTypeIntMin: ([Char], Int) -> ParseType in
con ParseTypeFloat: [Char] -> ParseType in
con ParseTypeFloatMin: ([Char], Float) -> ParseType in
con ParseTypeFloatInterval: ([Char], Float, Float) -> ParseType in
con ParseTypeGeneric: ([Char], [Char]) -> ParseType in
type ArgPart a5 =
  {str: [Char], fail: Ref (Option ParseType), options: a5}
in
type ParseOption =
  ([Char], [Char], [Char])
in
type ParseConfig a6 =
  [([ParseOption], [Char], ArgPart a6 -> a6)]
in
type ParseResult a7
in
con ParseOK: all a8. ArgResult a8 -> ParseResult a8 in
con ParseFailUnknownOption: all a9. [Char] -> ParseResult a9 in
con ParseFailMissingOpArg: all a10. [Char] -> ParseResult a10 in
con ParseFailConversion: all a11. (ParseType, [Char]) -> ParseResult a11 in
let stringLineFormat =
  lam s9.
    lam width.
      lam indent.
        lam startPos.
          recursive
            let next =
              lam s10.
                lam acc1.
                  lam w.
                    lam pos.
                      lam space.
                        let pos1 =
                          addi
                            (length
                               w)
                            pos
                        in
                        match
                          leqi
                            pos1
                            width
                        with
                          true
                        then
                          let pos2 =
                            addi
                              pos1
                              (length
                                 space)
                          in
                          let acc2 =
                            concat
                              acc1
                              w
                          in
                          let acc3 =
                            match
                              leqi
                                pos2
                                width
                            with
                              true
                            then
                              concat
                                acc2
                                space
                            else
                              acc2
                          in
                          work
                            s10
                            acc3
                            ""
                            pos2
                        else
                          let acc4 =
                            concat
                              acc1
                              (cons
                                 '\n'
                                 (make
                                    indent
                                    ' '))
                          in
                          let w1 =
                            concat
                              w
                              space
                          in
                          work
                            s10
                            (concat
                               acc4
                               w1)
                            ""
                            (addi
                               indent
                               (length
                                  w1))
            let work =
              lam s12.
                lam acc5.
                  lam w2.
                    lam pos3.
                      match
                        s12
                      with
                        [ c ] ++ rest1 ++ ""
                      then
                        match
                          eqChar
                            c
                            ' '
                        with
                          true
                        then
                          next
                            rest1
                            acc5
                            w2
                            pos3
                            " "
                        else
                          work
                            rest1
                            acc5
                            (snoc
                               w2
                               c)
                            pos3
                      else
                        match
                          eqi
                            (length
                               w2)
                            0
                        with
                          true
                        then
                          acc5
                        else
                          next
                            s12
                            acc5
                            w2
                            pos3
                            ""
          in
          work
            s9
            ""
            ""
            startPos
in
type Options_argHelpOptions =
  {indent: Int, lineWidth: Int, spaceToText: Int}
in
let argHelpOptions_defaults =
  { indent =
      2,
    lineWidth =
      80,
    spaceToText =
      2 }
in
let argHelpOptions_general: all a31. Options_argHelpOptions -> ParseConfig a31 -> [Char] =
  lam options1.
    lam opConfig1.
      let opStrings =
        map
          (lam e1.
             match
               e1
             with
               (lst, _, _)
             in
             let s22 =
                 map
                   (lam triple.
                      match
                        triple
                      with
                        (s11, s23, s31)
                      in
                      join
                          [ s11,
                            s23,
                            s31 ])
                   lst
               in
               strJoin
                 ", "
                 s22)
          opConfig1
      in
      let maxLen =
        foldl
          (lam acc.
             lam x4.
               maxi
                 (length
                    x4)
                 acc)
          0
          opStrings
      in
      let opDesc =
        map
          (lam e.
             match
               e
             with
               (_, s8, _)
             in
             s8)
          opConfig1
      in
      let f9 =
        lam x2.
          lam desc.
            let start =
              join
                [ make
                    options1.indent
                    ' ',
                  x2,
                  make
                    (addi
                       (subi
                          maxLen
                          (length
                             x2))
                       options1.spaceToText)
                    ' ' ]
            in
            let before =
              addi
                (addi
                   maxLen
                   options1.indent)
                options1.spaceToText
            in
            let x3 =
              concat
                start
                (stringLineFormat
                   desc
                   options1.lineWidth
                   before
                   before)
            in
            x3
      in
      strJoin
        "\n"
        (zipWith
           f9
           opStrings
           opDesc)
in
let argHelpOptions =
  lam opConfig.
    argHelpOptions_general
      argHelpOptions_defaults
      opConfig
in
let argToInt: all a30. ArgPart a30 -> Int =
  lam p11.
    match
      stringIsInt
        p11.str
    with
      true
    then
      string2int
        p11.str
    else
      let #var"74" =
        modref
          p11.fail
          (Some
             (ParseTypeInt
                p11.str))
      in
      0
in
let argToFloat: all a29. ArgPart a29 -> Float =
  lam p10.
    match
      stringIsFloat
        p10.str
    with
      true
    then
      string2float
        p10.str
    else
      let #var"73" =
        modref
          p10.fail
          (Some
             (ParseTypeFloat
                p10.str))
      in
      0.
in
let argToIntMin: all a28. ArgPart a28 -> Int -> Int =
  lam p9.
    lam minVal4.
      let v6 =
        argToInt
          p9
      in
      match
        deref
          p9.fail
      with
        None {}
      then
        match
          lti
            v6
            minVal4
        with
          true
        then
          let #var"72" =
            modref
              p9.fail
              (Some
                 (ParseTypeIntMin
                    (p9.str, minVal4)))
          in
          v6
        else
          v6
      else
        v6
in
let argToFloatMin: all a27. ArgPart a27 -> Float -> Float =
  lam p8.
    lam minVal3.
      let v5 =
        argToFloat
          p8
      in
      match
        deref
          p8.fail
      with
        None {}
      then
        match
          ltf
            v5
            minVal3
        with
          true
        then
          let #var"71" =
            modref
              p8.fail
              (Some
                 (ParseTypeFloatMin
                    (p8.str, minVal3)))
          in
          v5
        else
          v5
      else
        v5
in
type Options_argParse =
  {args: [[Char]], optionsStartWith: [[Char]]}
in
let argParse_defaults =
  { args =
      tail
        argv,
    optionsStartWith =
      [ "-" ] }
in
let argParse_general: all a26. Options_argParse -> a26 -> ParseConfig a26 -> ParseResult a26 =
  lam options: Options_argParse.
    lam argParseDefaults.
      lam argParseConfig.
        recursive
          let matchOption =
            lam str.
              lam confLst: ParseConfig a26.
                match
                  confLst
                with
                  [ (opLst, _, f6) ] ++ rest ++ ""
                then
                  match
                    find
                      (lam x.
                         match
                           x
                         with
                           (s4, _, _)
                         in
                         isPrefix
                             eqChar
                             s4
                             str)
                      opLst
                  with
                    Some (s5, sep, _)
                  then
                    Some
                      (s5, sep, f6)
                  else
                    matchOption
                      str
                      rest
                else
                  None
                    {}
          let handleOptionParsing =
            lam f7.
              lam o.
                lam opstr.
                  lam s6.
                    let failCode =
                      ref
                        (None
                           {})
                    in
                    let argOptions =
                      f7
                        { options =
                            o,
                          str =
                            s6,
                          fail =
                            failCode }
                    in
                    match
                      deref
                        failCode
                    with
                      Some pType
                    then
                      (Some
                        (ParseFailConversion
                           (pType, opstr)), argOptions)
                    else
                      (None
                        {}, argOptions)
          let argMain =
            lam argOptions1.
              lam strings.
                lam args.
                  match
                    args
                  with
                    [ s7 ] ++ xs ++ ""
                  then
                    match
                      matchOption
                        s7
                        argParseConfig
                    with
                      Some (op, sep1, f8)
                    then
                      match
                        eqi
                          (length
                             sep1)
                          0
                      with
                        true
                      then
                        match
                          eqString
                            s7
                            op
                        with
                          true
                        then
                          let parse =
                            handleOptionParsing
                              f8
                              argOptions1
                              ""
                              s7
                          in
                          match
                            parse
                          with
                            (Some ret, _)
                          then
                            ret
                          else
                            match
                              parse
                            with
                              (None {}, argOptions2)
                            in
                            argMain
                                argOptions2
                                strings
                                xs
                        else
                          ParseFailUnknownOption
                            s7
                      else
                        match
                          eqString
                            s7
                            op
                        with
                          true
                        then
                          match
                            xs
                          with
                            [ s21 ] ++ xs1 ++ ""
                          then
                            match
                              matchOption
                                s21
                                argParseConfig
                            with
                              Some _
                            then
                              ParseFailMissingOpArg
                                s7
                            else
                              let parse1 =
                                handleOptionParsing
                                  f8
                                  argOptions1
                                  s7
                                  s21
                              in
                              match
                                parse1
                              with
                                (Some ret1, _)
                              then
                                ret1
                              else
                                match
                                  parse1
                                with
                                  (None {}, argOptions3)
                                in
                                argMain
                                    argOptions3
                                    strings
                                    xs1
                          else
                            ParseFailMissingOpArg
                              s7
                        else
                          ParseFailUnknownOption
                            s7
                    else
                      match
                        any
                          (lam x1.
                             isPrefix
                               eqChar
                               x1
                               s7)
                          options.optionsStartWith
                      with
                        true
                      then
                        ParseFailUnknownOption
                          s7
                      else
                        argMain
                          argOptions1
                          (snoc
                             strings
                             s7)
                          xs
                  else
                    ParseOK
                      { options =
                          argOptions1,
                        strings =
                          strings }
        in
        argMain
          argParseDefaults
          ""
          options.args
in
let argParse =
  argParse_general
    argParse_defaults
in
let argPrintErrorString =
  lam result2.
    let #var"X2" =
      result2
    in
    match
      #var"X2"
    with
      ParseOK _
    then
      "Parse OK."
    else
      match
        #var"X2"
      with
        ParseFailUnknownOption s1
      then
        join
          [ "Unknown option ",
            s1,
            "." ]
      else
        match
          #var"X2"
        with
          ParseFailMissingOpArg s2
        then
          join
            [ "Option ",
              s2,
              " is missing an argument value." ]
        else
          match
            #var"X2"
          with
            ParseFailConversion (ptype, s3)
          in
          let receivedString =
              lam sval6.
                join
                  [ ", but received \'",
                    sval6,
                    "\'." ]
            in
            let #var"X3" =
              ptype
            in
            match
              #var"X3"
            with
              ParseTypeInt sval
            then
              join
                [ "Option ",
                  s3,
                  " expects an integer value",
                  receivedString
                    sval ]
            else
              match
                #var"X3"
              with
                ParseTypeFloat sval1
              then
                join
                  [ "Option ",
                    s3,
                    " expects a float value",
                    receivedString
                      sval1 ]
              else
                match
                  #var"X3"
                with
                  ParseTypeFloatMin (sval2, minVal)
                then
                  join
                    [ "Option ",
                      s3,
                      " expects a float value of at least ",
                      float2string
                        minVal,
                      receivedString
                        sval2 ]
                else
                  match
                    #var"X3"
                  with
                    ParseTypeFloatInterval (sval3, minVal1, maxVal)
                  then
                    join
                      [ "Option ",
                        s3,
                        " expects a float value in the interval [",
                        float2string
                          minVal1,
                        ", ",
                        float2string
                          maxVal,
                        "]",
                        receivedString
                          sval3 ]
                  else
                    match
                      #var"X3"
                    with
                      ParseTypeIntMin (sval4, minVal2)
                    then
                      join
                        [ "Option ",
                          s3,
                          " expects an integer value of at least ",
                          int2string
                            minVal2,
                          receivedString
                            sval4 ]
                    else
                      match
                        #var"X3"
                      with
                        ParseTypeGeneric (msg1, sval5)
                      in
                      join
                          [ msg1,
                            " \'",
                            sval5,
                            "\'." ]
in
let argPrintError =
  lam result1.
    error
      (join
         [ argPrintErrorString
             result1,
           "\n" ])
in
type Array a12
in
type CArray1 a13
in
external arrayCreateFloat : Int -> Array Float
in
let arrayCreateFloat1: Int -> Array Float =
  lam n7.
    arrayCreateFloat
      n7
in
external arrayLength : all a14. Array a14 -> Int
in
let arrayLength1: all a24. Array a24 -> Int =
  lam a25.
    arrayLength
      a25
in
external arrayGet : all a15. Array a15 -> Int -> a15
in
let arrayGet1: all a22. Array a22 -> Int -> a22 =
  lam a23.
    lam i12.
      arrayGet
        a23
        i12
in
external arraySet! : all a16. Array a16 -> Int -> a16 -> ()
in
let arraySet1: all a20. Array a20 -> Int -> a20 -> () =
  lam a21.
    lam i11.
      lam v4.
        arraySet
          a21
          i11
          v4
in
external cArray1Set! : all a17. CArray1 a17 -> Int -> a17 -> ()
in
let cArray1Set1: all a18. CArray1 a18 -> Int -> a18 -> () =
  lam a19.
    lam i10.
      lam v3.
        cArray1Set
          a19
          i10
          v3
in
let _debug =
  ref
    false
in
let debugPrint =
  lam msg.
    match
      deref
        _debug
    with
      true
    then
      let #var"69" =
        printError
          (join
             [ "LOG DEBUG:\t",
               msg
                 {} ])
      in
      let #var"70" =
        printError
          "\n"
      in
      flushStderr
        {}
    else
      {}
in
let doLoop: Int -> (Int -> ()) -> () =
  lam n6.
    lam f5.
      recursive
        let doLoop1 =
          lam i9.
            match
              eqi
                i9
                n6
            with
              true
            then
              {}
            else
              let #var"68" =
                f5
                  i9
              in
              doLoop1
                (succ
                   i9)
      in
      doLoop1
        0
in
type Vec =
  Tensor[Float]
in
type Mat =
  Tensor[Float]
in
let vget =
  tensorLinearGetExn
in
let vset =
  tensorLinearSetExn
in
let vlength =
  lam v2.
    head
      (tensorShape
         v2)
in
let vcreate =
  lam n5.
    lam f4.
      tensorCreateCArrayFloat
        [ n5 ]
        (lam idx.
           f4
             (head
                idx))
in
let mset =
  sundialsMatrixDenseSet1
in
let mupdate =
  sundialsMatrixDenseUpdate1
in
let vecToString =
  lam name.
    lam v1.
      let n4 =
        vlength
          v1
      in
      join
        (create
           n4
           (lam i8.
              join
                [ name,
                  "[",
                  int2string
                    i8,
                  "] = ",
                  float2string
                    (vget
                       v1
                       i8),
                  "\n" ]))
in
let stateToString =
  lam y8.
    lam yp5.
      join
        [ vecToString
            "y"
            y8,
          "\n",
          vecToString
            "y\'"
            yp5 ]
in
type DAEInit =
  ([Float], [Float])
in
type DAEResf =
  Array Float -> Array Float -> Tensor[Float] -> ()
in
type DAEJacVals =
  [((Int, Int), () -> Float)]
in
type DAEJacf =
  Array Float -> Array Float -> (DAEJacVals, DAEJacVals)
in
type DAEOutf =
  Array Float -> Array Float -> ()
in
type Options =
  {atol: Float, rtol: Float, seed: Int, debug: Bool, interval: Float, stepSize: Float, printStats: Bool, outputOnlyLast: Bool, benchmarkJacobian: Option Int, benchmarkResidual: Option Int}
in
let defaultOptions =
  { atol =
      1e-06,
    rtol =
      0.0001,
    seed =
      0,
    debug =
      false,
    interval =
      20.,
    stepSize =
      0.1,
    outputOnlyLast =
      false,
    dumpInfo =
      false }
in
let argConfig =
  [ ([ ("--interval", " ", "<value>") ], "Simulation interval. ", lam p.
      { p.options
        with
        interval =
          argToFloatMin
            p
            0. }),
    ([ ("--step-size", " ", "<value>") ], "Interval where to output solution. ", lam p1.
      { p1.options
        with
        stepSize =
          argToFloatMin
            p1
            0. }),
    ([ ("--rtol", " ", "<value>") ], "Relative tolerance. ", lam p2.
      { p2.options
        with
        rtol =
          argToFloatMin
            p2
            0. }),
    ([ ("--atol", " ", "<value>") ], "Absolute tolerance. ", lam p3.
      { p3.options
        with
        atol =
          argToFloatMin
            p3
            0. }),
    ([ ("--output-only-last", "", "") ], "Output only the solution after the last time-step. ", lam p4.
      { p4.options
        with
        outputOnlyLast =
          true }),
    ([ ("--debug", "", "") ], "Debug runtime. ", lam p5.
      { p5.options
        with
        debug =
          true }),
    ([ ("--dump-info", "", "") ], "Dump solver info to stderr. ", lam p6.
      { p6.options
        with
        dumpInfo =
          true }),
    ([ ("--seed", " ", "<value>") ], "Random seed. ", lam p7.
      { p7.options
        with
        seed =
          argToIntMin
            p7
            0 }) ]
in
let usage =
  lam prog1.
    join
      [ "Usage: ",
        prog1,
        " [OPTION]\n\n",
        "Options:\n",
        argHelpOptions
          argConfig,
        "\n" ]
in
let _randState =
  lam y7.
    doLoop
      (arrayLength1
         y7)
      (lam i7.
         arraySet1
           y7
           i7
           (int2float
              (randIntU
                 0
                 10)))
in
let _benchUsage =
  lam prog.
    join
      [ prog,
        " INTEGER\n" ]
in
let parseArgs =
  lam n3.
    let #var"X1" =
      argParse
        defaultOptions
        argConfig
    in
    match
      #var"X1"
    with
      ParseOK r4
    then
      match
        neqi
          (length
             r4.strings)
          n3
      with
        true
      then
        let #var"66" =
          print
            (usage
               (get
                  argv
                  0))
        in
        exit
          1
      else
        (r4.options, r4.strings)
    else
      match
        #var"X1"
      with
        result
      in
      let #var"67" =
          argPrintError
            result
        in
        exit
          1
in
let daeRuntimeBenchmarkRes: Int -> DAEResf -> () =
  lam n2.
    lam resf3.
      match
        parseArgs
          1
      with
        (opt2, [ neval2 ])
      then
        let #var"58" =
          randSetSeed
            opt2.seed
        in
        match
          stringIsInt
            neval2
        with
          true
        then
          let y6 =
            arrayCreateFloat1
              n2
          in
          let r3 =
            vcreate
              n2
              (lam #var"63".
                 0.)
          in
          let neval3 =
            string2int
              neval2
          in
          let sum1 =
            ref
              0.
          in
          let ws3 =
            wallTimeMs
              {}
          in
          let #var"59" =
            doLoop
              neval3
              (lam #var"60".
                 let #var"61" =
                   _randState
                     y6
                 in
                 let #var"62" =
                   resf3
                     y6
                     y6
                     r3
                 in
                 doLoop
                   n2
                   (lam i6.
                      modref
                        sum1
                        (addf
                           (deref
                              sum1)
                           (vget
                              r3
                              i6))))
          in
          let wt1 =
            subf
              (wallTimeMs
                 {})
              ws3
          in
          print
            (join
               [ "Executed the residual ",
                 int2string
                   neval3,
                 " times in ",
                 float2string
                   wt1,
                 " ms, accumulating the residual value ",
                 float2string
                   (deref
                      sum1),
                 "\n" ])
        else
          let #var"64" =
            print
              (_benchUsage
                 (get
                    argv
                    0))
          in
          exit
            1
      else
        let #var"65" =
          print
            (_benchUsage
               (get
                  argv
                  0))
        in
        exit
          1
in
let daeRuntimeBenchmarkJac: Int -> DAEJacf -> DAEJacf -> () =
  lam n1.
    lam jacYf2.
      lam jacYpf2.
        match
          parseArgs
            1
        with
          (opt1, [ neval ])
        then
          let #var"48" =
            randSetSeed
              opt1.seed
          in
          match
            stringIsInt
              neval
          with
            true
          then
            let y5 =
              arrayCreateFloat1
                n1
            in
            let neval1 =
              string2int
                neval
            in
            let sum =
              ref
                0.
            in
            let ws2 =
              wallTimeMs
                {}
            in
            let #var"49" =
              doLoop
                neval1
                (lam #var"50".
                   let #var"51" =
                     _randState
                       y5
                   in
                   let jy1 =
                     iter
                       (lam f3.
                          modref
                            sum
                            (addf
                               (deref
                                  sum)
                               (f3.1
                                  {})))
                   in
                   let fs2 =
                     jacYf2
                       y5
                       y5
                   in
                   let #var"52" =
                     jy1
                       fs2.0
                   in
                   let #var"53" =
                     jy1
                       fs2.1
                   in
                   let jyp1 =
                     iter
                       (lam f2.
                          modref
                            sum
                            (addf
                               (deref
                                  sum)
                               (f2.1
                                  {})))
                   in
                   let fs3 =
                     jacYpf2
                       y5
                       y5
                   in
                   let #var"54" =
                     jyp1
                       fs3.0
                   in
                   let #var"55" =
                     jyp1
                       fs3.1
                   in
                   {})
            in
            let wt =
              subf
                (wallTimeMs
                   {})
                ws2
            in
            print
              (join
                 [ "Executed the Jacobian ",
                   int2string
                     neval1,
                   " times in ",
                   float2string
                     wt,
                   " ms, accumulating the value ",
                   float2string
                     (deref
                        sum),
                   "\n" ])
          else
            let #var"56" =
              print
                (_benchUsage
                   (get
                      argv
                      0))
            in
            exit
              1
        else
          let #var"57" =
            print
              (_benchUsage
                 (get
                    argv
                    0))
          in
          exit
            1
in
let daeRuntimeRun: Bool -> [Bool] -> DAEInit -> DAEResf -> DAEJacf -> DAEJacf -> DAEOutf -> () =
  lam numjac.
    lam varids1.
      lam initVals1.
        lam resf1.
          lam jacYf1.
            lam jacYpf1.
              lam outf1.
                match
                  parseArgs
                    0
                with
                  (opt, _)
                in
                let n =
                    length
                      varids1
                  in
                  let #var"13" =
                    modref
                      _debug
                      opt.debug
                  in
                  let resEvalCount =
                    ref
                      0
                  in
                  let jacEvalCount =
                    ref
                      0
                  in
                  let resTimeCount =
                    ref
                      0.
                  in
                  let jacTimeCount =
                    ref
                      0.
                  in
                  let #var"14" =
                    randSetSeed
                      opt.seed
                  in
                  match
                    initVals1
                  with
                    (y0, yp0)
                  in
                  let tol =
                      idaSSTolerances1
                        opt.rtol
                        opt.atol
                    in
                    let y3 =
                      vcreate
                        n
                        (get
                           y0)
                    in
                    let yp3 =
                      vcreate
                        n
                        (get
                           yp0)
                    in
                    let ay =
                      arrayCreateFloat1
                        n
                    in
                    let ayp =
                      arrayCreateFloat1
                        n
                    in
                    let resf2 =
                      lam t11.
                        lam y4.
                          lam yp4.
                            lam r2.
                              let #var"43" =
                                modref
                                  resEvalCount
                                  (succ
                                     (deref
                                        resEvalCount))
                              in
                              let #var"44" =
                                doLoop
                                  n
                                  (lam i5.
                                     let #var"47" =
                                       arraySet1
                                         ay
                                         i5
                                         (vget
                                            y4
                                            i5)
                                     in
                                     arraySet1
                                       ayp
                                       i5
                                       (vget
                                          yp4
                                          i5))
                              in
                              let ws1 =
                                wallTimeMs
                                  {}
                              in
                              let #var"45" =
                                resf1
                                  ay
                                  ayp
                                  r2
                              in
                              let we1 =
                                wallTimeMs
                                  {}
                              in
                              let #var"46" =
                                modref
                                  resTimeCount
                                  (addf
                                     (deref
                                        resTimeCount)
                                     (subf
                                        we1
                                        ws1))
                              in
                              {}
                    in
                    let r1 =
                      vcreate
                        n
                        (lam #var"42".
                           0.)
                    in
                    let #var"15" =
                      resf2
                        y3
                        yp3
                        r1
                    in
                    let #var"16" =
                      debugPrint
                        (lam #var"41".
                           strJoin
                             "\n"
                             [ "Initial residual:",
                               stateToString
                                 y3
                                 yp3,
                               vecToString
                                 "r"
                                 r1 ])
                    in
                    let v =
                      nvectorSerialWrap1
                        y3
                    in
                    let vp =
                      nvectorSerialWrap1
                        yp3
                    in
                    let m =
                      sundialsMatrixDense1
                        n
                    in
                    let nlsolver =
                      sundialsNonlinearSolverNewtonMake1
                        v
                    in
                    let lsolver =
                      match
                        numjac
                      with
                        true
                      then
                        idaDlsSolver1
                          (idaDlsDense1
                             v
                             m)
                      else
                        let ay2 =
                          arrayCreateFloat1
                            n
                        in
                        let ayp2 =
                          arrayCreateFloat1
                            n
                        in
                        let jacf =
                          lam jacargs: IdaJacArgs.
                            lam m1: SundialsMatrixDense.
                              let #var"33" =
                                modref
                                  jacEvalCount
                                  (succ
                                     (deref
                                        jacEvalCount))
                              in
                              let #var"34" =
                                doLoop
                                  n
                                  (lam i4.
                                     let #var"40" =
                                       arraySet1
                                         ay2
                                         i4
                                         (vget
                                            jacargs.y
                                            i4)
                                     in
                                     arraySet1
                                       ayp2
                                       i4
                                       (vget
                                          jacargs.yp
                                          i4))
                              in
                              let ws =
                                wallTimeMs
                                  {}
                              in
                              let jy =
                                iter
                                  (lam ijf1.
                                     match
                                       ijf1
                                     with
                                       ((i3, j1), f1)
                                     in
                                     mset
                                         m1
                                         i3
                                         j1
                                         (f1
                                            {}))
                              in
                              let fs =
                                jacYf1
                                  ay2
                                  ayp2
                              in
                              let #var"35" =
                                jy
                                  fs.0
                              in
                              let #var"36" =
                                jy
                                  fs.1
                              in
                              let jyp =
                                iter
                                  (lam ijf.
                                     match
                                       ijf
                                     with
                                       ((i2, j), f)
                                     in
                                     mupdate
                                         m1
                                         i2
                                         j
                                         (addf
                                            (mulf
                                               jacargs.c
                                               (f
                                                  {}))))
                              in
                              let fs1 =
                                jacYpf1
                                  ay2
                                  ayp2
                              in
                              let #var"37" =
                                jyp
                                  fs1.0
                              in
                              let #var"38" =
                                jyp
                                  fs1.1
                              in
                              let we =
                                wallTimeMs
                                  {}
                              in
                              let #var"39" =
                                modref
                                  jacTimeCount
                                  (addf
                                     (deref
                                        jacTimeCount)
                                     (subf
                                        we
                                        ws))
                              in
                              {}
                        in
                        idaDlsSolverJacf1
                          jacf
                          (idaDlsDense1
                             v
                             m)
                    in
                    let varid =
                      nvectorSerialWrap1
                        (vcreate
                           n
                           (lam i1.
                              match
                                get
                                  varids1
                                  i1
                              with
                                true
                              then
                                idaVarIdDifferential
                              else
                                idaVarIdAlgebraic))
                    in
                    let t0 =
                      negf
                        0.0001
                    in
                    let s =
                      idaInit1
                        { t =
                            t0,
                          y =
                            v,
                          yp =
                            vp,
                          tol =
                            tol,
                          resf =
                            resf2,
                          roots =
                            idaNoRoots,
                          varid =
                            varid,
                          lsolver =
                            lsolver,
                          nlsolver =
                            nlsolver }
                    in
                    let #var"17" =
                      idaCalcICYaYd1
                        s
                        { y =
                            v,
                          yp =
                            vp,
                          tend =
                            0. }
                    in
                    let #var"18" =
                      resf2
                        y3
                        yp3
                        r1
                    in
                    let #var"19" =
                      debugPrint
                        (lam #var"32".
                           strJoin
                             "\n"
                             [ "After idaCalcICYaYd:",
                               stateToString
                                 y3
                                 yp3,
                               vecToString
                                 "r"
                                 r1 ])
                    in
                    let #var"20" =
                      idaSetStopTime1
                        s
                        opt.interval
                    in
                    let ay1 =
                      arrayCreateFloat1
                        n
                    in
                    let ayp1 =
                      arrayCreateFloat1
                        n
                    in
                    recursive
                      let recur =
                        lam t10.
                          let #var"26" =
                            doLoop
                              n
                              (lam i.
                                 let #var"31" =
                                   arraySet1
                                     ay1
                                     i
                                     (vget
                                        y3
                                        i)
                                 in
                                 arraySet1
                                   ayp1
                                   i
                                   (vget
                                      yp3
                                      i))
                          in
                          let #var"27" =
                            match
                              opt.outputOnlyLast
                            with
                              true
                            then
                              {}
                            else
                              outf1
                                ay1
                                ayp1
                          in
                          match
                            gtf
                              t10
                              opt.interval
                          with
                            true
                          then
                            {}
                          else
                            let #var"X" =
                              idaSolveNormal1
                                s
                                { y =
                                    v,
                                  yp =
                                    vp,
                                  tend =
                                    addf
                                      t10
                                      opt.stepSize }
                            in
                            match
                              #var"X"
                            with
                              (tend, IdaSuccess _)
                            then
                              recur
                                tend
                            else
                              match
                                #var"X"
                              with
                                (_, IdaStopTimeReached _)
                              then
                                let #var"28" =
                                  match
                                    opt.outputOnlyLast
                                  with
                                    true
                                  then
                                    outf1
                                      ay1
                                      ayp1
                                  else
                                    {}
                                in
                                {}
                              else
                                match
                                  #var"X"
                                with
                                  (tend1, IdaRootsFound _)
                                then
                                  let #var"29" =
                                    printError
                                      (join
                                         [ "Roots found at t = ",
                                           float2string
                                             tend1 ])
                                  in
                                  flushStderr
                                    {}
                                else
                                  match
                                    #var"X"
                                  with
                                    (tend2, IdaSolveError1 _)
                                  in
                                  let #var"30" =
                                      printError
                                        (join
                                           [ "Solver error at t = ",
                                             float2string
                                               tend2 ])
                                    in
                                    flushStderr
                                      {}
                    in
                    let #var"21" =
                      recur
                        0.
                    in
                    let #var"22" =
                      match
                        opt.dumpInfo
                      with
                        true
                      then
                        let #var"23" =
                          print
                            (join
                               [ "resvals: ",
                                 int2string
                                   (deref
                                      resEvalCount),
                                 "\n" ])
                        in
                        let #var"24" =
                          print
                            (join
                               [ "jacevals: ",
                                 int2string
                                   (deref
                                      jacEvalCount),
                                 "\n" ])
                        in
                        let #var"25" =
                          print
                            (join
                               [ "restime: ",
                                 float2string
                                   (deref
                                      resTimeCount),
                                 "\n" ])
                        in
                        print
                          (join
                             [ "jactime: ",
                               float2string
                                 (deref
                                    jacTimeCount),
                               "\n" ])
                      else
                        {}
                    in
                    {}
in
let varids =
  [ true,
    false,
    false,
    false,
    true,
    false ]
in
let initVals =
  ([ 1.,
    1.,
    0.,
    0.,
    0.,
    0. ], [ 0.,
    0.,
    0.,
    0.,
    0.,
    0. ])
in
let resf =
  lam y2.
    lam yp2.
      lam r.
        let t4 =
          tensorLinearSetExn
            r
            0
            (subf
               (arrayGet
                  yp2
                  4)
               (arrayGet
                  y2
                  5))
        in
        let t5 =
          tensorLinearSetExn
            r
            1
            (subf
               (arrayGet
                  y2
                  1)
               (arrayGet
                  yp2
                  0))
        in
        let t6 =
          tensorLinearSetExn
            r
            2
            (subf
               (arrayGet
                  y2
                  2)
               (arrayGet
                  y2
                  3))
        in
        let t7 =
          tensorLinearSetExn
            r
            3
            (subf
               (arrayGet
                  y2
                  3)
               (negf
                  (arrayGet
                     y2
                     5)))
        in
        let t8 =
          tensorLinearSetExn
            r
            4
            (subf
               (arrayGet
                  y2
                  0)
               (negf
                  (arrayGet
                     y2
                     5)))
        in
        let t9 =
          tensorLinearSetExn
            r
            5
            (subf
               (arrayGet
                  y2
                  4)
               (addf
                  (arrayGet
                     y2
                     2)
                  (arrayGet
                     y2
                     1)))
        in
        {}
in
let jacYf =
  lam y1.
    lam yp1.
      ("", [ ((4, 5), lam #var"2".
          1.),
        ((3, 5), lam #var"3".
          1.),
        ((0, 5), lam #var"4".
          negf
            1.),
        ((5, 4), lam #var"5".
          1.),
        ((3, 3), lam #var"6".
          1.),
        ((2, 3), lam #var"7".
          negf
            1.),
        ((5, 2), lam #var"8".
          negf
            1.),
        ((2, 2), lam #var"9".
          1.),
        ((5, 1), lam #var"10".
          negf
            1.),
        ((1, 1), lam #var"11".
          1.),
        ((4, 0), lam #var"12".
          1.) ])
in
let jacYpf =
  lam y1.
    lam yp1.
      ("", [ ((0, 4), lam #var"".
          1.),
        ((1, 0), lam #var"1".
          negf
            1.) ])
in
let outf =
  lam y.
    lam yp.
      let t =
        print
          (float2string
             (arrayGet
                y
                0))
      in
      let t1 =
        print
          ","
      in
      let t2 =
        print
          (float2string
             (arrayGet
                y
                0))
      in
      let t3 =
        print
          "\n"
      in
      t3
in
daeRuntimeRun
  false
  varids
  initVals
  resf
  jacYf
  jacYpf
  outf
