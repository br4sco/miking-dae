include "math.mc"
include "sundials/sundials.mc"
include "sundials/ida.mc"

let _debug = ref false
let debugPrint = lam msg.
  if deref _debug then
    printError (join ["LOG DEBUG:\t", msg ()]);
    printError "\n";
    flushStderr ()
  else ()

-- Built-ins
let onehot = lam n. lam i. create n (lam j. if eqi i j then 1. else 0.)

type Vec = Tensor[Float]
type Mat = Tensor[Float]

let vget = tensorLinearGetExn
let vset = tensorLinearSetExn
let vlength = lam v. head (tensorShape v)
let viteri = lam f. lam v. tensorIterSlice (lam i. lam v. f i (tensorGetExn v [])) v
let vcreate = lam n. lam f. tensorCreateCArrayFloat [n] (lam idx. f (head idx))
let mget = lam m. lam i. lam j. tensorGetExn m [i, j]
let mset = lam m. lam i. lam j. tensorSetExn m [i, j]

let vecToString = lam name. lam v.
  let n = vlength v in
  join
    (create n
       (lam i.
         join [name, "[", int2string i, "] = ", float2string (vget v i), "\n"]))

let stateToString = lam y. lam yp.
  join [vecToString "y" y, "\n", vecToString "y'" yp]

let usage = lam. strJoin " " ["USAGE:", get argv 0, "interval", "stepsize"]

let errorExit = lam. print (usage ()); print "\n"; exit 1

type DAEInit = ([Float], [Float])
type DAEResf = [Float] -> [Float] -> [Float]
type DAEJacVals = [((Int, Int), () -> Float)]
type DAEJacf = [Float] -> [Float] -> [DAEJacVals]
type DAEOutf = [Float] -> [Float] -> ()

let daeRuntimeRun
  : Bool -> Bool -> [Bool] -> DAEInit -> DAEResf -> DAEJacf -> DAEJacf -> DAEOutf -> ()
  = lam debug. lam numjac. lam varids. lam initVals. lam resf. lam jacYf. lam jacYpf. lam outf.
    modref _debug debug;
    -- Parse arguments
    let args =
      if eqi (length argv) 3 then
        let interv = get argv 1 in
        let stepsize = get argv 2 in
        if and (stringIsFloat interv) (stringIsFloat stepsize) then
          (string2float interv, string2float stepsize)
        else errorExit ()
      else errorExit ()
    in
    match args with (interv, stepsize) in
    -- Initialize
    match initVals with (y0, yp0) in
    let tol = idaSSTolerances 1.e-4 1.e-6 in
    let n = length varids in
    let y = vcreate n (get y0) in
    let yp = vcreate n (get yp0) in
    let resf = lam t. lam y. lam yp. lam r.
      let y = create n (vget y) in
      let yp = create n (vget yp) in
      let t = resf y yp in
      iteri (vset r) t;
      ()
    in
    let r = vcreate n (lam. 0.) in
    resf y yp r;
    debugPrint
      (lam. strJoin "\n"
            ["Initial residual:", stateToString y yp, vecToString "r" r]);
    let v = nvectorSerialWrap y in
    let vp = nvectorSerialWrap yp in
    let m = sundialsMatrixDense n in
    let nlsolver = sundialsNonlinearSolverNewtonMake v in
    let lsolver =
      if numjac then idaDlsSolver (idaDlsDense v m)
      else
        let jacf = lam jacargs : IdaJacArgs. lam m : SundialsMatrixDense.
          let y = create n (vget jacargs.y) in
          let yp = create n (vget jacargs.yp) in
          let m = sundialsMatrixDenseUnwrap m in
          iter
            (iter
                (lam ijf.
                  match ijf with ((i, j), f) in
                  -- m is in column-major format
                  mset m j i (f ())))
            (jacYf y yp);
          iter
            (iter
                (lam ijf.
                  match ijf with ((i, j), f) in
                  -- m is in column-major format
                  mset m j i (addf (mget m j i) (mulf jacargs.c (f ())))))
            (jacYpf y yp);
          ()
        in
        idaDlsSolverJacf jacf (idaDlsDense v m)
    in
    let varid =
      nvectorSerialWrap
        (vcreate n
           (lam i. if get varids i then idaVarIdDifferential
                 else idaVarIdAlgebraic))
    in
    let t0 = negf 1.e-4 in
    let s = idaInit {
      tol      = tol,
      nlsolver = nlsolver,
      lsolver  = lsolver,
      resf     = resf,
      varid    = varid,
      roots    = idaNoRoots,
      t        = t0,
      y        = v,
      yp       = vp
    } in
    idaCalcICYaYd s { tend = 0., y = v, yp = vp };
    resf y yp r;
    debugPrint
      (lam. strJoin "\n"
            ["After idaCalcICYaYd:", stateToString y yp, vecToString "r" r]);
    idaSetStopTime s interv;
    -- Solve
    recursive let recur = lam t.
      let y = create n (vget y) in
      let yp = create n (vget yp) in
      outf y yp;
      if gtf t interv then ()
      else
        switch idaSolveNormal s { tend = addf t stepsize, y = v, yp = vp }
        case (tend, IdaSuccess _) then recur tend
        case (_, IdaStopTimeReached _) then ()
        case (tend, IdaRootsFound _) then
          printError (join ["Roots found at t = ", float2string tend]);
          flushStderr ()
        case (tend, IdaSolveError _) then
          printError (join ["Solver error at t = ", float2string tend]);
          flushStderr ()
        end
    in
    recur 0.;
    ()

mexpr

-- Hack that allows us to parse this file with dead-code elimination
dprint [
  dprint [daeRuntimeRun],
  dprint [sin, cos, exp, sqrt],
  dprint [pow],
  dprint [onehot]
]
