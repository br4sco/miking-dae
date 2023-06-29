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

type Vec = Tensor[Float]
type Mat = Tensor[Float]

let vget = tensorLinearGetExn
let vset = tensorLinearSetExn
let vlength = lam v. head (tensorShape v)
let viteri = lam f. lam v. tensorIterSlice (lam i. lam v. f i (tensorGetExn v [])) v
let vcreate = lam n. lam f. tensorCreateCArrayFloat [n] (lam idx. f (head idx))
let mget = lam m. lam i. lam j. tensorGetExn m [i, j]
let mset = lam m. lam i. lam j. tensorSetExn m [i, j]

type DAERuntimeSession = {
  x : NvectorSerial,
  xp : NvectorSerial,
  s : IdaSession
}

type DAEInitf = Vec -> Vec -> ()
type DAEResf = Vec -> Vec -> Vec -> ()
type DAEJacf = Vec -> Vec -> Float -> Vec -> Vec -> Mat -> ()
type DAEOutputf = Vec -> Vec -> ()
type DAEHeaderf = () -> ()

-- let printSol = lam y.
--   let n = vlength y in
--   viteri
--     (lam i. lam y.
--       print (float2string y);
--       (if eqi (addi i 1) n then () else print ","))
--     y;
--   print "\n"

-- let printHeader = lam names.
--   iter (lam name. print name; print ",") (init names);
--   print (last names);
--   print "\n"

let vecToString = lam name. lam v.
  let n = vlength v in
  join
    (create n
       (lam i.
         join [name, "[", int2string i, "] = ", float2string (vget v i), "\n"]))

let stateToString = lam y. lam yp.
  join [vecToString "y" y, "\n", vecToString "y'" yp]

let daeInit : [Bool] -> DAEInitf -> DAEResf -> Option DAEJacf -> DAERuntimeSession
  = lam varids. lam initf. lam resf. lam jacf.
    let resf = lam. resf in
    let tol = idaSSTolerances 1.e-4 1.e-6 in
    let n = length varids in
    let x = vcreate n (lam. 0.) in
    let xp = vcreate n (lam. 0.) in
    let r = vcreate n (lam. 0.) in
    initf x xp;
    resf x xp r;
    debugPrint
      (lam. strJoin "\n"
            ["After initf:", stateToString x xp, vecToString "r" r]);
    let v = nvectorSerialWrap x in
    let vp = nvectorSerialWrap xp in
    let m = sundialsMatrixDense n in
    let nlsolver = sundialsNonlinearSolverNewtonMake v in
    let lsolver =
      optionMapOrElse
        (lam. idaDlsSolver (idaDlsDense v m))
        (lam jacf.
          let jacf = lam jacargs : IdaJacArgs. lam m : SundialsMatrixDense.
            let m = sundialsMatrixDenseUnwrap m in
            jacf jacargs.y jacargs.tmp.0 jacargs.c jacargs.yp jacargs.tmp.1 m;
            ()
          in
          idaDlsSolverJacf jacf (idaDlsDense v m))
        jacf
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
    resf x xp r;
    debugPrint
      (lam. strJoin "\n"
            ["After idaCalcICYaYd:", stateToString x xp, vecToString "r" r]);
    { x = v, xp = vp, s = s }

let daeSolve : DAERuntimeSession -> DAEHeaderf -> DAEOutputf -> Float -> Float -> ()
  = lam s. lam headerf. lam outputf. lam interv. lam stepsize.
    let x = nvectorSerialUnwrap s.x in
    let xp = nvectorSerialUnwrap s.xp in
    idaSetStopTime s.s interv;
    recursive let recur = lam t.
      outputf x xp;
      if gtf t interv then ()
      else
        switch idaSolveNormal s.s { tend = addf t stepsize, y = s.x, yp = s.xp }
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
    headerf ();
    recur 0.

let usage = lam. strJoin " " ["USAGE:", get argv 0, "interval", "stepsize"]

let errorExit = lam. print (usage ()); print "\n"; exit 1

let daeRuntimeRun
  : Bool -> Bool -> [Bool] -> DAEInitf -> DAEResf -> DAEJacf -> DAEHeaderf -> DAEOutputf -> ()
  = lam debug. lam numjac. lam varids. lam initf. lam resf. lam jacf. lam headerf. lam outputf.
    modref _debug debug;
    if eqi (length argv) 3 then
      let interv = get argv 1 in
      let stepsize = get argv 2 in
      if and (stringIsFloat interv) (stringIsFloat stepsize) then
        let jacf = if numjac then None () else (Some jacf) in
        let s = daeInit varids initf resf jacf in
        daeSolve s headerf outputf (string2float interv) (string2float stepsize)
      else errorExit ()
    else errorExit ()

let daeRuntimeRun2
  : Bool
    -> Bool
      -> [Bool]
        -> ([Float], [Float])
          -> ([Float] -> [Float] -> [Float])
             -> ([Float] -> [Float] -> [((Int, Int), Float)])
                -> ([Float] -> [Float] -> [((Int, Int), Float)])
                   -> ([Float] -> [Float] -> ())
                      -> ()
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
          iter (lam ijv.
            match ijv with ((i, j), v) in
            -- m is in column-major format
            mset m j i v)
            (jacYf y yp);
          iter (lam ijv.
            match ijv with ((i, j), v) in
            -- m is in column-major format
            mset m j i (addf (mget m j i) (mulf jacargs.c v)))
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
  dprint [daeRuntimeRun2],
  dprint [sin, cos, exp, sqrt],
  dprint [pow]
]
