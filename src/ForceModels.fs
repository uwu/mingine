module MiniPhys.ForceModellers
open FSharp.Data.UnitSystems.SI.UnitSymbols

open System
open MiniPhys.Types

// yeah so you can make gravity go sideways if you want just set the x component lmao
/// Models the weight force (gravity!)
let weight (gravity: Vec2<m/s^2>) gObj = gravity * gObj.mass // Fg=mg

let earthWeight = weight {x = 0.<m/s^2>; y = -9.81<m/s^2>}

/// Models a spring force (which wants to come to rest at a fixed position)
let spring (springConstant: float<N/m>) restPos gObj = (restPos - gObj.pos) * -springConstant // Fs=-kx

/// Models a damping force
let viscousDamping zeta gObj = raise (NotImplementedException "TODO!!!")

/// Models air resistance - useful for more naturally implementing terminal velocity for example
let airDrag flowVelocity (density: float<kg/m^3>) (csArea: float<m^2>) (dragCoff: float) gObj =
    let relativeFv = flowVelocity - gObj.velocity
    // Fd=upAc/2
    0.5 * density * (relativeFv * relativeFv) * dragCoff * csArea

/// Models air resistance in still air - see `airDrag`
let stillAirDrag = airDrag Vec2.origin

/// The air density on earth at 20c and at 101.325kPa
let earthAirDensity = 1204.<kg/m^3>