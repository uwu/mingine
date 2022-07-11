module MiniPhys.ForceModellers

open System
open MiniPhys.Types

// yeah so you can make gravity go sideways if you want just set the x component lmao
/// Models the weight force (gravity!)
let weight (gravity: Vec2) gObj = gObj.mass * gravity // Fg=mg

let earthWeight = weight {x = 0; y = -9.81}

/// Models a spring force (which wants to come to rest at a fixed position)
let spring (springConstant: float) restPos gObj = -springConstant * (restPos - gObj.pos) // Fs=-kx

/// Models a damping force - 0=undamped <1=underdamped 1=critically damped >1=overdamped (will overshoot!)
let viscousDamping zeta gObj = raise (NotImplementedException "TODO!!!")

/// Models air resistance - useful for more naturally implementing terminal velocity for example
let airDrag flowVelocity density csArea dragCoff gObj =
    let relativeFv = flowVelocity - gObj.velocity
    0.5 * density * (relativeFv .* relativeFv) * dragCoff * csArea

/// Models air resistance in still air - see `airDrag`
let stillAirDrag = airDrag Vec2.origin

/// The air density on earth at 20c and at 101.325kPa
let earthAirDensity = 1204 // kg/m^3
// TODO: F# units of measure