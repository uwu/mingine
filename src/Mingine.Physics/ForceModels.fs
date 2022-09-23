module Mingine.Physics.ForceModels

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Mingine.Types
open Mingine

// yeah so you can make gravity go sideways if you want just set the x component lmao
/// Models the weight force (gravity!)
let weight (gravity: Vec2<m / s^2>) (gObj, _ : float<s>) : ForceAndTorque =
    // Fg=mg, no torque
    gravity * gObj.mass, 0.<_>

let earthGravity = {x = 0.<m/s^2>; y = -9.81<m/s^2>}


/// Models a spring force (which wants to come to rest at a fixed position)
let spring (springConstant: float<N / m>) restPos (connectionOset: Vec2<_>) (gObj, _ : float<s>) : ForceAndTorque =
    let distToCentre = gObj.pos - restPos

    let e =
        distToCentre
        + (connectionOset.rotate gObj.angle Vec2.origin)
    // Fs=-ke
    let Fs = -e * springConstant
    let torque = distToCentre +* Fs // cross product brr

    Fs, torque


/// Models air resistance - useful for more naturally implementing terminal velocity for example
let airDrag
    flowVelocity
    (density: float<kg / m^3>)
    (csArea: float<m^2>)
    (dragCoff: float)
    (gObj, _ : float<s>)
    : ForceAndTorque =

    let relativeFv =
        flowVelocity - gObj.velocity
    // Fd=upAc/2
    let magnitude =
        0.5
        * density
        * (relativeFv * relativeFv)
        * dragCoff
        * csArea
    // magnitude pointing in direction of the relativeFv vector
    // because relativeFv already points against the object's velocity vector
    // no torque
    relativeFv.norm * magnitude, 0.<_>

/// Models air resistance in still air - see `airDrag`
let stillAirDrag = airDrag Vec2.origin

/// The air density on earth at 20c and at 101.325kPa
let earthAirDensity = 1.204<kg/m^3>


/// Not even a physical model, just a really basic damping force
let simpleDamping (posRatio: float) (angRatio: float) (gObj, _ : float<s>) : ForceAndTorque =
    // this force isnt really a proper scientific calculation but oh well
    -gObj.velocity * posRatio
    |> Vec2.map Units.typedToTyped,
    -gObj.angVelocity * angRatio |> Units.typedToTyped