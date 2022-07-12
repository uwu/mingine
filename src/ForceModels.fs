module MiniPhys.ForceModels
open FSharp.Data.UnitSystems.SI.UnitSymbols

open System
open MiniPhys.Types

// yeah so you can make gravity go sideways if you want just set the x component lmao
/// Models the weight force (gravity!)
let weight (gravity: Vec2<m/s^2>) gObj (_: float<s>) (_: float<s>) = gravity * gObj.mass // Fg=mg

let earthWeight = weight {x = 0.<m/s^2>; y = -9.81<m/s^2>}


/// Models a spring force (which wants to come to rest at a fixed position)
let spring (springConstant: float<N/m>) restPos gObj (_: float<s>) (_: float<s>) = (restPos - gObj.pos) * springConstant // Fs=kx


/// Models air resistance - useful for more naturally implementing terminal velocity for example
let airDrag flowVelocity (density: float<kg/m^3>) (csArea: float<m^2>) (dragCoff: float) gObj (_: float<s>) (_: float<s>) =
    let relativeFv = flowVelocity - gObj.velocity
    // Fd=upAc/2
    let magnitude = 0.5 * density * (relativeFv * relativeFv) * dragCoff * csArea
    // magnitude pointing in direction of the relativeFv vector
    // because relativeFv already points against the object's velocity vector 
    relativeFv.norm * magnitude

/// Models air resistance in still air - see `airDrag`
let stillAirDrag = airDrag Vec2.origin

/// The air density on earth at 20c and at 101.325kPa
let earthAirDensity = 1.204<kg/m^3>


/// Not even a physical model, just a really basic damping force
let simpleDamping (ratio: float) gObj (tStep: float<s>) (_: float<s>) =
    -gObj.velocity * ratio * gObj.mass / tStep

let private rawViscousDamping (startAmpl: Vec2<_>) (decayRate: float<1/s>) (freq: float<Hz>) time =
    // this should be radians/s but im too lazy to setup units for that so Hz it is
    let angularFreq = 2. * Math.PI * freq
    // y(t) = A e^(-λt) cos(wt)
    startAmpl * Math.Exp(-decayRate * time) * Math.Cos(angularFreq * time)

// honestly this is a bad way of doing it but god help me i dont know how to do this
let private diffToForce (diff: Vec2<_>) gObj (timeStep: float<s>) =
    diff * gObj.mass / (timeStep * timeStep)

// decay rate = lambda
/// Models a damping force - uses decay rate in Hz (1/s)
let dampingDecayRate decayRate restPos freq startTick gObj tStep globalTime =
    let t = globalTime - startTick
    let current = rawViscousDamping restPos decayRate freq t
    let diff = gObj.pos - current

    diffToForce diff gObj tStep

// time constant = tau
/// Models a damping force - uses time constant in s (time for amplitude to decrease by e)
let dampingTimeConst (timeConstant: float<s>) = dampingDecayRate (1. / timeConstant)

// half life = N/A
/// Models a damping force - uses half life in s (time for exponential amplitude to decrease by factor of 2)
let dampingHalfLife (halfLife: float<s>) =
    halfLife
    |> Units.typedToFloat
    |> Math.Log
    |> Units.floatToTyped
    |> dampingDecayRate


// damping ratio = zeta
/// Models a damping force - uses damping ratio (time for exponential amplitude to decrease by factor of 2)
let dampingDampingRatio zeta restPos (freq: float<Hz>) =
    // this is only approx
    let decayRate = zeta * (freq * 2. * Math.PI)
    dampingDecayRate decayRate restPos freq

// Q Factor = Q (wow)
/// Models a damping force - uses Q Factor (high Q = slow damping)
let dampingQFactor Q = dampingDampingRatio (1. / (2. * Q)) // Q = 1/(2zeta) hence zeta = 1/(2Q)