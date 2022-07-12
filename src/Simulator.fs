module MiniPhys.Simulator

open MiniPhys.Types

open FSharp.Data.UnitSystems.SI.UnitSymbols
open MiniPhys.Types.Units

// huge shoutout to
// http://buildnewgames.com/gamephysics/

let calcForces gObj timeStep globalTime =
    match gObj.forces with
    | BatchFC bfc -> bfc gObj timeStep globalTime
    | SingleFCs sfcs -> List.map (fun (_, f) -> f gObj timeStep globalTime) sfcs
    
    |> List.reduce (+)

let calcTorques gObj timeStep globalTime =
    match gObj.torques with
    | BatchTC btc -> btc gObj timeStep globalTime
    | SingleTCs stcs -> List.map (fun (_, f) -> f gObj timeStep globalTime) stcs
    
    |> List.reduce (+)

let updateObjectPos gObj timeStep globalTime =
    let newPos = gObj.pos + (gObj.velocity * timeStep) + (gObj.accel * 0.5 * (timeStep * timeStep))
    
    let newAccel = (calcForces {gObj with pos = newPos} timeStep globalTime) / gObj.mass
    let avgAccel = (gObj.accel + newAccel) / 2.
    let newVelocity = gObj.velocity + (avgAccel * timeStep)
    
    { gObj with
        pos = newPos
        accel = newAccel
        velocity = newVelocity
        }

let updateObjectRotation gObj timeStep globalTime =
    let newAngle = gObj.angle + (gObj.angularVelocity * timeStep) + (gObj.angularAccel * 0.5 * (timeStep * timeStep))
    
    let newAccel = (calcTorques {gObj with angle = newAngle} timeStep globalTime) / gObj.momentOfInertia
                |> typedToTyped // lol i dont know what the hell is going on with these units

    let avgAccel = (gObj.angularAccel + newAccel) / 2.
    let newVelocity = gObj.angularVelocity + (avgAccel * timeStep)
    
    { gObj with
        angle = newAngle
        angularAccel = newAccel
        angularVelocity = newVelocity
        }