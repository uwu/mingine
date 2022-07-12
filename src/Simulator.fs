module MiniPhys.Simulator

open MiniPhys.Types

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
    
    // need to multiply by 1 radian as radians are dimensionless
    // hence (N m)/(kg m^2) simplifies down to 1/s^2 not rad/s^2
    let newAccel = 1.<rad> * (calcTorques {gObj with angle = newAngle} timeStep globalTime) / gObj.momentOfInertia

    let avgAccel = (gObj.angularAccel + newAccel) / 2.
    let newVelocity = gObj.angularVelocity + (avgAccel * timeStep)
    
    { gObj with
        angle = newAngle
        angularAccel = newAccel
        angularVelocity = newVelocity
        }