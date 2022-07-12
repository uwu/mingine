module MiniPhys.Simulator

open MiniPhys.Types

// huge shoutout to
// http://buildnewgames.com/gamephysics/

let calcForces gObj timeStep globalTime =
    match gObj.forces with
    | BatchFC bfc -> bfc gObj timeStep globalTime
    | SingleFCs sfcs -> List.map (fun (_, f) -> f gObj timeStep globalTime) sfcs
    
    |> List.reduce (+)

let updateObject gObj timeStep globalTime =
    let newPos = gObj.pos + (gObj.velocity * timeStep) + (gObj.accel * 0.5 * (timeStep * timeStep))
    
    let newAccel = (calcForces {gObj with pos = newPos} timeStep globalTime) / gObj.mass
    let avgAccel = (gObj.accel + newAccel) / 2.
    let newVelocity = gObj.velocity + (avgAccel * timeStep)
    
    { gObj with
        pos = newPos
        accel = newAccel
        velocity = newVelocity
        }