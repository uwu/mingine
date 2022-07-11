module MiniPhys.Simulator

open MiniPhys.Types

// huge shoutout to
// http://buildnewgames.com/gamephysics/

let calcForces gObj timeStep globalTime =
    let forces =
        match gObj.forces with
        | SingleFCs sfcs -> 
            sfcs
            |> List.map (fun (_, f) -> f gObj timeStep globalTime)
        | BatchFC bfc -> bfc gObj timeStep globalTime
    
    List.reduce (+) forces

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