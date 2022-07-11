module MiniPhys.Simulator

open MiniPhys.Types

// huge shoutout to
// http://buildnewgames.com/gamephysics/

let calcForces gObj =
    gObj.forces
    |> List.map (fun (_, f) -> f gObj)
    |> List.reduce (+)

let updateObject gObj (timeStep: float) =
    let newPos = gObj.pos + (gObj.velocity * timeStep) + (0.5 * gObj.accel * (timeStep ** 2))
    
    let newAccel = (calcForces {gObj with pos = newPos}) / gObj.mass
    let avgAccel = (gObj.accel + newAccel) / 2.
    let newVelocity = gObj.velocity + (avgAccel * timeStep)
    
    { gObj with
        pos = newPos
        accel = newAccel
        velocity = newVelocity
        }