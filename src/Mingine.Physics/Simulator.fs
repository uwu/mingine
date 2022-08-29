module Mingine.Physics.Simulator

open System
open Mingine.Types
open Mingine.Units
open FSharp.Data.UnitSystems.SI.UnitSymbols
open FSharp.Collections

// workaround for F# compiler internal error kekw
let private vecOrigin = { x = 0.<_>; y = 0.<_> } // Vec2.origin

let guardFloatInstability<[<Measure>] 'u> =
    mapFloatTyped<'u> (fun value ->
        if Double.IsNaN value || Math.Abs value = infinity then
            0.
        else
            value)

let guardForceTorqueInstability (f, t) =
    Vec2.map guardFloatInstability f, guardFloatInstability t

let calcForcesAndTorques (gObj: PhysicsObj) (timeStep: float<s>) =
    let forcesAndTorques =
        gObj.forces
        |> Array.map (fun f -> f (gObj, timeStep))
    
    if forcesAndTorques.Length = 0 then
        vecOrigin, 0.<_>
    else
        forcesAndTorques
        // remove NaN and +-Infinity forces and torques
        |> Array.map guardForceTorqueInstability
        // sum all forces and torques
        |> Array.reduce (fun (f1, t1) (f2, t2) -> (f1 + f2, t1 + t2))

let updateObjectPos gObj timeStep =
    // update transform from last tick info
    let newPos =
        gObj.pos
        + (gObj.velocity * timeStep)
        + (gObj.accel * 0.5 * (timeStep * timeStep))

    let newAngle =
        gObj.angle
        + (gObj.angVelocity * timeStep)
        + (gObj.angAccel * 0.5 * (timeStep * timeStep))

    // calculate forces and torques
    let force, torque =
        calcForcesAndTorques
            {gObj with
                pos = newPos
                angle = newAngle}
            timeStep

    // update this tick acceleration
    // radians are dimensionless so are not part of the torque nor moment of inertia units, so add them in with *1rad
    let newAccel = force / gObj.mass

    let newAngAccel =
        torque / gObj.momentOfInertia * 1.<rad>

    let avgAccel = (gObj.accel + newAccel) / 2.

    let avgAngAccel =
        (gObj.angAccel + newAngAccel) / 2.

    // update this tick velocity
    let newVelocity =
        gObj.velocity + (avgAccel * timeStep)

    let newAngVelocity =
        gObj.angVelocity + (avgAngAccel * timeStep)

    {gObj with
        pos = newPos
        accel = newAccel
        velocity = newVelocity

        angle = newAngle
        angAccel = newAngAccel
        angVelocity = newAngVelocity}