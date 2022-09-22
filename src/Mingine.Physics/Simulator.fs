module Mingine.Physics.Simulator

open System
open Mingine.Types
open Mingine.Units
open FSharp.Data.UnitSystems.SI.UnitSymbols
open FSharp.Collections

// workaround for F# compiler internal error kekw
let private vecOrigin = { x = 0.<_>; y = 0.<_> } // Vec2.origin

let private guardFloatInstability<[<Measure>] 'u> =
    mapFloatTyped<'u> (fun value ->
        if Double.IsNaN value || Math.Abs value = infinity then
            0.
        else
            value)

let inline private guardForceTorqueInstability (f, t) =
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

let updateObjectPos pObj timeStep =
    // update transform from last tick info
    let newPos =
        pObj.pos
        + (pObj.velocity * timeStep)
        + (pObj.accel * 0.5 * (timeStep * timeStep))

    let newAngle =
        pObj.angle
        + (pObj.angVelocity * timeStep)
        + (pObj.angAccel * 0.5 * (timeStep * timeStep))

    // calculate forces and torques
    let force, torque =
        calcForcesAndTorques
            {pObj with
                pos = newPos
                angle = newAngle}
            timeStep

    // update this tick acceleration
    // radians are dimensionless so are not part of the torque nor moment of inertia units, so add them in with *1rad
    let newAccel = force / pObj.mass

    let newAngAccel =
        torque / pObj.momentOfInertia * 1.<rad>

    let avgAccel = (pObj.accel + newAccel) / 2.

    let avgAngAccel =
        (pObj.angAccel + newAngAccel) / 2.

    // update this tick velocity
    let newVelocity =
        pObj.velocity + (avgAccel * timeStep)

    let newAngVelocity =
        pObj.angVelocity + (avgAngAccel * timeStep)

    {pObj with
        pos = newPos
        accel = newAccel
        velocity = newVelocity

        angle = newAngle
        angAccel = newAngAccel
        angVelocity = newAngVelocity}

let impulse (force: Vec2<_>) pObj = { pObj with accel = pObj.accel + (force / pObj.mass) }