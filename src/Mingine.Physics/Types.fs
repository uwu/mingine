namespace Mingine.Types

open System
open System.Collections.Generic
open FSharp.Data.UnitSystems.SI.UnitSymbols
open Mingine.Units

/// 2d vector with float64 precision. Works with F# units of measure.
type Vec2<[<Measure>] 'u> =
    {x: float<'u>
     y: float<'u>}
    
    /// origin in 2d space
    static member origin = {x = 0.<_>; y = 0.<_>}

    /// map function over both vec components
    static member map<[<Measure>] 'u, [<Measure>] 'v> (f: float<'u> -> float<'v>) (v: Vec2<'u>) : Vec2<'v> =
        {x = f v.x; y = f v.y}

    /// map function over both vec components of two vecs
    static member map2 f v1 v2 = {x = f v1.x v2.x; y = f v1.y v2.y}

    /// map function over both vec components, also passing an extra param in
    static member maps f v s = {x = f v.x s; y = f v.y s}

    /// map function over both vec components, also passing an extra param first
    static member smap f s v = {x = f s v.x; y = f s v.y}

    static member (~+)(v: Vec2<'u>) = v
    static member (~-) v = Vec2<'u>.origin - v

    static member (+)(v1, v2) = (Vec2<'u>.map2 (+)) v1 v2

    static member (-)(v1, v2) =
        (Vec2<'u>.map typedToFloat v1, Vec2<'u>.map typedToFloat v2)
        ||> (Vec2<'u>.map2 (-))
        |> Vec2.map floatToTyped<'u>

    // dot product
    static member (*)(v1, v2) = (v1.x * v2.x) + (v1.y * v2.y)

    // cross product can't really exist in 2d but this gives us the magnitude
    static member (+*)(v1, v2) = (v1.x * v2.y) - (v1.y * v2.x)

    static member (*)(v, s) = (Vec2<'u>.maps (*)) v s
    static member (*)(s: float<'u>, v: Vec2<'v>) = v * s

    static member (/)(v, s) = (Vec2<'u>.maps (/)) v s
    static member (/)(s, v) = (Vec2<'u>.smap (/)) s v
    
    
    static member atAngle (angle: float<rad>) : Vec2<1> = { x = cos (-angle * 1.<_>); y = sin (-angle * 1.<_>) }
    
    member vec.len = // ew
        let untypedVec =
            Vec2<'u>.map typedToFloat vec

        (untypedVec.x ** 2 + untypedVec.y ** 2)
        |> Math.Sqrt
        |> floatToTyped<'u>

    member v.rotate (typedAngle: float<rad>) origin =
        let offset = v - origin
        // lol negate it to make it cw not ccw
        let angle = typedToFloat -typedAngle

        let xPrime =
            origin.x
            + ((offset.x * Math.Cos angle)
               - (offset.y * Math.Sin angle))

        let yPrime =
            origin.y
            + ((offset.x * Math.Sin angle)
               + (offset.y * Math.Cos angle))

        {x = xPrime; y = yPrime}

    member v.norm =
        let len = v.len

        if len = 0.<_> then
            Vec2.origin
        else
            v / len

    member v1.angleTo (v2: Vec2<_>) =
        // dot product = |a| |b| cos(theta)
        // so cos-1(dot product / |a| |b|) = theta
        acos ((v1 * v2) / (v1.len * v2.len)) |> floatToTyped<rad>
    
    member v.perp = { x = v.y; y = -v.x }
    
    // debugging purposes
    override v.ToString() = $"(%f{v.x}, %f{v.y})"


type Collider =
    | CircularCollider of float<m> * Vec2<m> // radius, center
    | RectCollider of Vec2<m> * Vec2<m> // bottom left, top right
    | CompositeCollider of Collider * Collider
    // note that the normal is to the anticlockwise side of this vec, so that (1, 0) makes a floor
    | PlaneCollider of Vec2<m> * Vec2<m>
    | NullCollider

type ForceAndTorque = Vec2<N> * float<N m>

/// A function that calculates a force and torque. It takes the object & time step.
type ForceCalculator = PhysicsObj * float<s> -> ForceAndTorque

/// Represents an object that can have physics simulated on it
and PhysicsObj =
    {pos: Vec2<m>
     mass: float<kg>
     velocity: Vec2<m / s>
     accel: Vec2<m / s^2>
     
     forces: ForceCalculator[]

     momentOfInertia: float<kg m^2>
     angle: float<rad> // theta
     angVelocity: float<rad / s> // omega
     angAccel: float<rad / s^2> // alpha
     
     restitutionCoeff: float}