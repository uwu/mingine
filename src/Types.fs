namespace MiniPhys.Types

open System
open System.Collections.Generic
open FSharp.Data.UnitSystems.SI.UnitSymbols
open MiniPhys.Types
open MiniPhys.Types.Units

// 2d vector with float64 precision. Works with F# units of measure.
type Vec2<[<Measure>] 'u> = { x: float<'u>; y: float<'u> }
    with
    
    /// origin in 2d space
    static member origin = { x = 0.<_>; y = 0.<_> }
    
    /// map function over both vec components
    static member map<[<Measure>] 'u, [<Measure>] 'v>
        (f: float<'u> -> float<'v>) (v: Vec2<'u>) : Vec2<'v>
            = { x = f v.x; y = f v.y }
    
    /// map function over both vec components of two vecs
    static member map2 f v1 v2 = { x = f v1.x v2.x; y = f v1.y v2.y  }
    
    /// map function over both vec components, also passing an extra param in
    static member maps f v s = { x = f v.x s; y = f v.y s  }
    
    /// map function over both vec components, also passing an extra param first
    static member smap f s v = { x = f s v.x; y = f s v.y  }
    
    static member (~+) (v: Vec2<'u>) = v
    static member (~-) v = Vec2<'u>.origin - v
    
    static member (+) (v1, v2) = (Vec2<'u>.map2 (+)) v1 v2
    
    static member (-) (v1, v2) =
        (Vec2<'u>.map typedToFloat v1, Vec2<'u>.map typedToFloat v2)
        ||> (Vec2<'u>.map2 (-))
        |> Vec2.map floatToTyped<'u>
    
    // dot product
    static member (*) (v1, v2) = (v1.x * v2.x) + (v1.y * v2.y)
    
    // cross product can't really exist in 2d but this gives us the magnitude
    static member (+*) (v1, v2) = (v1.x * v2.y) - (v1.y * v2.x)
    
    static member (*) (v, s) = (Vec2<'u>.maps (*)) v s
    static member (*) (s: float<'u>, v: Vec2<'v>) = v * s
    
    static member (/) (v, s) = (Vec2<'u>.maps (/)) v s
    static member (/) (s, v) = (Vec2<'u>.smap (/)) s v
    
    member vec.len with get () = // ew
        let untypedVec = Vec2<'u>.map typedToFloat vec
        
        (untypedVec.x ** 2 + untypedVec.y ** 2)
        |> Math.Sqrt
        |> floatToTyped<'u>
    
    member v.rotate (typedAngle: float<rad>) origin =
        let offset = v - origin
        let angle = typedToFloat typedAngle
        
        let xPrime = origin.x + ((offset.x * Math.Cos angle) - (offset.y * Math.Sin angle))
        let yPrime = origin.y + ((offset.x * Math.Sin angle) + (offset.y * Math.Cos angle))
        
        { x = xPrime; y = yPrime }

    member v.norm with get () =
        let len = v.len
        if (typedToFloat len) = 0.
        then Vec2.origin
        else v / len
    
    // debugging purposes
    override v.ToString () = $"(%f{v.x}, %f{v.y})"


type Collider =
    | CircularCollider of radius: float<m>
    | RectCollider of size: Vec2<m>
    | CompositeCollider of Collider * Collider

type ForceAndTorque = Vec2<N> * float<N m>

/// A function that calculates a force and torque. It takes the object, time step, and global time.
type SingleForceCalculator = PhysicsObj -> float<s> -> float<s> -> ForceAndTorque

/// A function that calculates all forces and torques. It takes the object, time step, and global time.
and BatchForceCalculator = PhysicsObj -> float<s> -> float<s> -> ForceAndTorque list

and ForceCalculator =
    | SingleFCs of (string * SingleForceCalculator) list
    | BatchFC of BatchForceCalculator

/// Represents an object that can have physics simulated on it
and PhysicsObj = {
    pos: Vec2<m>
    mass: float<kg>
    velocity: Vec2<m/s>
    accel: Vec2<m/s^2>
    forces: ForceCalculator
    
    momentOfInertia: float<kg m^2>
    angle: float<rad> // theta
    angVelocity: float<rad/s> // omega
    angAccel: float<rad/s^2> // alpha
}

/// Represents a renderable object in the game
[<ReferenceEquality>] // pure pain
type GameObj = {
    physicsObj: PhysicsObj
    layer: int
    /// the vector of center subtract bottom left - this is used to position objects accurately
    blOffset: Vec2<m>
    styles: obj
    collider: Collider
}

/// Holds all the game objects in the game and controls the root render div
type Scene = {
    scale: float<px/m> // scale of 1 means 1px=1m, scale of 10 means 10px=1m, etc
    rootStyles: obj
    objects: GameObj HashSet // hashset = es set, unlike F# map
    /// where in the scene should be rendered as the origin
    /// - for example: when set to (1, 2) then an object at (1, 2) would be rendered as if at (0, 0)
    renderOffset: Vec2<m>
    canvasSize: Vec2<m>
}