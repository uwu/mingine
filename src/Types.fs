namespace MiniPhys.Types

open System
open FSharp.Data.UnitSystems.SI.UnitSymbols
open MiniPhys.Types.Units

// 2d vector with float64 precision. Works with F# units of measure.
type Vec2<[<Measure>] 'u> = { x: float<'u>; y: float<'u> }
    with
    
    member vec.len with get () = // ew
        let untypedVec = Vec2<'u>.map typedToFloat vec
        
        (untypedVec.x ** 2 + untypedVec.y ** 2)
        |> Math.Sqrt
        |> floatToTyped<'u>
    
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
    
    member v.rotate (angle: float<rad>) origin =
        let offset = origin - v
        let noUnitAng = typedToFloat angle
        
        let xPrime = origin.x + ((offset.x * Math.Cos noUnitAng) - (offset.y * Math.Sin noUnitAng))
        let yPrime = origin.y + ((offset.x * Math.Sin noUnitAng) - (offset.y * Math.Cos noUnitAng))
        
        { x = xPrime; y = yPrime }



/// A function that calculates a force. It takes the object, time step, and global time.
type SingleForceCalculator = GameObj -> float<s> -> float<s> -> Vec2<N>

/// A function that calculates all forces. It takes the object, time step, and global time.
and BatchForceCalculator = GameObj -> float<s> -> float<s> -> Vec2<N> list

and ForceCalculator =
    | SingleFCs of (string * SingleForceCalculator) list
    | BatchFC of BatchForceCalculator

/// A function that calculates a torque. It takes the object, time step, and global time.
and SingleTorqueCalculator = GameObj -> float<s> -> float<s> -> float<N m>

/// A function that calculates all torques. It takes the object, time step, and global time.
and BatchTorqueCalculator = GameObj -> float<s> -> float<s> -> float<N m> list

and TorqueCalculator =
    | SingleTCs of (string * SingleTorqueCalculator) list
    | BatchTC of BatchTorqueCalculator

/// Represents a physics / game object
and GameObj = {
    pos: Vec2<m>
    mass: float<kg>
    velocity: Vec2<m/s>
    accel: Vec2<m/s^2>
    forces: ForceCalculator
    
    momentOfInertia: float<kg m^2>
    angle: float<rad> // theta
    angularVelocity: float<rad/s> // omega
    angularAccel: float<rad/s^2> // alpha
    torques: TorqueCalculator
}