namespace MiniPhys.Types

open System
open FSharp.Data.UnitSystems.SI.UnitSymbols

module Units =
    /// an arbitrary unit of measure
    [<Measure>] type arb
    
    let arbToFloat (x: float<arb>) = float x
    
    let floatToTyped<[<Measure>] 'u> x : float<'u> = LanguagePrimitives.FloatWithMeasure x
    
    let typedToFloat<[<Measure>] 'u> (x: float<'u>) = float x
    
    let typedToTyped<[<Measure>] 'u, [<Measure>] 'v> = typedToFloat<'u> >> floatToTyped<'v>
    
    let floatToArb = floatToTyped<arb>
    
    let arbToTyped<[<Measure>] 'u> = arbToFloat >> floatToTyped<'u>

type Vec2<[<Measure>] 'u> = { x: float<'u>; y: float<'u> }
    with
    
    member vec.len with get () = // ew
        let untypedVec = Vec2<'u>.map Units.typedToFloat vec
        
        (untypedVec.x ** 2 + untypedVec.y ** 2)
        |> Math.Sqrt
        |> Units.floatToTyped<'u>
    
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
        (Vec2<'u>.map Units.typedToFloat v1, Vec2<'u>.map Units.typedToFloat v2)
        ||> (Vec2<'u>.map2 (-))
        |> Vec2.map Units.floatToTyped<'u>
    
    static member (*) (v1, v2) = (v1.x * v2.x) + (v1.y * v2.y)
    static member (*) (v, s) = (Vec2<'u>.maps (*)) v s
    static member (*) (s: float<'u>, v: Vec2<'v>) = v * s
    
    static member (/) (v, s) = (Vec2<'u>.maps (/)) v s
    static member (/) (s, v) = (Vec2<'u>.smap (/)) s v


/// A function that calculates a force. It takes the object, time step, and global time.
type SingleForceCalculator = GameObj -> float<s> -> float<s> -> Vec2<N>

/// A function that calculates all forces. It takes the object, time step, and global time.
and BatchForceCalculator = GameObj -> float<s> -> float<s> -> Vec2<N> list

and ForceCalculator =
    | SingleFCs of (string * SingleForceCalculator) list
    | BatchFC of BatchForceCalculator

/// Represents a physics / game object
and GameObj = {
    pos: Vec2<m>
    mass: float<kg>
    velocity: Vec2<m/s>
    accel: Vec2<m/s^2>
    forces: ForceCalculator
    updateForces: GameObj -> unit
}