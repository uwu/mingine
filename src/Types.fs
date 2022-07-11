namespace MiniPhys.Types

open System

type Vec2 = { x: float; y: float }
    with
    
    /// origin in 2d space
    static member origin = { x = 0; y = 0 }
    
    /// map function over both vec components
    static member map f v = { x = f v.x; y = f v.y }
    
    /// map function over both vec components of two vecs
    static member map2 f v1 v2 = { x = f v1.x v2.x; y = f v1.y v2.y  }
    
    /// map function over both vec components, also passing an extra param in
    static member maps f v s = { x = f v.x s; y = f v.y s  }
    
    /// map function over both vec components, also passing an extra param first
    static member smap f s v = { x = f s v.x; y = f s v.y  }
    
    static member (~+) (v: Vec2) = v
    static member (~-) v = {x = 0; y = 0} - v
    
    static member (+) (v1, v2) = (Vec2.map2 (+)) v1 v2
    
    static member (-) (v1, v2) = (Vec2.map2 (-)) v1 v2
    
    static member (*) (v1, v2) = (Vec2.map2 (*)) v1 v2
    static member (*) (v, s) = (Vec2.maps (*)) v s
    static member (*) (s: float, v: Vec2) = v * s
    
    static member (/) (v1, v2) = (Vec2.map2 (/)) v1 v2
    static member (/) (v, s) = (Vec2.maps (/)) v s
    static member (/) (s, v) = (Vec2.smap (/)) s v
    
    // dot product
    // no cross product in 2d!
    static member (.*) (v1, v2) = (v1.x * v2.x) + (v1.y * v2.y)
    
    member vec.len with get () = Math.Sqrt (vec.x ** 2 + vec.y ** 2)


type ForceCalculator = GameObj -> Vec2

and GameObj = {
    pos: Vec2
    mass: float
    velocity: Vec2
    accel: Vec2
    forces: (string * ForceCalculator) list
    updateForces: GameObj -> unit
}