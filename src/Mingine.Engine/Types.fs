module Mingine.Engine.Types

open Browser.Types
open Mingine.Types
open Mingine.Units
open System.Collections.Generic
open FSharp.Data.UnitSystems.SI.UnitSymbols

/// Represents a renderable object in the game
type GameObj =
    {id: string
     physicsObj: PhysicsObj
     layer: int
     /// the vector of center subtract bottom left - this is used to position objects accurately
     blOffset: Vec2<m>
     styles: obj
     collider: Collider
     eventHandlers: (string * GameObj -> Event -> unit)[]}

// pure pain
type WrappedGObj(o: GameObj) =
    // despite what the msdocs say, mutating o leaves the equality intact but hashcodes different
    // so heres a custom hashcode impl
    /// global hashcode for all instances of this class. increments on construct.
    static let mutable ghc = 0
    do ghc <- ghc + 1
    /// value of ghc at the time of this instance construction
    member val hc = ghc
    // api cruft.
    override this.GetHashCode() = this.hc

    override this.Equals o =
        match o with
        | :? WrappedGObj as other -> other.GetHashCode() = this.GetHashCode()
        | _ -> false

    member val o = o with get, set

/// Holds all the game objects in the game and controls the root render div
type Scene =
    {scale: float<px / m> // scale of 1 means 1px=1m, scale of 10 means 10px=1m, etc
     rootStyles: obj
     objects: WrappedGObj HashSet // hashset = es set, unlike F# map
     /// where in the scene should be rendered as the origin
     /// - for example: when set to (1, 2) then an object at (1, 2) would be rendered as if at (0, 0)
     renderOffset: Vec2<m>
     canvasSize: Vec2<m>
     postTickHooks: (Scene * float<s> -> unit)[]
     postFrameHooks: (Scene -> unit)[]
     eventHandlers: (string * GameObj -> Event -> unit)[]}