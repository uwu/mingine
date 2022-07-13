module MiniPhys

open Browser
open Browser.Types
open Fable.Core.JS
open Fable.Core.JsInterop
open FSharp.Data.UnitSystems.SI.UnitSymbols
open MiniPhys
open MiniPhys.Types

// basic simulator test
// this will all be replaced by the (mini) game engine
// but for now this'll do just to test the simulator!

let inline setStyle (elem: HTMLElement) (styles: obj) =
    Constructors.Object.assign (elem?style, styles) |> ignore

let inline loop f escape =
    let rec loopF = (fun t ->
        f t
        if not (escape()) then
            Dom.window.requestAnimationFrame loopF |> ignore
        )
    
    Dom.window.requestAnimationFrame loopF |> ignore

let renderingArea = document.createElement "div" :?> HTMLDivElement
document.body.appendChild renderingArea |> ignore

let ball = document.createElement "div"
renderingArea.appendChild ball |> ignore

let springPoint = document.createElement "div"
renderingArea.appendChild springPoint |> ignore

setStyle renderingArea
    {|
        // actual size 2x2m
        width = "200px"
        height = "200px"
        zIndex = "1"
        border = "1px solid gray"
        position = "relative"
    |}

setStyle ball
    {|
        // actual size 20cm diameter
        width = "20px"
        height = "20px"
        border = "1px solid black"
        borderTop = "1px solid red"
        borderRight = "1px solid red"
        borderRadius = "99999px 0 99999px 99999px"
        position = "absolute"
    |}

setStyle springPoint
    {|
        position = "absolute"
        background = "red"
        width = "3px"
        height = "3px"
        left = "99px"
        bottom = "149px"
    |}

[<Measure>] type px
let scale = 100.<px/m>

let typedAbs<[<Measure>] 'u> : float<'u> -> float<'u> = Units.typedToFloat >> Math.abs >> Units.floatToTyped 

// the mutable aspect of this would be handled in-engine but its not implemented yet, this is just temp
let mutable object = {
    pos = { x = 0.<_>; y = 1.8<m> }
    mass = 0.5<kg>
    velocity = Vec2.origin
    accel = Vec2.origin
    forces = SingleFCs [
        // wait this api is actually quite comf ayo
        // strings are just tags, theyre ignored by the engine lol
        "gravity", ForceModels.earthWeight
        
        "ground push force", (fun g ts _ ->
            (if g.pos.y >= 0.<_>
            then Vec2.origin
            else
                { x = 0.<_>; y = typedAbs(g.velocity.y) * g.mass / ts + 10.<N> })
            , 0.<_>
            )
        
        "tether", ForceModels.spring 5.<N/m> { x = 1.<m>; y = 1.5<m> } { x = 0.1<m>; y = 0.1<_> }
        "air resistance", ForceModels.stillAirDrag ForceModels.earthAirDensity (Math.PI * 0.1<m> * 0.1<m>) 0.47
        
        "crude basic damping", ForceModels.simpleDamping 0.25 0.05
    ]
    
    // for a disc rotating, I=MR^2/2
    momentOfInertia = 0.5 * 0.5<kg> * (0.1<m> * 0.1<m>)
    angle = 0.<_>
    angVelocity = 0.<_>
    angAccel = 0.<_>
    }

let startTick: float<s> = performance.now() / 1000. |> Units.floatToTyped
let mutable lastTick = startTick

loop
    (fun t ->
       let thisTick = (Units.floatToTyped t) / 1000.
       let timeStep = thisTick - lastTick
       let globalTick = thisTick - startTick
        
       object <- Simulator.updateObjectPos object timeStep globalTick
        
       ball?style?left <- $"%f{(object.pos.x - 0.1<_>) * scale}px"
       ball?style?bottom <- $"%f{(object.pos.y - 0.1<_>) * scale}px"
       ball?style?transform <- $"rotate(%f{object.angle}rad)"
        
       lastTick <- thisTick
   )
    // this function should return true to escape the RAF loop
    (fun () -> false)