module MiniPhys

open System.Collections.Generic
open Browser
open Fable.Core.JS
open FSharp.Data.UnitSystems.SI.UnitSymbols
open MiniPhys
open MiniPhys.Types

let renderScale = 100.<Units.px/m>

let engine =
    Engine.createEngine
        {scale = renderScale
         renderOffset = Vec2.origin
         canvasSize = {x = 2.<m>; y = 2.<m>}
         rootStyles = {|border = "1px solid gray"|}

         objects =
             HashSet [|WrappedGObj
                           {layer = 1
                            blOffset = {x = 0.1<m>; y = 0.1<m>}
                            styles =
                               {|
                                 // TODO: renderers
                                 width = $"{0.2<m> * renderScale}px"
                                 height = $"{0.2<m> * renderScale}px"
                                 border = "1px solid black"
                                 borderTop = "1px solid red"
                                 borderRight = "1px solid red"
                                 borderRadius = "99999px 0 99999px 99999px"|}
                            collider = CircularCollider 0.1<m>
                            physicsObj =
                               {pos = {x = 0.<m>; y = 1.8<m>}
                                mass = 0.5<kg>
                                velocity = Vec2.origin
                                accel = Vec2.origin
                                forces =
                                   SingleFCs [
                                              // wait this api is actually quite comf ayo
                                              // strings are just tags, theyre ignored by the engine lol
                                              "gravity", ForceModels.earthWeight

                                              (*"ground push force", (fun g ts ->
                                                    (if g.pos.y >= 0.<_>
                                                    then Vec2.origin
                                                    else
                                                        { x = 0.<_>; y = typedAbs(g.velocity.y) * g.mass / ts + 10.<N> })
                                                    , 0.<_>
                                                    )*)

                                              "tether",
                                              ForceModels.spring
                                                  5.<N/m>
                                                  {x = 1.<m>; y = 1.5<m>}
                                                  {x = 0.1<m>; y = 0.1<_>}
                                              "air resistance",
                                              ForceModels.stillAirDrag
                                                  ForceModels.earthAirDensity
                                                  (Math.PI * 0.1<m> * 0.1<m>)
                                                  0.47

                                              "crude basic damping", ForceModels.simpleDamping 0.25 0.05]

                                momentOfInertia = 0.5 * 0.5<kg> * (0.1<m> * 0.1<m>)
                                angle = 0.<_>
                                angVelocity = 0.<_>
                                angAccel = 0.<_>}}|]}

let root =
    document.createElement "div" (* :?> HTMLDivElement*)

document.body.appendChild root |> ignore

engine.mount root

engine.start None