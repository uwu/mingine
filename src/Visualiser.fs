module MiniPhys.Visualiser

open Fable.Core.JS
open Fable.Core.JsInterop
open MiniPhys.Types

type VisObj = | V2 of WrappedGObj * Vec2<1>

type VisData = int * VisObj list

let private tickHook (s, _) =
    (s?__VIS__DATA : VisData)
    |> snd
    |> List.iter (
            function
            | V2(go, vvis) ->
                let angle = atan2 vvis.y vvis.x
                go.o <- {
                        go.o with
                            physicsObj =
                            {
                                go.o.physicsObj with angle = -angle * 1.<_>
                            }
                    }
                
                go.o.styles?width <- "20px" //$"{vvis.len * s.scale}px"
        )

let initVis scene =
    let hookI = scene.postTickHooks.Length
    let newScene =
        {
            scene with postTickHooks =
                        scene.postTickHooks
                        |> Collections.Array.append [| tickHook |]
        }
    newScene?__VIS__DATA <- ((hookI, []) : VisData)
    newScene

let uninitVis scene =
    let hookI, _ = scene?__VIS__DATA
    {
        scene with postTickHooks =
                    scene.postTickHooks
                    |> Collections.Array.removeAt hookI
    }

let createVisVec scene vvis =
    let gObj =
        WrappedGObj {
            id = "VVIS_" + string (Math.random())
            physicsObj = {
                pos = Vec2.origin
                mass = 1.<_>
                velocity = Vec2.origin
                accel = Vec2.origin
                forces = [||]
                momentOfInertia = 1.<_>
                angle = 0.<_>
                angVelocity = 0.<_>
                angAccel = 0.<_> }
            layer = 9999
            blOffset = Vec2.origin
            styles =
                {|
                    borderTop = "1px solid red"
                    borderRight = "5px solid blue"
                    height = "1px"
                |}
            collider = NullCollider }
    
    let i, data = scene?__VIS__DATA
    scene.objects.Add gObj |> ignore
    
    scene?__VIS__DATA <- ((i, V2(gObj, vvis)::data) : VisData)
    
    (fun vpos vvis ->
        let i, data = scene?__VIS__DATA
        
        let newData =
            data
            |> List.map (function
                    | V2(g, v) ->
                        if g <> gObj then
                            V2(g, v)
                        else
                            V2(g, vvis)
                )
        
        scene?__VIS__DATA <- ((i, newData) : VisData)
        )