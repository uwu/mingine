module MiniPhys.Engine

open System.Collections.Generic
open Browser
open Browser.Types
open Fable.Core.JS
open Fable.Core.JsInterop
open Fable.Core.DynamicExtensions
open MiniPhys.Types
open FSharp.Collections

let mutable ticks = 0
let mutable last = performance.now()
let rec loop t =
    let time = (t - last) / 10.
    let ticksPerMs = (float ticks) / time
    let ticksHz = ticksPerMs * 1000.
    let ticksKHz = ticksHz / 1000. // thats fun
    
    (document.getElementById "khz").innerText <- (string (round ticksKHz))
    
    last <- t
    ticks <- 0
    window.requestAnimationFrame loop |> ignore

loop last

let requiredRootStyles =
    {|position = "relative"|}

let requiredElementStyles =
    {|position = "absolute"|}

let inline applyStyles (elem: HTMLElement) (styles: obj) =
    Constructors.Object.assign (elem?style, styles)
    |> ignore

/// options to control the engine
type StartOpts =
    {
     /// the rate to update the engine at - overriden by lockPhysicsToRender
     physicsHz: float option
     /// a timestep cap, avoids engine instability at the cost of slowdowns
     tsCap: float option
     /// locks the physics tick to happen on each draw frame - overrides physicsHz
     /// (set to -1 to disable)
     lockPhysicsToRender: bool option}

type EngineWrap =
    {mutable scene: Scene
     mutable running: bool
     mutable mounted: HTMLElement option
     mutable lastTick: float
     // F# map does not compile to ES map, but Dictionary does.
     mutable gObjMountedCache: Dictionary<WrappedGObj, HTMLElement>

     mutable start: StartOpts option -> unit
     mutable stop: unit -> unit
     mutable mount: HTMLElement -> unit
     mutable unmount: unit -> HTMLElement}

let private defaultStopFunc () = failwith "Cannot stop a stopped scene"

let updateGameObject scene gObj elem =
    applyStyles elem gObj.styles

    let pos =
        (gObj.physicsObj.pos
        - gObj.blOffset
        - scene.renderOffset)
        * scene.scale

    applyStyles elem requiredElementStyles

    applyStyles
        elem
        {|left = $"{pos.x}px"
          bottom = $"{pos.y}px"
          transform = $"rotate({gObj.physicsObj.angle}rad)"|}

let renderRoot engine =
    let elem = Option.get engine.mounted
    applyStyles elem engine.scene.rootStyles

    let canvasPxSize =
        engine.scene.canvasSize * engine.scene.scale

    applyStyles
        elem
        {|width = $"{canvasPxSize.x}px"
          height = $"{canvasPxSize.y}px"|}

    applyStyles elem requiredRootStyles

let renderGameObjects engine =
    for wrapped in engine.scene.objects do
        let exists =
            engine.gObjMountedCache.ContainsKey wrapped

        if not exists then
            let elem =
                window.document.createElement "div"

            (Option.get engine.mounted).appendChild elem
            |> ignore

            engine.gObjMountedCache[wrapped] <- elem

        updateGameObject engine.scene wrapped.o engine.gObjMountedCache[wrapped]

    for kv in Seq.toArray engine.gObjMountedCache do
        if not (engine.scene.objects.Contains kv.Key) then
            kv.Value.remove ()
            engine.gObjMountedCache.Remove kv.Key |> ignore

let runPhysicsTick engine timeStep =
    ticks <- ticks + 1
    // EWWWW MUTABILITY
    for wrapped in engine.scene.objects do
        let o = wrapped.o
        wrapped.o <- {o with physicsObj = Simulator.updateObjectPos o.physicsObj timeStep}
    
    let hooks = engine.scene.postTickHooks
    for h in hooks do
        // automatic uncurrying fail
        h(*.Invoke*)(engine.scene, timeStep)

let createEngine scene =
    // i love hacks
    // fable doesnt assign instance members to the prototype so thats a nope
    // and it wraps funcs in a record in arrow functions so `jsThis` won't work.

    let mutable this = Unchecked.defaultof<_> // actually just `null`

    this <-
        {scene = scene
         running = false
         mounted = None
         lastTick = 0

         gObjMountedCache = Dictionary()

         mount = (fun elem -> this.mounted <- Some elem)

         unmount =
             (fun () ->
                 match this.mounted with
                 | Some m ->
                     m.remove ()
                     this.mounted <- None
                     m
                 | None -> failwith "Cannot unmount a non-mounted scene")

         stop = defaultStopFunc
         start =
            (Option.defaultValue
                {lockPhysicsToRender = None
                 physicsHz = None
                 tsCap = None})
            >> (fun sOpts ->
                let lockPhysicsToRender =
                    sOpts.lockPhysicsToRender
                    |> Option.defaultValue false

                let physicsHz =
                    sOpts.physicsHz |> Option.defaultValue 200

                let inline calcTStep tick =
                    match sOpts.tsCap with
                    | Some t when t > 0 -> min (tick - this.lastTick) t
                    | None -> min (tick - this.lastTick) 25 // 25ms default cap
                    | _ -> tick - this.lastTick
                
                let mutable cancel = false

                let rec renderLoop =
                    (fun tick ->
                        if lockPhysicsToRender then
                            let timeStep = calcTStep tick
                            this.lastTick <- tick
                            runPhysicsTick this (timeStep / 1000.<_>)

                        renderRoot this
                        renderGameObjects this

                        if not cancel then
                            window.requestAnimationFrame renderLoop |> ignore)

                renderLoop (performance.now ())

                let intervalCode =
                    if lockPhysicsToRender then
                        None
                    else
                        Some(
                            Macrotask.macroloop
                                (fun () ->
                                    let tick = performance.now ()
                                    let timeStep = 1. //calcTStep tick
                                    this.lastTick <- tick

                                    runPhysicsTick this (timeStep / 1000.<_>))
                                //(int (1000. / physicsHz))
                        )

                this.stop <-
                    (fun () ->
                        cancel <- true
                        // clear the interval if intervalCode is Some(int), do nothing if None
                        Option.map (fun f -> f()) intervalCode |> ignore
                        this.stop <- defaultStopFunc)

                this.lastTick <- performance.now ())}

    this