module MiniPhys.Engine

open System.Collections.Generic
open Browser
open Browser.Types
open Fable.Core.JS
open Fable.Core.JsInterop
open MiniPhys.Types
open FSharp.Collections

let requiredRootStyles =
    {|
        position = "relative"
    |}

let requiredElementStyles =
    {|
        position = "absolute"
    |}

let inline applyStyles (elem: HTMLElement) (styles: obj) =
    Constructors.Object.assign (elem?style, styles)
    |> ignore

type StartOpts =
    {physicsHz: float option
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
        gObj.physicsObj.pos
        - gObj.blOffset
        - scene.renderOffset

    applyStyles elem requiredElementStyles
    
    applyStyles
        elem
        {|left = $"{pos.x}px"
          bottom = $"{pos.y}px"
          transform = $"rotate(${gObj.physicsObj.angle}rad)"|}

let renderRoot engine =
    let elem = Option.get engine.mounted
    applyStyles elem engine.scene.rootStyles
    
    let canvasPxSize = engine.scene.canvasSize * engine.scene.scale
    applyStyles elem
        {|
            width = $"{canvasPxSize.x}px"
            height = $"{canvasPxSize.y}px"
        |}
    
    applyStyles elem requiredRootStyles

let renderGameObjects engine =
    for wrapped in engine.scene.objects do
        let exists = engine.gObjMountedCache.ContainsKey wrapped

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
    // EWWWW MUTABILITY
    for wrapped in engine.scene.objects do
        let o = wrapped.o
        wrapped.o <- { o with physicsObj = Simulator.updateObjectPos o.physicsObj timeStep }
    
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

         mount =
             (fun elem -> this.mounted <- Some elem)

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
                 physicsHz = None})
            >> (fun sOpts ->
                let lockPhysicsToRender =
                    sOpts.lockPhysicsToRender
                    |> Option.defaultValue false

                let physicsHz =
                    sOpts.physicsHz |> Option.defaultValue 200

                let mutable cancel = false

                let rec renderLoop =
                    (fun tick ->
                        if lockPhysicsToRender then
                            let timeStep = tick - this.lastTick
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
                            setInterval
                                (fun () ->
                                    let tick = performance.now ()
                                    let timeStep = tick - this.lastTick
                                    this.lastTick <- tick
                                    
                                    runPhysicsTick this (timeStep / 1000.<_>))
                                (int (1000. / physicsHz))
                        )

                this.stop <-
                    (fun () ->
                        cancel <- true
                        // clear the interval if intervalCode is Some(int), do nothing if None
                        Option.map clearInterval intervalCode |> ignore
                        this.stop <- defaultStopFunc)

                this.lastTick <- performance.now ())}

    this