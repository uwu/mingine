module Mingine.Engine

open System.Collections
open System.Collections.Generic
open Browser
open Browser.Types
open Fable.Core
open Fable.Core.JS
open Fable.Core.JsInterop
open Mingine.Types
open Mingine.Types.Engine
open Mingine.Physics
open FSharp.Collections

let vecOrigin = { x = 0.<_>; y = 0.<_> }

let private eventNames =
    (Constructors.Object.keys window :> obj :?> string[])
    |> Collections.Array.choose (fun s -> if s.StartsWith "on" then Some(s.Substring 2) else None)

let requiredRootStyles =
    {|position = "relative"
      boxSizing = "border-box"|}

let requiredElementStyles =
    {|position = "absolute"
      boxSizing = "border-box"|}

// TODO remove
// workaround for fable bug
[<Emit("Object.assign($0, $1)")>]
let ___assign _ _ = jsNative<unit>

let inline applyStyles (elem: HTMLElement) (styles: obj) =
    (*Constructors.Object.*)___assign elem?style styles
    //|> ignore

/// options to control the engine
type StartOpts =
    {/// the rate to update the engine at - overriden by lockPhysicsToRender
     physicsHz: float option
     /// a timestep cap, avoids engine instability at the cost of slowdowns
     /// (set to -1 to disable)
     tsCap: float option
     /// locks the physics tick to happen on each draw frame - overrides physicsHz
     lockPhysicsToRender: bool option}

type EngineWrap =
    {mutable scene: Scene
     mutable running: bool
     mutable mounted: HTMLElement option
     mutable lastTick: float
     
     // keeps track of mounted elements
     mutable gObjMountedCache: DoubleDict<WrappedGObj, HTMLElement>

     // used both for the collision api and for the actual algo
     mutable collisionCache: Dictionary<WrappedGObj, WrappedGObj HashSet>
     queryCollision: WrappedGObj -> WrappedGObj -> bool
     
     // these caches are internal only.
     mutable tickEventCache: ResolvedEvent list
     mutable frameEventCache: ResolvedEvent list
     
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
        {|left = 0
          bottom = 0
          transform = $"translate({pos.x}px, {-pos.y}px) rotate({gObj.physicsObj.angle}rad)"|}

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
            engine.gObjMountedCache.Contains1 wrapped

        if not exists then
            let elem =
                window.document.createElement "div"

            (Option.get engine.mounted).appendChild elem
            |> ignore

            engine.gObjMountedCache.Set1 wrapped elem

        updateGameObject engine.scene wrapped.o (engine.gObjMountedCache.Get1 wrapped)

    for k, v in Seq.toArray engine.gObjMountedCache do
        if not (engine.scene.objects.Contains k) then
            v.remove ()
            engine.gObjMountedCache.Remove1 k |> ignore

let collideAllObjects engine _ =
    engine.collisionCache.Clear()

    // yes this is all mutable state, whatever.
    
    for obj in engine.scene.objects do
        let worldCollisions = // TODO
            engine.scene.worldColliders
            |> Seq.choose (fun c2 ->
                Collision.collideColliders obj.o.collider c2 obj.o.physicsObj.pos obj.o.physicsObj.angle vecOrigin 0.<_>
                |> Option.map Collision.resolveMTV
                )
        
        let set =
            if engine.collisionCache.ContainsKey obj
            then engine.collisionCache[obj]
            else
                let h = HashSet()
                engine.collisionCache.Add(obj, h)
                h
        
        let collisions = List()
        
        for obj2 in engine.scene.objects |> Seq.except [|obj|] do
            // only one collision per pair of objects
            if not (engine.collisionCache.ContainsKey obj2 && engine.collisionCache[obj2].Contains obj)
            then
                ignore <| set.Add obj2
                
                if engine.collisionCache.ContainsKey obj2
                then ignore <| engine.collisionCache[obj2].Add obj
                else
                    engine.collisionCache.Add(obj2, HashSet([|obj|]))
                
                match Collision.collideObjs obj.o.collider obj.o.physicsObj obj2.o.collider obj2.o.physicsObj with
                | None -> ()
                | Some mtv ->
                    let resolved1, resolved2 =
                        Collision.resolveCollision
                            obj.o.collider obj.o.physicsObj
                            obj2.o.collider obj2.o.physicsObj
                    
                    collisions.Add (mtv.len, resolved1, resolved2, obj2)
        
        // why do we use the smallest possible collision? idrk ask 2022 me -- sink 2024
        // size of mtv, self phys obj, other phys obj, other wrappedgobj
        if collisions.Count > 0 then
            let _len, newo1, newo2, wo2 =
                collisions
                |> Seq.reduce (fun c1 c2 ->
                    let l1, _, _, _ = c1
                    let l2, _, _, _ = c2
                    if l1 > l2 then c1 else c2)
            
            obj.o <- { obj.o with physicsObj = newo1 }
            wo2.o <- { wo2.o with physicsObj = newo2 }
    
    (*let objects =
        engine.scene.objects
        |> Seq.choose (fun o ->
            let worldCollisions =
                engine.scene.worldColliders
                |> Seq.choose (fun c2 ->
                    Collision.collideColliders o.o.collider c2 o.o.physicsObj.pos o.o.physicsObj.angle vecOrigin 0.<_>
                    |> Option.map Collision.resolveMTV
                    )
            
            let collisions =
                engine.scene.objects
                |> Seq.except [|o|]
                |> Seq.choose (fun o2 ->
                            match Collision.collideObjs o.o.collider o.o.physicsObj o2.o.collider o2.o.physicsObj with
                            | None -> None
                            | Some rawMtv ->
                                match engine.collisionCache.TryGetValue o with
                                | true, list -> engine.collisionCache[o] <- o2::list
                                | _ -> engine.collisionCache[o] <- [o2]
                                
                                if (abs o2.o.physicsObj.mass) = (infinity * 1.<_>)
                                then Some rawMtv // prevent (NaN, NaN)
                                else Some (rawMtv * (o2.o.physicsObj.mass / (o2.o.physicsObj.mass + o.o.physicsObj.mass)))
                            )
                |> Seq.append worldCollisions
                |> Seq.toArray

            if collisions.Length = 0 then
                None
            else
                Some (
                    o,
                    collisions
                    |> Collections.Array.reduce Vec2<_>.lenMin
                )
            )

    // mutability bad but also itd be more comfy :skull:
    for obj, v in objects do
        // reflect velocity along our axis
        // https://math.stackexchange.com/a/13263
        let d = obj.o.physicsObj.velocity * 1.<_>
        let n = v.norm
        let r = d - ((d * n) * 2.) * n
        
        obj.o <-
            {obj.o with
                physicsObj =
                    {obj.o.physicsObj with
                        velocity = r * 1.<_> * obj.o.physicsObj.restitutionCoeff
                        pos =
                            obj.o.physicsObj.pos
                            + (Vec2.map Units.floatToTyped v)}}*)

let runPhysicsTick engine timeStep =
    // EWWWW MUTABILITY
    for wrapped in engine.scene.objects do
        let o = wrapped.o
        wrapped.o <- {o with physicsObj = Simulator.updateObjectPos o.physicsObj timeStep}

    collideAllObjects engine timeStep

    let hooks = engine.scene.postTickHooks

    for h in hooks do
        h (engine.scene, timeStep, engine.tickEventCache)
    
    engine.tickEventCache <- []

let createEngine scene =
    // i love hacks
    // fable doesnt assign instance members to the prototype so thats a nope
    // and it wraps funcs in a record in arrow functions so `jsThis` won't work.

    let mutable this = Unchecked.defaultof<_> // actually just `null`

    let eventHandler (e: Event) =
        let maybeGObj = this.gObjMountedCache.TryGet2 (e.target :?> HTMLElement)
        match maybeGObj with
        | None -> ()
        | Some(go) ->
            for h in go.o.eventHandlers do
                h (e.``type``, e, Some go)

        let resolvedEvent = e.``type``, e, maybeGObj
        
        this.tickEventCache <- resolvedEvent::this.tickEventCache
        this.frameEventCache <- resolvedEvent::this.frameEventCache
        
        for h in this.scene.eventHandlers do h resolvedEvent
    
    this <-
        {scene = scene
         running = false
         mounted = None
         lastTick = 0

         gObjMountedCache = DoubleDict()
         collisionCache = Dictionary()
         
         tickEventCache = []
         frameEventCache = []
         
         queryCollision = (fun o1 o2 ->
             (this.collisionCache.ContainsKey o1 && (this.collisionCache[o1].Contains o2))
             || (this.collisionCache.ContainsKey o2 && (this.collisionCache[o2].Contains o1)))

         mount = (fun elem ->
             for eventName in eventNames do
                 elem.addEventListener(eventName, eventHandler)
                 
             this.mounted <- Some elem
             )

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

                        for h in this.scene.postFrameHooks do h (this.scene, this.frameEventCache)
                        
                        this.frameEventCache <- []

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
                                    let timeStep = calcTStep tick
                                    this.lastTick <- tick

                                    runPhysicsTick this (timeStep / 1000.<_>))
                                (int (1000. / physicsHz))
                        )

                this.stop <-
                    (fun () ->
                        cancel <- true
                        this.running <- true
                        // clear the interval if intervalCode is Some(int), do nothing if None
                        Option.map clearInterval intervalCode |> ignore
                        this.stop <- defaultStopFunc)

                this.lastTick <- performance.now ()
                this.running <- true)}

    this