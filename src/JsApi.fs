module MiniPhys.JsApi

open System.Collections.Generic
open Fable.Core
open Fable.Core.JS
open Browser.Dom
open Fable.Core.DynamicExtensions
open MiniPhys.Types
open Fable.Core.JsInterop

// its a symbol not a string but its gotta be usable for indexing
let private tsTag : string = window?Symbol?toStringTag

Vec2.origin?__proto__.[tsTag] <- "Vec2"

let inline private backup v b = if isNullOrUndefined v then b else v

[<Emit("Object.defineProperty($1, $2, $3)")>]
let private defProp : obj -> string -> obj -> unit = jsNative

let private unwrapWrapped (wGObj: WrappedGObj) =
    for k in Constructors.Object.keys wGObj.o do
        defProp wGObj k {|
            enumerable = Some true
            get = Some (fun () ->  wGObj.o[k])
            set = Some (fun v -> wGObj.o[k] <- v)
        |}
    
    wGObj

let v x y =
    {x = x; y = y}

let createEngine = Engine.createEngine

let createScene obj =
    let mutable this = Unchecked.defaultof<_>
    
    this <- {
        scale = backup obj?scale 1.<_>
        rootStyles = backup obj?rootStyles {||}
        objects = HashSet(backup obj?objects [||])
        renderOffset = backup obj?renderOffset Vec2.origin
        canvasSize = backup obj?renderOffset Vec2.origin
        postTickHooks = backup obj?postTickHooks [] }
    
    this?getObjects <- (fun () -> Seq.toArray this.objects)
    this?addObject <- this.objects.Add
    this?removeObject <- this.objects.Remove
    
    this

let createObject obj =
    if not obj?id then
        failwith "cannot create an object with empty ID"
        
    if not obj?blOffset then
        failwith "custom objects need a bottom-left offset setting"
        
    WrappedGObj {
            id = obj?id
            physicsObj = failwith "todo"
            layer = backup obj?layer 1
            blOffset = obj?blOffset
            styles = backup obj?styles {||}
            collider = backup obj?collider NullCollider }
    |> unwrapWrapped

let createCircle obj =
    createObject {
                id = obj?id
                physicsObj = failwith "todo"
                layer = obj?layer
                blOffset = { x = obj?radius; y = obj?radius }
                styles = obj?styles
                collider = if obj?collide then CircularCollider(obj?radius, Vec2.origin) else NullCollider
            }

let createRect obj =
    let bl = { x = obj?width / 2.<_>; y = obj?height / 2.<_> }
    createObject {
                id = obj?id
                physicsObj = failwith "todo"
                layer = obj?layer
                blOffset = bl
                styles = obj?styles
                collider = if obj?collide then RectCollider(bl, -bl) else NullCollider
            }