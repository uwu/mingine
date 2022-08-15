module MiniPhys.JsApi

open System.Collections.Generic
open Fable.Core
open Fable.Core.JS
open Browser.Dom
open Fable.Core.DynamicExtensions
open MiniPhys.Types
open Fable.Core.JsInterop

// its a symbol not a string but its gotta be usable for indexing
let private tsTag: string =
    window?Symbol?toStringTag

Vec2.origin?__proto__.[tsTag] <- "Vec2"
NullCollider?__proto__.[tsTag] <- "Collider"

let inline private backup v b = if isNullOrUndefined v then b else v

[<Global("Object.defineProperty")>]
let private defProp: obj -> string -> obj -> unit =
    jsNative

let private unwrapWrapped (wGObj: WrappedGObj) =
    for k in Constructors.Object.keys wGObj.o do
        defProp.Invoke(
            wGObj,
            k,
            {|enumerable = Some true
              get = Some(fun () -> wGObj.o[k])
              set = Some(fun v -> wGObj.o[k] <- v)|}
        )

    wGObj

let v x y =
    if jsTypeof x <> "number" || jsTypeof y <> "number" then
        failwith "cannot create a vector with non-number components!"

    {x = x; y = y}

let vo () = Vec2.origin

let createEngine = Engine.createEngine

let createScene obj =
    let mutable this = Unchecked.defaultof<_>

    this <-
        {scale = backup obj?scale 1.<_>
         rootStyles = backup obj?rootStyles {||}
         objects = HashSet(backup obj?objects [||])
         renderOffset = backup obj?renderOffset Vec2.origin
         canvasSize = backup obj?canvasSize Vec2.origin
         postTickHooks = backup obj?postTickHooks [||]}

    this?getObjects <- (fun () -> Seq.toArray this.objects)
    this?addObject <- this.objects.Add
    this?removeObject <- this.objects.Remove

    this

let createObject obj =
    if not obj?id then
        failwith "cannot create an object with empty ID"

    if not obj?blOffset then
        failwith "custom objects need a bottom-left offset setting"

    if not obj?mass then
        failwith "objects must have a mass"

    if not obj?momentOfInertia then
        failwith "objects must have a moment of inertia"

    WrappedGObj
        {id = obj?id
         layer = backup obj?layer 1
         blOffset = obj?blOffset
         styles = backup obj?styles {||}
         collider = backup obj?collider NullCollider
         physicsObj =
            {pos = backup obj?pos Vec2.origin
             mass = obj?mass
             velocity = backup obj?velocity Vec2.origin
             accel = backup obj?accel Vec2.origin
             forces = backup obj?forces [||]
             momentOfInertia = obj?momentOfInertia
             angle = backup obj?angle 0.<_>
             angVelocity = backup obj?angVelocity 0.<_>
             angAccel = backup obj?angAccel 0.<_>}}
    |> unwrapWrapped

let createCircle obj =
    if isNullOrUndefined obj?radius then
        failwith "circles must have a radius!"

    obj?collider <- if obj?collide then
                        CircularCollider(obj?radius, Vec2.origin)
                    else
                        NullCollider

    obj?blOffset <- {x = obj?radius; y = obj?radius}

    createObject obj

let createRect obj =
    if isNullOrUndefined obj?width
       || isNullOrUndefined obj?height then
        failwith "rects must have a width and height!"

    let bl =
        {x = obj?width / 2.<_>
         y = obj?height / 2.<_>}

    obj?collider <- if obj?collide then
                        RectCollider(bl, -bl)
                    else
                        NullCollider

    obj?blOffset <- bl

    createObject obj

let createColliderNull () = NullCollider
let createColliderComposite a b = CompositeCollider(a, b)
let createColliderCircle rad center = CircularCollider(rad, center)
let createColliderRect botLeft topRight = RectCollider(botLeft, topRight)

let forceModels =
    {|
     weight = (fun (gravity, gObj) -> ForceModels.weight gravity gObj)
     earthWeight = (fun gObj -> ForceModels.earthWeight gObj)
     spring = ForceModels.spring
     
      |}