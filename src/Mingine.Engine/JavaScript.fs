module Mingine.JavaScript

open System.Collections.Generic
open Fable.Core
open Fable.Core.JS
open Browser.Dom
open Fable.Core.DynamicExtensions
open Mingine.Types
open Mingine.Engine.Types
open Mingine
open Mingine.Physics
open Fable.Core.JsInterop

// workaround for F# compiler internal error kekw
let private vecOrigin = { x = 0.<_>; y = 0.<_> } // Vec2.origin

[<Emit("function(...a){return($0)(this, ...a)}")>]
let private captureThis<'this, 'a, 'r> (_: 'this -> 'a -> 'r): 'a -> 'r  = jsNative

///////////////////////////////////////
// BEGIN IMPERATIVE MODULE LOAD CODE //
///////////////////////////////////////

// its a symbol not a string but its gotta be usable for indexing
let private tsTag: string =
    window?Symbol?toStringTag

jsConstructor<Vec2<_>>?prototype.[tsTag] <- "Vec2"
jsConstructor<Collider>?prototype.[tsTag] <- "Collider"
jsConstructor<PhysicsObj>?prototype.[tsTag] <- "PhysicsObj"
jsConstructor<GameObj>?prototype.[tsTag] <- "GameObj (raw)"
jsConstructor<WrappedGObj>?prototype.[tsTag] <- "GameObj (wrapped)"
jsConstructor<Scene>?prototype.[tsTag] <- "Scene"
jsConstructor<Engine.EngineWrap>?prototype.[tsTag] <- "Engine"

// see below comment for why pre-captureThis method used
let private v2rot angle other = (jsThis: Vec2<_>).rotate angle other

Constructors.Object.assign (
    jsConstructor<Vec2<_>>?prototype,
    {|add = captureThis (fun (this: Vec2<_>) other -> this + other)
      sub = captureThis (fun (this: Vec2<_>) other -> this - other)
      neg = captureThis (fun (this: Vec2<_>) () -> -this)
      dot = captureThis (fun (this: Vec2<_>) (other: Vec2<_>) -> this * other)
      cross = captureThis (fun (this: Vec2<_>) (other: Vec2<_>) -> this +* other)
      scale = captureThis (fun (this: Vec2<_>) (other: float) -> this * other)
      scdiv = captureThis (fun (this: Vec2<_>) other -> this / other)
      len = captureThis (fun (this: Vec2<_>) () -> this.len)
      rotate = v2rot // uncurrying screws me otherwise
      norm = captureThis (fun (this: Vec2<_>) () -> this.norm)
      angleTo = captureThis (fun (this: Vec2<_>) other -> this.angleTo other)
      perp = captureThis (fun (this: Vec2<_>) () -> this.perp)|}
)
|> ignore

Constructors.Object.assign (
    jsConstructor<PhysicsObj>?prototype,
    {|impulse = captureThis (fun this force ->
            Constructors.Object.assign(this, Simulator.impulse force this) |> ignore
            )
      osetImpulse = captureThis (fun this force origin ->
            Constructors.Object.assign(this, Simulator.osetImpulse force origin this) |> ignore
            )|}
)
|> ignore


/////////////////////////////////////
// END IMPERATIVE MODULE LOAD CODE //
//       BEGIN API EXPORTS         //
/////////////////////////////////////

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

let vAngle theta len =
    if jsTypeof theta <> "number" then
        failwith "cannot create a vector from a non-number angle!"
    
    let scale = if jsTypeof len = "number" then len else 1.
    
    (Vec2<_>.atAngle theta) * scale

let vo () = vecOrigin

let createEngine = Engine.createEngine

jsConstructor<Scene>?prototype?getObjects <- captureThis (fun this () -> Seq.toArray this.objects)
jsConstructor<Scene>?prototype?addObject <- captureThis (fun this o -> this.objects.Add o)
jsConstructor<Scene>?prototype?removeObject <- captureThis (fun this o -> this.objects.Remove o)

let createScene obj =
    {scale = backup obj?scale 1.<_>
     rootStyles = backup obj?rootStyles {||}
     objects = HashSet(backup obj?objects [||])
     worldColliders = backup obj?worldColliders [||]
     renderOffset = backup obj?renderOffset vecOrigin
     canvasSize = backup obj?canvasSize vecOrigin
     postTickHooks = backup obj?postTickHooks [||]
     postFrameHooks = backup obj?postFrameHooks [||]
     eventHandlers = backup obj?eventHandlers [||]}

let createObject obj =
    if not obj?id then
        failwith "cannot create an object with empty ID"

    if not obj?blOffset then
        failwith "custom objects need a bottom-left offset setting"

    if isNullOrUndefined obj?mass then
        failwith "objects must have a mass"

    if isNullOrUndefined obj?momentOfInertia then
        failwith "objects must have a moment of inertia"

    WrappedGObj
        {id = obj?id
         layer = backup obj?layer 1
         blOffset = obj?blOffset
         styles = backup obj?styles {||}
         collider = backup obj?collider NullCollider
         physicsObj =
            {pos = backup obj?pos vecOrigin
             mass = obj?mass
             velocity = backup obj?velocity vecOrigin
             accel = backup obj?accel vecOrigin
             forces = backup obj?forces [||]
             momentOfInertia = obj?momentOfInertia
             angle = backup obj?angle 0.<_>
             angVelocity = backup obj?angVelocity 0.<_>
             angAccel = backup obj?angAccel 0.<_>
             restitutionCoeff = backup obj?restitutionCoeff 1.}
         eventHandlers = backup obj?eventHandlers [||]}
    |> unwrapWrapped

let createCircle obj =
    if isNullOrUndefined obj?radius then
        failwith "circles must have a radius!"

    obj?collider <- if obj?collide then
                        CircularCollider(obj?radius, vecOrigin)
                    else
                        NullCollider

    obj?blOffset <- {x = obj?radius; y = obj?radius}
    
    // 1/2 mr^2
    obj?momentOfInertia <- obj?mass * (obj?radius * obj?radius) / 2

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
    
    // 1/12 m (h^2 + w^2)
    obj?momentOfInertia <- (float obj?mass * (obj?height * obj?height + obj?width * obj?width)) / 12.

    createObject obj

let createColliderNull () = NullCollider
let createColliderPlane axis oset = PlaneCollider(axis, oset |> Option.defaultValue vecOrigin)
let createColliderComposite a b = CompositeCollider(a, b)
let createColliderCircle rad center = CircularCollider(rad, center)
let createColliderRect botLeft topRight = RectCollider(botLeft, topRight)

let forceModels =
    // using delegates can force currying in fable
    // https://github.com/fable-compiler/Fable/issues/815#issuecomment-294296159
    {|weight = (fun gravity -> (ForceModels.weight gravity): System.Func<_, _>)
      spring =
        (fun sprConst restPos connectionOset -> (ForceModels.spring sprConst restPos connectionOset): System.Func<_, _>)
      airDrag =
        (fun flowVel density csArea dragCoeff ->
            (ForceModels.airDrag flowVel density csArea dragCoeff): System.Func<_, _>)
      stillAirDrag =
        (fun density csArea dragCoeff -> (ForceModels.stillAirDrag density csArea dragCoeff): System.Func<_, _>)

      simpleDamping = (fun posRatio angRatio -> (ForceModels.simpleDamping posRatio angRatio): System.Func<_, _>)|}

let consts =
    {|earthGravity = ForceModels.earthGravity
      earthAirDensity = ForceModels.earthAirDensity|}