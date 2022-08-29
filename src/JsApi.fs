module MiniPhys.JsApi

open System.Collections.Generic
open Fable.Core
open Fable.Core.JS
open Browser.Dom
open Fable.Core.DynamicExtensions
open MiniPhys.Types
open Fable.Core.JsInterop

let private nothingFunc () = ()

// its a symbol not a string but its gotta be usable for indexing
let private tsTag: string =
    window?Symbol?toStringTag

Vec2.origin?__proto__.[tsTag] <- "Vec2"
NullCollider?__proto__.[tsTag] <- "Collider"

let private v2a other = (jsThis: Vec2<_>) + other
let private v2s other = (jsThis: Vec2<_>) - other
let private v2neg () = -(jsThis: Vec2<_>)
let private v2dm other = (jsThis: Vec2<_>) * (other: Vec2<_>)
let private v2cm other = (jsThis: Vec2<_>) +* (other: Vec2<_>)
let private v2sm other = (jsThis: Vec2<_>) * (other: float<_>)
let private v2sd other = (jsThis: Vec2<_>) / other

Constructors.Object.assign (
    Vec2.origin?__proto__,
    {|add = v2a
      sub = v2s
      neg = v2neg
      dot = v2dm
      cross = v2cm
      scale = v2sm
      scdiv = v2sd|}
)
|> ignore

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

// these being here is needed for fun fable reasons
let private scene_getOs () = Seq.toArray jsThis.objects
let private scene_addO o = jsThis.objects.Add o
let private scene_removeO o = jsThis.objects.Remove o

let createScene obj =
    let mutable this =
        {scale = backup obj?scale 1.<_>
         rootStyles = backup obj?rootStyles {||}
         objects = HashSet(backup obj?objects [||])
         renderOffset = backup obj?renderOffset Vec2.origin
         canvasSize = backup obj?canvasSize Vec2.origin
         postTickHooks = backup obj?postTickHooks [||]}

    // theres a more efficient way to do this but im tired
    this?__proto__?getObjects <- scene_getOs
    this?__proto__?addObject <- scene_addO
    this?__proto__?removeObject <- scene_removeO

    // UNCOMMMENT IF YOU EVER NEED VISUALISATION FOR DEBUG PURPOSES
    //this <- Visualiser.initVis this
    //window?REPORT_VEC <- Visualiser.createVisVec this Vec2.origin Vec2.origin
    
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
             angAccel = backup obj?angAccel 0.<_>
             restitutionCoeff = backup obj?restitutionCoeff 1.}}
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

let getCollision (g1: WrappedGObj) (g2: WrappedGObj) = Collision.collideGObjs g1.o g2.o

let areColliding g1 g2 = getCollision g1 g2 |> Option.isSome