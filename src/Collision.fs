module MiniPhys.Collision

open System
open MiniPhys.Types
open FSharp.Data.UnitSystems.SI.UnitSymbols

// bottom left, top right
type AABB = Vec2<m> * Vec2<m>

let private vecMin = Vec2<_>.map2 min
let private vecMax = Vec2<_>.map2 max

/// rotates a rectangle and gets its axis-aligned bounding box <> -> []
let aabbOfRotatedRect (rawBL, rawTR) angle : AABB =
    // we need all 4 corners to turn a rotated rectangle into an aligned rectangle
    let rawTL, rawBR = { x = rawBL.x; y = rawTR.y }, { x = rawTR.x; y = rawBL.y } 
    let rotated1, rotated2, rotated3, rotated4 =
        rawTL.rotate angle Vec2.origin,
        rawTR.rotate angle Vec2.origin,
        rawBR.rotate angle Vec2.origin, 
        rawBL.rotate angle Vec2.origin
    
    vecMin (vecMin rotated1 rotated2) (vecMin rotated3 rotated4),
    vecMax (vecMax rotated1 rotated2) (vecMax rotated3 rotated4)

/// gets an unrotated rectangular bounding box from a collider
let rec unalignedBoundingBox =
    function
    | RectCollider (bl, tr) -> bl, tr : AABB
    | CircularCollider (rad, center) ->
        let radVec = { x = rad; y = rad }
        center - radVec, center + radVec
    | CompositeCollider (a, b) ->
        let b1 = unalignedBoundingBox a
        let b2 = unalignedBoundingBox b
        
        vecMin (fst b1) (fst b2), vecMax (snd b1) (snd b2)


/// gets an axis-aligned rectangular bounding box from a gObj
let aabbFromGObj gObj =
    let rawBB = unalignedBoundingBox gObj.collider
    aabbOfRotatedRect rawBB gObj.physicsObj.angle


/// checks if a circle collider is colliding with another given collider
let rec collidesWithCircle (rad, center) pos collider otherPos otherAngle =
    match collider with
    | CompositeCollider (a, b) ->
        collidesWithCircle (rad, center) pos a otherPos otherAngle
        || collidesWithCircle (rad, center) pos b otherPos otherAngle

    | CircularCollider (otherRad, otherCenter) ->
        let distance = abs ((otherPos + otherCenter) - (pos + center)).len
        distance > (rad + otherRad)
    
    | RectCollider (BL, TR) ->
        let relativeBL = BL + otherPos - pos
        let relativeTR = TR + otherPos - pos
        raise (NotImplementedException())