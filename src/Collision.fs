module MiniPhys.Collision

open MiniPhys.Types
(*open FSharp.Data.UnitSystems.SI.UnitSymbols

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
    aabbOfRotatedRect rawBB gObj.physicsObj.angle*)


let inline tupleMap f (v1, v2) = f v1, f v2

let private getRectPlanes (bl, tr) angle origin =
    let tl = { x = bl.x; y = tr.y }
    let br = { x = tr.x; y = bl.y }
    let planeT = tr, tl // these would be vectors but its more useful to have them as two points
    let planeR = tr, br
    let planeB = br, bl
    let planeL = tl, bl
    
    let rotatePlane = tupleMap (fun (p: Vec2<_>) -> p.rotate angle origin)
    let rot1 = rotatePlane planeT
    let rot2 = rotatePlane planeR
    let rot3 = rotatePlane planeB
    let rot4 = rotatePlane planeL
    
    rot1, rot2, rot3, rot4

let doesPlaneCollideCircle (rad, center: Vec2<_>) plane =
    // calculate vector from start of line to circle centre
    // form right angle triangle by projecting circle center to line
    // find theta
    // apply trigonometry to find circle -> line length
    // check if shorter than circle radius
    let scVec = center - (fst plane)
    let theta = scVec.angleTo (snd plane - fst plane) |> Units.typedToFloat
    let projectedLen = scVec.len * (sin theta)
    projectedLen <= rad


let planesCollide (a, b) (c, d) =
    // dont even question it
    // https://math.stackexchange.com/a/3981906
    let ccw a b c = ((c.y - a.y) * (b.x - a.x)) > ((b.y - a.y) * (c.x - a.x))
    ((ccw a c d) <> (ccw b c d)) && ((ccw a b c) <> (ccw a b d))

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
        let absBL, absTR = BL + otherPos, TR + otherPos
        
        let plane1, plane2, plane3, plane4 = getRectPlanes (absBL, absTR) otherAngle otherPos
        
        let inline check p = doesPlaneCollideCircle (rad, center + pos) p
        
        check plane1 || check plane2 || check plane3 || check plane4

let rec collidesWithRect (bl, tr) pos angle collider otherPos otherAngle =
    match collider with
    | CompositeCollider (a, b) ->
        collidesWithRect (bl, tr) pos angle a otherPos otherAngle
        || collidesWithRect (bl, tr) pos angle b otherPos otherAngle
    
    | CircularCollider (r, c) -> collidesWithCircle (r, c) otherPos (RectCollider (bl, tr)) pos angle
    
    | RectCollider (bl2, tr2) ->
        let absBL1, absTR1 = bl + pos, tr + pos
        let absBL2, absTR2 = bl2 + otherPos, tr2 + otherPos
        
        let plane1_1, plane1_2, plane1_3, plane1_4 = getRectPlanes (absBL1, absTR1) angle pos
        let plane2_1, plane2_2, plane2_3, plane2_4 = getRectPlanes (absBL2, absTR2) otherAngle otherPos
        
        let check1 = planesCollide plane1_1
        let check2 = planesCollide plane1_2
        let check3 = planesCollide plane1_3
        let check4 = planesCollide plane1_4
        
        check1 plane2_1 || check1 plane2_2 || check1 plane2_3 || check1 plane2_4
        || check2 plane2_1 || check2 plane2_2 || check2 plane2_3 || check2 plane2_4
        || check3 plane2_1 || check3 plane2_2 || check3 plane2_3 || check3 plane2_4
        || check4 plane2_1 || check4 plane2_2 || check4 plane2_3 || check4 plane2_4

let rec checkColliderCollision c1 c2 pos1 angle1 pos2 angle2 =
    match c1 with
    | CompositeCollider (a, b) ->
        checkColliderCollision a c2 pos1 angle1 pos2 angle2
        || checkColliderCollision b c2 pos1 angle1 pos2 angle2

    | CircularCollider (r, c) -> collidesWithCircle (r, c) pos1 c2 pos2 angle2
    | RectCollider (bl, tr) -> collidesWithRect (bl, tr) pos1 angle1 c2 pos2 angle2

let checkGObjCollision gO1 gO2 =
    checkColliderCollision
        gO1.collider
        gO2.collider
        gO1.physicsObj.pos
        gO1.physicsObj.angle
        gO2.physicsObj.pos
        gO2.physicsObj.angle