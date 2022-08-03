module MiniPhys.Collision

open MiniPhys.Types

let private getOsetRectPoints (bl, tr) pos angle =
    [bl
     tr
     {x = bl.x; y = tr.y}
     {x = tr.x; y = bl.y}]
    |> List.map (fun p -> pos + (p.rotate angle Vec2.origin))

let private getRectNormals (bl, tr) angle =
    let tl = {x = bl.x; y = tr.y}
    let br = {x = tr.x; y = bl.y}
    let planeT = tr - tl
    let planeR = tr - br
    let planeB = br - bl
    let planeL = tl - bl

    let rot1 =
        (planeT.rotate angle Vec2.origin).perp.norm

    let rot2 =
        (planeR.rotate angle Vec2.origin).perp.norm

    let rot3 =
        (planeB.rotate angle Vec2.origin).perp.norm

    let rot4 =
        (planeL.rotate angle Vec2.origin).perp.norm

    //rot1, rot2, rot3, rot4
    [|rot1; rot2; rot3; rot4|]

/// takes an axis and a list of points, returns the min and max point of the projection
let projectPolygonToAxis (axis: Vec2<_>) (points: list<_>) =
    let startVal =
        axis * points[0] |> Units.typedToFloat

    points
    |> List.skip 1
    |> List.fold
        (fun (currMin, currMax) point ->
            let proj =
                axis * point |> Units.typedToFloat

            min proj currMin, max proj currMax)
        (startVal, startVal)

/// takes an axis and circle, returns the min and max point of the projection
let projectCircleToAxis (axis: Vec2<_>) (rad, center: Vec2<_>) =
    let centerPoint =
        axis * center |> Units.typedToFloat

    centerPoint - (Units.typedToFloat rad), centerPoint + (Units.typedToFloat rad)

/// checks if two projections overlap. the pairs of values are expected to be (min, max)
let checkProjectionOverlap (min1, max1) (min2, max2) =
    // some of these tests may be unnecessary but that's what short circuits are for
    (min1 < min2 && min2 < max1) // min2 is inside range 1
    || (min1 < max2 && max2 < max1) // max2 is inside range 1
    || (min2 < min1 && min1 < max2) // min1 is inside range 2
    || (min2 < max1 && max1 < max2) // max1 is inside range 2

/// checks if a circle collider is colliding with another given collider
let rec collidesWithCircle (rad, center) pos collider otherPos otherAngle =
    match collider with
    | CompositeCollider (a, b) ->
        collidesWithCircle (rad, center) pos a otherPos otherAngle
        || collidesWithCircle (rad, center) pos b otherPos otherAngle

    | CircularCollider (otherRad, otherCenter) ->
        let distance =
            abs ((otherPos + otherCenter) - (pos + center)).len

        distance > (rad + otherRad)

    | RectCollider (bl, tr) ->
        let points =
            getOsetRectPoints (bl, tr) otherPos otherAngle


        let closestPoint =
            points
            |> List.minBy (fun p -> (center + pos - p).len)
        
        let axis =
            (center + pos - closestPoint).norm

        let circProj =
            projectCircleToAxis axis (rad, center + pos)

        let recProj =
            projectPolygonToAxis axis points

        let circleSpecificCheck = checkProjectionOverlap circProj recProj
        
        let rectCheck =
            getRectNormals (bl, tr) otherAngle
            |> Array.distinct
            |> Array.forall (fun axis ->
                checkProjectionOverlap
                    (projectPolygonToAxis axis points)
                    (projectCircleToAxis axis (rad, center + pos))
                )
        
        circleSpecificCheck && rectCheck

/// checks if a rect collider is colliding with another given collider
let rec collidesWithRect (bl, tr) pos angle collider otherPos otherAngle =
    match collider with
    | CompositeCollider (a, b) ->
        collidesWithRect (bl, tr) pos angle a otherPos otherAngle
        || collidesWithRect (bl, tr) pos angle b otherPos otherAngle

    | CircularCollider (r, c) -> collidesWithCircle (r, c) otherPos (RectCollider(bl, tr)) pos angle

    | RectCollider (bl2, tr2) ->
        let myPoints =
            getOsetRectPoints (bl, tr) pos angle

        let theirPoints =
            getOsetRectPoints (bl2, tr2) otherPos otherAngle

        Array.append (getRectNormals (bl, tr) angle) (getRectNormals (bl2, tr2) otherAngle)
        |> Array.distinct
        |> Array.forall (fun axis ->
            let proj1 =
                projectPolygonToAxis axis myPoints

            let proj2 =
                projectPolygonToAxis axis theirPoints

            checkProjectionOverlap proj1 proj2)

/// checks if two colliders collide
let rec checkColliderCollision c1 c2 pos1 angle1 pos2 angle2 =
    match c1 with
    | CircularCollider (r, c) -> collidesWithCircle (r, c) pos1 c2 pos2 angle2
    | RectCollider (bl, tr) -> collidesWithRect (bl, tr) pos1 angle1 c2 pos2 angle2
    | CompositeCollider (a, b) ->
        checkColliderCollision a c2 pos1 angle1 pos2 angle2
        || checkColliderCollision b c2 pos1 angle1 pos2 angle2

/// checks if two objects collide
let checkGObjCollision gO1 gO2 =
    checkColliderCollision
        gO1.collider
        gO2.collider
        gO1.physicsObj.pos
        gO1.physicsObj.angle
        gO2.physicsObj.pos
        gO2.physicsObj.angle