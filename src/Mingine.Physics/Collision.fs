module Mingine.Physics.Collision

open Mingine.Types
open Mingine

let private getOsetRectPoints (bl, tr) pos angle =
    [bl
     tr
     {x = bl.x; y = tr.y}
     {x = tr.x; y = bl.y}]
    |> List.map (fun p -> pos + (p.rotate angle Vec2.origin))

let private getRectNormals (bl, tr) angle =
    //let tl = {x = bl.x; y = tr.y}
    let br = {x = tr.x; y = bl.y}
    //let planeT = tr - tl
    let planeR = tr - br
    let planeB = br - bl
    //let planeL = tl - bl

    (*let normT =
        -(planeT.rotate angle Vec2.origin).perp.norm*)

    let normR =
        (planeR.rotate angle Vec2.origin).perp.norm

    let normB =
        (planeB.rotate angle Vec2.origin).perp.norm

    (*let normL =
        -(planeL.rotate angle Vec2.origin).perp.norm*)

    [|(*normT;*) normR; normB(*; normL*)|]


let private absMin v1 v2 = if (abs v1) < (abs v2) then v1 else v2

let private absMin2 v1 v2 =
    if (abs (fst v1)) < (abs (fst v2)) then
        v1
    else v2

let private absMax2 v1 v2 =
    if (abs (fst v1)) > (abs (fst v2)) then
        v1
    else v2

let private minOptionsAnd o1 o2 =
    match o1 with
    | None -> None
    | Some v1 ->
        match o2 with
        | None -> None
        | Some v2 -> Some(absMin2 v1 v2)
        
let private minOptionsOr o1 o2 =
    match o1 with
    | None ->
        match o2 with
        | None -> None
        | Some r2 -> Some r2
    | Some v1 ->
        match o2 with
        | None -> Some v1
        | Some v2 -> Some(absMin2 v1 v2)

let private maxOptionsOr o1 o2 =
    match o1 with
    | None ->
        match o2 with
        | None -> None
        | Some r2 -> Some r2
    | Some v1 ->
        match o2 with
        | None -> Some v1
        | Some v2 -> Some(absMax2 v1 v2)

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
        axis * center |> Units.typedToTyped

    centerPoint - rad |> Units.typedToFloat, centerPoint + rad |> Units.typedToFloat

/// checks if two projections overlap. the pairs of values are expected to be (min, max)
let checkProjectionOverlap (min1, max1) (min2, max2) =
    // some of these tests may be unnecessary but that's what short circuits are for
    (min1 < min2 && min2 < max1) // min2 is inside range 1
    || (min1 < max2 && max2 < max1) // max2 is inside range 1
    || (min2 < min1 && min1 < max2) // min1 is inside range 2
    || (min2 < max1 && max1 < max2) // max1 is inside range 2

/// checks if two projections overlap and returns how much they overlap if they do
let getProjectionOverlap (min1, max1) (min2, max2) =
    if checkProjectionOverlap (min1, max1) (min2, max2) then
        // textbook impl, but this is always positive!
        // Some <| (min max1 max2) - (max min1 min2)
        // my impl after some experimenting, can return negative!
        Some <| absMin (min2 - max1) (max2 - min1)
    else
        None

/// gets MTV if a circle collider is colliding with another given collider
let rec collideWithCircle (rad, center) pos collider otherPos otherAngle =
    match collider with
    | NullCollider -> None
    | CompositeCollider (a, b) ->
        maxOptionsOr
            (collideWithCircle (rad, center) pos a otherPos otherAngle)
            (collideWithCircle (rad, center) pos b otherPos otherAngle)

    | PlaneCollider axis ->
        // without * -1 the normal is clockwise of the line
        // this is an entirely arbitrary decision however with it this way,
        // an axis of (1, 0) makes a usable floor, which is intuitive.
        let normal = axis.perp.norm * -1.
        
        let proj1, proj2 = projectCircleToAxis normal (rad, pos + center)
        
        if proj1 < 0 then Some(abs proj1, normal)
        else if proj2 < 0 then Some(abs proj2, normal)
        else None
    
    | CircularCollider (otherRad, otherCenter) ->
        let vecToOther = (otherPos + otherCenter) - (pos + center)
        
        let overlapAmount =
            (rad + otherRad) - (abs vecToOther.len)

        if overlapAmount > 0.<_> then
            Some(Units.typedToFloat -overlapAmount, vecToOther * 1.<_>)
        else
            None

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

        let circleSpecificCheck =
            match getProjectionOverlap circProj recProj with
            | None -> None
            | Some olap -> Some(olap, axis)

        let rectCheck =
            getRectNormals (bl, tr) otherAngle
            |> Array.distinct
            |> Array.fold
                (fun curr axis ->
                    match curr with
                    | None -> None
                    | Some c ->
                        let p = 
                            getProjectionOverlap
                               (projectPolygonToAxis axis points)
                               (projectCircleToAxis axis (rad, center + pos))

                        match p with
                        | None -> None
                        | Some rp -> Some(absMin2 (rp, -axis) c)
                    )
                (Some(infinity, Vec2.origin))

        minOptionsAnd
            circleSpecificCheck
            rectCheck // i dont think this is necessary for collision but it is for accurate MTV!!

/// gets MTV if a rect collider is colliding with another given collider
let rec collideWithRect (bl, tr) pos angle collider otherPos otherAngle =
    match collider with
    | NullCollider -> None
    | CompositeCollider (a, b) ->
        maxOptionsOr
            (collideWithRect (bl, tr) pos angle a otherPos otherAngle)
            (collideWithRect (bl, tr) pos angle b otherPos otherAngle)

    | PlaneCollider axis ->
        let normal = axis.perp.norm * -1.
        
        let myPoints = getOsetRectPoints (bl, tr) pos angle
        
        let proj1, proj2 = projectPolygonToAxis normal myPoints
        
        if proj1 < 0 then Some(abs proj1, normal)
        else if proj2 < 0 then Some(abs proj2, normal)
        else None
    
    | CircularCollider (r, c) -> collideWithCircle (r, c) otherPos (RectCollider(bl, tr)) pos angle

    | RectCollider (bl2, tr2) ->
        let myPoints =
            getOsetRectPoints (bl, tr) pos angle

        let theirPoints =
            getOsetRectPoints (bl2, tr2) otherPos otherAngle

        Array.append (getRectNormals (bl, tr) angle) (getRectNormals (bl2, tr2) otherAngle)
        |> Array.distinct
        |> Array.fold
                (fun curr axis ->
                    match curr with
                    | None -> None
                    | Some c ->
                        let p = 
                            getProjectionOverlap
                               (projectPolygonToAxis axis myPoints)
                               (projectPolygonToAxis axis theirPoints)
                        
                        match p with
                        | None -> None
                        | Some rp -> Some(absMin2 (rp, axis) c)
                    )
                (Some(infinity, Vec2.origin))

let rec collideWithPlane axis c2 pos2 angle2 =
    match c2 with
    | NullCollider -> None
    | CompositeCollider(c2_1, c2_2) ->
        maxOptionsOr
            (collideWithPlane axis c2_1 pos2 angle2)
            (collideWithPlane axis c2_2 pos2 angle2)
        
    | PlaneCollider _ -> None // if infinite planes can collide with each other this would cause havoc
    | CircularCollider (r, c) -> collideWithCircle (r, c) pos2 (PlaneCollider axis) Vec2.origin 0.<_>
    | RectCollider (bl, tr) -> collideWithRect (bl, tr) pos2 angle2 (PlaneCollider axis) Vec2.origin 0.<_>

/// resolves an MTV into one vector
let resolveMTV (mag: float<_>, vec: Vec2<_>) = vec.norm * mag

/// gets MTV if two colliders collide
let rec collideColliders c1 c2 pos1 angle1 pos2 angle2 =
    match c1 with
    | NullCollider -> None
    | PlaneCollider axis -> collideWithPlane axis c2 pos2 angle2
    | CircularCollider (r, c) -> collideWithCircle (r, c) pos1 c2 pos2 angle2
    | RectCollider (bl, tr) -> collideWithRect (bl, tr) pos1 angle1 c2 pos2 angle2
    | CompositeCollider (a, b) ->
        maxOptionsOr
            (collideColliders a c2 pos1 angle1 pos2 angle2)
            (collideColliders b c2 pos1 angle1 pos2 angle2)

/// gets resolved vector if two objects collide
let collideGObjs c1 p1 c2 p2 =
    collideColliders
        c1
        c2
        p1.pos
        p1.angle
        p2.pos
        p2.angle
    |> Option.map resolveMTV