module Mingine.Physics.Collision

open Mingine.Types
open Mingine
open FSharp.Data.UnitSystems.SI.UnitSymbols

let private getOsetRectPoints (bl, tr) pos angle =
    [bl
     tr
     {x = bl.x; y = tr.y}
     {x = tr.x; y = bl.y}]
    |> List.map (fun p -> pos + (p.rotate angle Vec2.origin))

let private getRectNormals (bl, tr) angle =
    let br = {x = tr.x; y = bl.y}
    let planeR = tr - br
    let planeB = br - bl

    let normR = (planeR.rotate angle Vec2.origin).perp.norm

    let normB = (planeB.rotate angle Vec2.origin).perp.norm

    [|normR; normB|]


let private fst3 (v, _, _) = v

let private absMin v1 v2 = if (abs v1) < (abs v2) then v1 else v2

let private absMin2 v1 v2 =
    if (abs (fst3 v1)) < (abs (fst3 v2)) then
        v1
    else v2

let private absMax2 v1 v2 =
    if (abs (fst3 v1)) > (abs (fst3 v2)) then
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

/// gets MTV if a plane collider is colliding with another given collider
let rec collideWithPlane (axis: Vec2<_>, oset) pos1 angle1 c2 pos2 angle2 =
    
    // without * -1 the normal is clockwise of the line
    // this is an entirely arbitrary decision however with it this way,
    // an axis of (1, 0) makes a usable floor, which is intuitive.
    let normal = axis.perp.norm * -1.
    
    let checkOlap (closestPoint: Vec2<m>) (proj1 : float, proj2 : float) =
        if proj1 < 0 then Some(abs proj1, normal, closestPoint)
        else if proj2 < 0 then Some(abs proj2, normal, closestPoint)
        else None
    
    match c2 with
    | NullCollider -> None
    | CompositeCollider(a, b) ->
        maxOptionsOr
            (collideWithPlane (axis, oset) pos1 angle1 a pos2 angle2)
            (collideWithPlane (axis, oset) pos1 angle1 b pos2 angle2)
    
    | PlaneCollider _ -> None // if infinite planes can collide with each other this would cause havoc
    | CircularCollider(rad, center) ->
        projectCircleToAxis normal (rad, pos2 + center - pos1 - oset)
        |> checkOlap Vec2.origin
    
    | RectCollider(bl, tr) ->
        let points = getOsetRectPoints (bl, tr) (pos2 - pos1 - oset) angle2
        let closestPoint = points |> List.minBy ((*) normal)
        
        projectPolygonToAxis normal points
        |> checkOlap closestPoint

/// gets MTV if a circle collider is colliding with another given collider
let rec collideWithCircle (rad, center) pos collider otherPos otherAngle =
    match collider with
    | NullCollider -> None
    | CompositeCollider (a, b) ->
        maxOptionsOr
            (collideWithCircle (rad, center) pos a otherPos otherAngle)
            (collideWithCircle (rad, center) pos b otherPos otherAngle)

    | PlaneCollider (axis, oset) ->
        collideWithPlane (axis, oset) otherPos otherAngle (CircularCollider(rad, center)) pos 0.<_>
    
    | CircularCollider (otherRad, otherCenter) ->
        let vecToOther = (otherPos + otherCenter) - (pos + center)
        
        let overlapAmount =
            (rad + otherRad) - (abs vecToOther.len)

        if overlapAmount > 0.<_> then
            Some(
                Units.typedToFloat -overlapAmount,
                vecToOther * 1.<_>,
                (pos + center) + (vecToOther / 2.))
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
            | Some olap -> Some(olap, axis, closestPoint)

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
                        | Some rp -> Some(absMin2 (rp, -axis, closestPoint) c)
                    )
                (Some(infinity, Vec2.origin, closestPoint))

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

    | PlaneCollider (axis, oset) -> collideWithPlane (axis, oset) otherPos otherAngle (RectCollider(bl, tr)) pos angle
    
    | CircularCollider (r, c) -> collideWithCircle (r, c) otherPos (RectCollider(bl, tr)) pos angle

    | RectCollider (bl2, tr2) ->
        let myPoints =
            getOsetRectPoints (bl, tr) pos angle

        let theirPoints =
            getOsetRectPoints (bl2, tr2) otherPos otherAngle

        let closestPoint =
            myPoints
            |> List.minBy (fun p1 ->
                theirPoints
                |> List.minBy (fun p2 -> (p2 - p1).len)
                )
        
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
                        | Some rp -> Some(absMin2 (rp, axis, closestPoint) c)
                    )
                (Some(infinity, Vec2.origin, closestPoint))

/// resolves an MTV into one vector
let resolveMTV (mag: float<_>, vec: Vec2<_>, point: Vec2<_>) = vec.norm * mag, point

/// gets MTV if two colliders collide
let rec collideColliders c1 c2 pos1 angle1 pos2 angle2 =
    match c1 with
    | NullCollider -> None
    | PlaneCollider (axis, oset) -> collideWithPlane (axis, oset) pos1 angle1 c2 pos2 angle2
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

// TODO: make the returned magnitudes in metres or something

let private signf x = if x > 0.<_> then 1. else -1.

// https://math.stackexchange.com/a/13263
let private reflectOver<[<Measure>] 'u> (vec: Vec2<'u>) (normal: Vec2<_>): Vec2<'u> =
    let d = vec |> Vec2.map Units.typedToFloat
    let n = normal.norm
    
    d - ((d * n) * 2.) * n
    |> Vec2.map Units.floatToTyped

let respondToCollision p1 (mtv: Vec2<_>) m2 av2 point tStep =
    let velFromAngular1 = (point - p1.pos).perp * (Units.mapFloatTyped tan (p1.angVelocity / 1.<Units.rad>))
    let vel1 = p1.velocity + velFromAngular1
    
    let reflectedVel = reflectOver vel1 mtv
    
    let massProportion =
        if p1.mass = (infinity * 1.<_>) then
            0.
        else
            if m2 = (infinity * 1.<_>) then
                1.
            else m2 / (p1.mass + m2)
    
    // generate torque - first we need a force!
    let velocityDiff = reflectedVel - vel1
    // generate an acceleration that will apply this velocity diff next frame
    let neededAccel = velocityDiff / tStep
    let force = neededAccel * p1.mass
    let torque = (force +* (point - p1.pos)) * 0.1
    let neededAngAccel = torque / p1.momentOfInertia * 1.<Units.rad>
    let angVelChange = neededAngAccel * tStep
    
    let finalVelocity = reflectedVel - (reflectOver velFromAngular1 mtv)
    
    {p1 with
        pos = p1.pos + mtv
        velocity = finalVelocity * p1.restitutionCoeff * massProportion
        angVelocity = p1.angVelocity + angVelChange}

// TODO: ensure all "closest points" are definitely right 