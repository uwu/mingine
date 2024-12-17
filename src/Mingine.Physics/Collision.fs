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
    let br = {x = tr.x; y = bl.y}
    let planeR = tr - br
    let planeB = br - bl

    let normR = (planeR.rotate angle Vec2.origin).perp.norm

    let normB = (planeB.rotate angle Vec2.origin).perp.norm

    [|normR; normB|]


let private absMin v1 v2: float<_> = if (abs v1) < (abs v2) then v1 else v2

let private absMin2 v1 v2: float<_> * _ =
    if (abs (fst v1)) < (abs (fst v2)) then
        v1
    else v2

let private absMax2 v1 v2: float<_> * _ =
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
let projectPolygonToAxis (axis: Vec2<1>) (points: list<Vec2<_>>) =
    let startVal =
        axis * points[0]

    points
    |> List.skip 1
    |> List.fold
        (fun (currMin, currMax) point ->
            let proj =
                axis * point

            min proj currMin, max proj currMax)
        (startVal, startVal)

/// takes an axis and circle, returns the min and max point of the projection
let projectCircleToAxis (axis: Vec2<1>) (rad, center: Vec2<_>) =
    let centerPoint =
        axis * center

    centerPoint - rad, centerPoint + rad

/// checks if two projections overlap. the pairs of values are expected to be (min, max)
let checkProjectionOverlap (min1: float<_>, max1) (min2, max2) =
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
let rec collideWithPlane (axis: Vec2<1>, oset) pos1 _angle1 c2 pos2 angle2 =
    
    // without * -1 the normal is clockwise of the line
    // this is an entirely arbitrary decision however with it this way,
    // an axis of (1, 0) makes a usable floor, which is intuitive.
    let normal = axis.perp.norm * -1.
    
    let checkOlap (proj1, proj2) =
        if proj1 < 0.<_> then Some(abs proj1, normal)
        else if proj2 < 0.<_> then Some(abs proj2, normal)
        else None
    
    match c2 with
    | NullCollider -> None
    | CompositeCollider(a, b) ->
        maxOptionsOr
            (collideWithPlane (axis, oset) pos1 _angle1 a pos2 angle2)
            (collideWithPlane (axis, oset) pos1 _angle1 b pos2 angle2)
    
    | PlaneCollider _ -> None // if infinite planes can collide with each other this would cause havoc
    | CircularCollider(rad, center) ->
        projectCircleToAxis normal (rad, pos2 + center - pos1 - oset)
        |> checkOlap
    
    | RectCollider(bl, tr) ->
        let points = getOsetRectPoints (bl, tr) (pos2 - pos1 - oset) angle2
        
        projectPolygonToAxis normal points
        |> checkOlap

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
            Some(-overlapAmount, vecToOther * 1.<_>)
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
                (Some(infinity |> Units.floatToTyped, Vec2.origin))

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
                (Some(infinity |> Units.floatToTyped, Vec2.origin))

/// resolves an MTV into one vector
let resolveMTV (mag: float<_>, vec: Vec2<_>) = vec.norm * mag

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
let collideObjs c1 p1 c2 p2 =
    collideColliders
        c1
        c2
        p1.pos
        p1.angle
        p2.pos
        p2.angle
    |> Option.map resolveMTV

// Fable doesn't have F# 9's bool return support.
let inline (|ImmovablePObj|_|) p = if Units.typedToFloat p.mass = infinity then Some () else None

// given a pair of objects that may be colliding, resolve their collision
// you should NOT pass duplicate pairs! - TODO: find out if this is actually a bug causer or just plain slow
let rec resolveCollision c1 p1 c2 p2 =
    
    match p1 with
    | ImmovablePObj ->
        match p2 with
        | ImmovablePObj _ -> p1, p2 // both are infinite mass, no-op
        | _ ->
            let np1, np2 = resolveCollision c2 p2 c1 p1 // flip the args to deduplicate work
            np2, np1 // without this swap, the gameobjects swap their physicsobjects, which is very funny to watch
    | _ ->
        // first mass is definitely finite
        match collideObjs c1 p1 c2 p2 with
        | None -> p1, p2
        | Some(rawMtv) ->
            
            // TODO: just define this, and fix it where its generated
            // note about the raw MTV: it is not defined which object it must be applied to to stop the collision.
            // perform a test:
            let dstadd = ((p1.pos + rawMtv) - p2.pos).len
            //let dstsub = ((p1.pos - rawMtv) - p2.pos).len
            
            // add this mtv to p1, subtract it from p2
            let mtv =
                if dstadd > (p1.pos - p2.pos).len then
                    rawMtv // we were further away when we added it to p1, so we should add it to p1. 
                else
                    -rawMtv // we got closer when we added! we need to flip it.
            
            match p2 with
            | ImmovablePObj _ -> // one mass is infinite (the second one), so we have a simpler case for the collision
                
                let mtvNorm = mtv.norm
                let mtvNPerp = mtvNorm.perp
                
                // un-intersect the objects
                let newPos = p1.pos + mtv
                
                // see the comment below about this
                let restitution = p1.restitutionCoeff * p2.restitutionCoeff
                
                // reflect the velocity across the axis of the collision, and apply restitution
                let perpVel = p1.velocity * mtvNPerp
                let newParaVel = -restitution * (p1.velocity * mtvNorm)
                let newVel = mtvNorm * newParaVel + mtvNPerp * perpVel
                
                { p1 with pos = newPos; velocity = newVel }, p2
            | _ ->
                // two finite masses, standard collision case.
            
                let m1 = p1.mass
                let m2 = p2.mass
                
                // this is of course, not correct, you cannot assign an `e` to an individual surface/object, but it's good enough.
                // https://gamedev.net/forums/topic/321575-coefficients-of-restitution-and-friction/3070963/
                let e = p1.restitutionCoeff * p2.restitutionCoeff
                
                // don't recompute these tons of times
                let mtvNorm = mtv.norm
                let mtvNPerp = mtvNorm.perp
                
                // find perpendicular velocities, which will be untouched
                let perpv1 = p1.velocity * mtvNPerp
                let perpv2 = p2.velocity * mtvNPerp
                
                (*
                    for parallel to MTV direction (where u, v, w, x are VELOCITIES):
                    u  v
                    -> ->
                    w  x
                    
                    x - w == e (u - v)          (restitution eqn)
                    u m1 + v m2 == w m1 + x m2  (cons of momentum)
                    
                    => w = ( u (m1 - e m2) + v m2 (1 + e) ) / (m1 + m2)
                    &  x = ( v (m2 - e m1) + u m1 (1 + e) ) / (m1 + m2)
                     -- Thanks Wolfram Mathematica ;)
                     
                     for perpendicular, both objects keep their velocities
                *)
                
                // pre-compute re-used values
                let m1m2 = m1 + m2
                let e1 = e + 1.
                
                // project velocities to the parallel direction
                let u = p1.velocity * mtvNorm
                let v = p2.velocity * mtvNorm
                
                // perform collision
                let w = ( u * (m1 - e * m2) + v * m2 * e1 ) / m1m2
                let x = ( v * (m2 - e * m1) + u * m1 * e1 ) / m1m2
                
                // reconstitute velocities
                let finalv1 = mtvNorm * w + mtvNPerp * perpv1
                let finalv2 = mtvNorm * x + mtvNPerp * perpv2
                
                // apply MTV to objects to un-intersect them. Just do it proportionally to masses because its easy.
                let pos1 = p1.pos + (mtv * (m2 / m1m2))
                let pos2 = p2.pos - (mtv * (m1 / m1m2))
                
                let newP1 = { p1 with pos = pos1; velocity = finalv1 }
                let newP2 = { p2 with pos = pos2; velocity = finalv2 }
                
                newP1, newP2