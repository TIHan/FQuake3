(*
Copyright (C) 2013 William F. Smith

This program is free software; you can redistribute it
and/or modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the License,
or (at your option) any later version.

This program is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

Derivative of Quake III Arena source:
Copyright (C) 1999-2005 Id Software, Inc.
*)

module Engine.Renderer.Main

open System
open System.Diagnostics.Contracts
open FSharp.Game.Math
open Engine.Core
open Engine.Renderer.Core
open Engine.Renderer.Shader
open FQuake3.Math

let flipMatrix =
    // convert from our coordinate system (looking down X)
    // to OpenGL's coordinate system (looking down -Z)
    mat4 (
        0.f,  0.f, -1.f, 0.f,
        -1.f, 0.f,  0.f, 0.f,
        0.f,  1.f,  0.f, 0.f,
        0.f,  0.f,  0.f, 1.f)

[<Literal>]
let private TransformSize = 8

/// <summary>
/// Based on Q3: R_CullLocalBox
/// CullLocalBox
/// </summary>
[<Pure>]
let cullLocalBox (bounds: Bounds) (orientation: OrientationR) (frustum: Frustum) (r_nocull: Cvar) =
    match r_nocull.Integer = 1 with
    | true -> ClipType.Clip
    | _ ->

    // transform into world space
    let inline transform i =
        let v = vec3 (bounds.[i &&& 1].x, bounds.[(i >>> 1) &&& 1].y, bounds.[(i >>> 2) &&& 1].z)

        orientation.Origin
        |> Vec3.multiplyAdd v.x orientation.Axis.[0]
        |> Vec3.multiplyAdd v.y orientation.Axis.[1]
        |> Vec3.multiplyAdd v.z orientation.Axis.[2]

    let rec checkFrustumPlane (frust: Plane) front back isFront n =
        match n with
        | TransformSize -> (front, back)
        | _ ->
        match isFront with
        | true -> (front, back)
        | _ ->
            let distance = Vec3.dot (transform n) frust.Normal

            match distance > frust.Distance with
            | true -> checkFrustumPlane frust 1 back (back = 1) (n + 1)
            | _ -> checkFrustumPlane frust front 1 false (n + 1)

    let rec checkFrustumPlanes anyBack isFront n =
        match n with
        | Frustum.size -> (anyBack, isFront)
        | _ ->
        match isFront with
        | false -> (anyBack, isFront)
        | _ ->
            let frust = frustum.[n]

            match checkFrustumPlane frust 0 0 false 0 with
            | (front, back) ->
                checkFrustumPlanes (anyBack ||| back) (front = 1) (n + 1)

    // check against frustum planes
    match checkFrustumPlanes 0 true 0 with
    | (_, false) -> ClipType.Out // all points were behind one of the planes
    | (0, _) -> ClipType.In // completely inside frustum
    | _ -> ClipType.Clip // partially clipped

/// <summary>
/// Based on Q3: R_CullPointAndRadius
/// CullPointAndRadius
/// </summary>
[<Pure>]
let cullPointAndRadius (point: vec3) (radius: single) (frustum: Frustum) (r_nocull: Cvar) =
    match r_nocull.Integer = 1 with
    | true -> ClipType.Clip
    | _ ->

    let rec checkFrustumPlanes mightBeClipped canCullOut n =
        match n with
        | Frustum.size -> (mightBeClipped, canCullOut)
        | _ ->
        match canCullOut with
        | true -> (mightBeClipped, canCullOut)
        | _ ->
        let frust = frustum.[n]
        let distance = (Vec3.dot point frust.Normal) - frust.Distance

        match distance < -radius with
        | true -> checkFrustumPlanes mightBeClipped true (n + 1)
        | _ when distance <= radius -> checkFrustumPlanes true false (n + 1)
        | _ -> checkFrustumPlanes mightBeClipped false (n + 1)

    match checkFrustumPlanes false false 0 with
    | (_, true) -> ClipType.Out // all points were behind one of the planes
    | (true, _) -> ClipType.Clip // partially clipped
    | _ -> ClipType.In // completely inside frustum

/// <summary>
/// Based on Q3: R_LocalPointToWorld
/// LocalPointToWorld
/// </summary>
[<Pure>]
let localPointToWorld (local: vec3) (orientation: OrientationR) =
    let inline f i = Vec3.dot local orientation.Axis.[i] + orientation.Origin.[i]
    vec3 (f 0, f 1, f 2)

/// <summary>
/// Based on Q3: R_LocalNormalToWorld
/// LocalNormalToWorld
/// </summary>
[<Pure>]
let localNormalToWorld (local: vec3) (orientation: OrientationR) =
    let inline f i = Vec3.dot local orientation.Axis.[i]
    vec3 (f 0, f 1, f 2)

/// <summary>
/// Based on Q3: R_WorldToLocal
/// WorldToLocal
/// </summary>
[<Pure>]
let worldToLocal (world: vec3) (orientation: OrientationR) =
    let inline f i = Vec3.dot world orientation.Axis.[i]
    vec3 (f 0, f 1, f 2)

/// <summary>
/// Based on Q3: R_CullLocalPointAndRadius
/// CullLocalPointAndRadius
/// </summary>
[<Pure>]
let cullLocalPointAndRadius (point: vec3) (radius: single) (orientation: OrientationR) (frustum: Frustum) (r_nocull: Cvar) =
    let transformed = localPointToWorld point orientation
    cullPointAndRadius transformed radius frustum r_nocull

/// <summary>
/// Based on Q3: R_CullLocalPointAndRadius
/// TransformModelToClip
/// </summary>
[<Pure>]
let transformModelToClip (source: vec3) (modelMatrix: mat4) (projectionMatrix: mat4) =
    let inline calculateEye i =
        (source.x * modelMatrix.[0, i]) + (source.y * modelMatrix.[1, i]) +
        (source.z * modelMatrix.[2, i]) + (1.f * modelMatrix.[3, i])
          
    let eye =
        vec4 (
            calculateEye 0, calculateEye 1,
            calculateEye 2, calculateEye 3
        )

    let inline calculateDestination i =
        (eye.x * projectionMatrix.[0, i]) + (eye.y * projectionMatrix.[1, i]) +
        (eye.z * projectionMatrix.[2, i]) + (eye.w * projectionMatrix.[3, i])

    (eye,
        vec4 (
            calculateDestination 0, calculateDestination 1,
            calculateDestination 2, calculateDestination 3
        )
    )
    
/// <summary>
/// Based on Q3: R_TransformClipToWindow
/// TransformClipToWindow
/// </summary>
[<Pure>]
let transformClipToWindow (clip: vec4) (view: ViewParms) =
    let normalized =
        vec4 (
            (clip.x / clip.w),
            (clip.y / clip.w),
            ((clip.z + clip.w) / (2.f * clip.w)),
            0.f
        )

    let window =
        vec4 (
            (truncate ((0.5f * (1.0f + normalized.x) * (single view.ViewportWidth)) + 0.5f)),
            (truncate ((0.5f * (1.0f + normalized.y) * (single view.ViewportHeight)) + 0.5f)),
            normalized.z,
            0.f
        )

    (normalized, window)

// TODO: This will need to go away eventually.
let myGLMultMatrix (a: mat4) (b: mat4) =
    a * b

/// <summary>
/// Based on Q3: R_RotateForEntity
/// RotateForEntity
///
/// Generates an orientation for an entity and viewParms
/// Does NOT produce any GL calls
/// Called by both the front end and the back end
/// </summary>
[<Pure>]
let rotateForEntity (viewParms: ViewParms) (entity: RefEntity) =
    match entity.Type <> RefEntityType.Model with
    | true -> viewParms.World
    | _ ->

    let axis = entity.Axis
    let origin = entity.Origin

    let glMatrix =
        mat4 (
            axis.[0].[0], axis.[0].[1], axis.[0].[2], 0.f,
            axis.[1].[0], axis.[1].[1], axis.[1].[2], 0.f,
            axis.[2].[0], axis.[2].[1], axis.[2].[2], 0.f,
            origin.x, origin.y, origin.z, 1.f)

    // calculate the viewer origin in the model's space
    // needed for fog, specular, and environment mapping
    let delta = viewParms.Orientation.Origin - origin

    // compensate for scale in the axes if necessary
    let axisLength =
        match entity.HasNonNormalizedAxes with
        | true ->
            // TODO: Is it ok to compare the single like this?
            match Vec3.length axis.x with
            | 0.f -> 0.f
            | axisLength ->
                1.0f / axisLength
        | _ -> 1.0f

    let inline calculateOrigin i = (Vec3.dot delta axis.[i]) * axisLength

    {
        Origin = origin;
        Axis = axis;
        ViewOrigin = vec3 (calculateOrigin 0, calculateOrigin 1, calculateOrigin 2);
        ModelMatrix = glMatrix * viewParms.World.ModelMatrix;
    }

/// <summary>
/// Based on Q3: R_RotateForViewer
/// RotateForViewer
///
/// Sets up the modelview matrix for a given viewParm
/// </summary>
[<Pure>]
let rotateForViewer (viewParms: ViewParms) =
    // transform by the camera placement
    let origin = viewParms.Orientation.Origin
    let axis = viewParms.Orientation.Axis

    let viewerMatrix =
        mat4 (
            axis.[0].[0], axis.[1].[0], axis.[2].[0], 0.f,
            axis.[0].[1], axis.[1].[1], axis.[2].[1], 0.f,
            axis.[0].[2], axis.[1].[2], axis.[2].[2], 0.f,
            (-origin.[0] * axis.[0].[0] + -origin.[1] * axis.[0].[1] + -origin.[2] * axis.[0].[2]),
            (-origin.[0] * axis.[1].[0] + -origin.[1] * axis.[1].[1] + -origin.[2] * axis.[1].[2]),
            (-origin.[0] * axis.[2].[0] + -origin.[1] * axis.[2].[1] + -origin.[2] * axis.[2].[2]),
            1.f)
        
    {
    Origin = Vec3.zero;
    Axis = Axis.identity;
    ViewOrigin = origin;
    // convert from our coordinate system (looking down X)
    // to OpenGL's coordinate system (looking down -Z)
    ModelMatrix = viewerMatrix * flipMatrix }

/// <summary>
/// Based on Q3: SetFarClip
/// SetFarClip
/// </summary>
[<Pure>]
let setFarClip (rdFlags: RdFlags) (visibilityBounds: Bounds) (orientation: OrientationR) =
    // if not rendering the world (icons, menus, etc)
    // set a 2k far clip plane
    match rdFlags.HasFlag RdFlags.NoWorldModel with
    | true -> 2048.f
    | _ ->

    // set far clipping planes dynamically
    let rec calculateFarthestCornerDistance distance acc =
        match acc with
        | TransformSize -> distance
        | _ ->
            
        let x = if (acc &&& 1) <> 0 then visibilityBounds.[0].[0] else visibilityBounds.[1].[0]
        let y = if (acc &&& 2) <> 0 then visibilityBounds.[0].[1] else visibilityBounds.[1].[1]
        let z = if (acc &&& 4) <> 0 then visibilityBounds.[0].[2] else visibilityBounds.[1].[2]

        let v = vec3 (x, y, z)
        let possibleDistance = Vec3.lengthSquared <| v - orientation.Origin

        calculateFarthestCornerDistance (if possibleDistance > distance then possibleDistance else distance) (acc + 1)

    sqrt <| calculateFarthestCornerDistance 0.f 0

/// <summary>
/// Based on Q3: R_SetupProjection
/// SetupProjection
/// </summary>
[<Pure>]
let setupProjection (zNear: single) (rdFlags: RdFlags) (view: ViewParms) (fovX: single) (fovY: single) =
    // dynamically compute far clip plane distance
    let zFar = setFarClip rdFlags view.VisibilityBounds view.Orientation

    let xMax = zNear * (tan <| fovX * Math.PI / 360.f)
    let xMin = -xMax

    let yMax = zNear * (tan <| fovY * Math.PI / 360.f)
    let yMin = -yMax

    let width = xMax - xMin
    let height = yMax - yMin
    let depth = zFar - zNear

    (
        mat4 (
            (2.f * zNear / width), 0.f, 0.f, 0.f,
            0.f, (2.f * zNear / height), 0.f, 0.f,
            ((xMax + xMin) / width), ((yMax + yMin) / height), (-(zFar + zNear) / depth), -1.f,
            0.f, 0.f, (-2.f * zFar * zNear / depth), 0.f),
        zFar
    )

/// <summary>
/// Based on Q3: R_SetupProjection
/// SetupFrustum
/// 
/// Setup that culling frustum planes for the current view
/// </summary>
[<Pure>]
let setupFrustum (view: ViewParms) =
    let xAngle = view.FovX / 180.f * Math.PI * 0.5f
    let xs = sin xAngle
    let xc = cos xAngle

    let yAngle = view.FovY / 180.f * Math.PI * 0.5f
    let ys = sin yAngle
    let yc = cos yAngle

    let xNormal = xs * view.Orientation.Axis.[0]
    let yNormal = ys * view.Orientation.Axis.[0]

    let leftNormal = Vec3.multiplyAdd xc view.Orientation.Axis.[1] xNormal
    let rightNormal = Vec3.multiplyAdd -xc view.Orientation.Axis.[1] xNormal
    let bottomNormal = Vec3.multiplyAdd yc view.Orientation.Axis.[2] yNormal
    let topNormal = Vec3.multiplyAdd -yc view.Orientation.Axis.[2] yNormal

    {
    Left =
        {
        Normal = leftNormal;
        Distance = Vec3.dot view.Orientation.Origin leftNormal;
        Type = PlaneType.NonAxial;
        SignBits = Plane.CalculateSignBits leftNormal };
    Right = 
        {
        Normal = rightNormal;
        Distance = Vec3.dot view.Orientation.Origin rightNormal;
        Type = PlaneType.NonAxial;
        SignBits = Plane.CalculateSignBits rightNormal };
    Bottom =
        {
        Normal = bottomNormal;
        Distance = Vec3.dot view.Orientation.Origin bottomNormal;
        Type = PlaneType.NonAxial;
        SignBits = Plane.CalculateSignBits bottomNormal };
    Top =
        {
        Normal = topNormal;
        Distance = Vec3.dot view.Orientation.Origin topNormal;
        Type = PlaneType.NonAxial;
        SignBits = Plane.CalculateSignBits topNormal }}

/// <summary>
/// Based on Q3: R_MirrorPoint
/// MirrorPoint
/// </summary>
[<Pure>]
let mirrorPoint (v: vec3) (surface: Orientation) (camera: Orientation) =
    let local = v - surface.Origin
    let inline transform i transformed = Vec3.multiplyAdd (Vec3.dot local surface.Axis.[i]) camera.Axis.[i] transformed
    transform 0 Vec3.zero |> transform 1 |> transform 2 |> (+) camera.Origin

/// <summary>
/// Based on Q3: R_MirrorVector
/// MirrorVector
/// </summary>
[<Pure>]
let mirrorVector (v: vec3) (surface: Orientation) (camera: Orientation) =
    let inline transform i transformed = Vec3.multiplyAdd (Vec3.dot v surface.Axis.[i]) camera.Axis.[i] transformed
    transform 0 Vec3.zero |> transform 1 |> transform 2

/// <summary>
/// Based on Q3: R_PlaneForSurface
/// PlaneForSurface
/// </summary>
[<Pure>]
let planeForSurface (surface: Surface) (plane: Plane) =
    match surface with
    | Face (value) ->
        value.Plane
    | Triangles (value) ->
        let vertices = value.Vertices
        let indices = value.Indices
        let v1 = vertices.[indices.[0]]
        let v2 = vertices.[indices.[1]]
        let v3 = vertices.[indices.[2]]
        let plane4 = Plane.ofPoints v1.Vertex v2.Vertex v3.Vertex

        { plane4 with Type = plane.Type; SignBits = plane.SignBits }
    | Poly (value) ->
        let vertices = value.Vertices
        let plane4 = Plane.ofPoints vertices.[0].Vertex vertices.[1].Vertex vertices.[2].Vertex

        { plane4 with Type = plane.Type; SignBits = plane.SignBits }
    | _ ->
        { Normal = Vec3.right; Distance = 0.f; Type = PlaneType.X; SignBits = 0uy }

/// <summary>
/// create plane axis for the portal we are seeing
/// </summary>
[<Pure>]
let createPlaneAxis (surface: Surface) =
    planeForSurface surface Plane.zero

/// <summary>
/// rotate the plane if necessary
/// </summary>
[<Pure>]
let tryRotatePlane (originalPlane: Plane) (entityId: int) (r: Renderer) =
    match entityId <> Constants.EntityIdWorld with
    | false -> (originalPlane, originalPlane, r)
    | _ ->

    let tr = Renderer.updateCurrentEntityById entityId r
    match tr.CurrentEntity with
    | None -> raise <| Exception "Current entity does not exist"
    | Some (trEntity) ->

    // get the orientation of the entity
    let orientation = rotateForEntity tr.ViewParms trEntity.Entity

    // rotate the plane, but keep the non-rotated version for matching
    // against the portalSurface entities
    let normal = localNormalToWorld originalPlane.Normal orientation
    let distance = originalPlane.Distance + Vec3.dot normal orientation.Origin

    // translate the original plane
    let originalDistance = originalPlane.Distance + Vec3.dot originalPlane.Normal orientation.Origin

    (
        { originalPlane with Distance = originalDistance },
        { Normal = normal; Distance = distance; Type = PlaneType.X; SignBits = 0uy },
        { tr with Orientation = orientation }
    )

/// Transforms existing axis based on a normal.
[<Pure>]
let transformAxisOfNormal (normal: vec3) (axis: Axis) =
    let y = Vec3.perpendicular normal
    Axis (normal, y, Vec3.cross normal y)

/// Tries to find the closest portal entity based on a plane.
[<Pure>]
let tryFindClosestPortalEntityByPlane (plane: Plane) (r: Renderer) =
    r.Refdef.Entities |>
    List.tryFind (fun x ->
        let isPortalSurface = not (x.Entity.Type <> RefEntityType.PortalSurface)
        let distance = (Vec3.dot x.Entity.Origin plane.Normal) - plane.Distance
        let isWithinDistance = not (distance > 64.f || distance < -64.f)

        isPortalSurface && isWithinDistance
    )

/// Calculates the portal orientation.
[<Pure>]
let calculatePortalOrientation (entity: RefEntity) (plane: Plane) (surface: Orientation) (camera: Orientation) (refdef: TrRefdef) =
    // if the entity is just a mirror, don't use as a camera point
    match entity.OldOrigin = entity.Origin with
    | true ->
        let origin = plane.Normal * plane.Distance
        let axis = surface.Axis.Set (x = Vec3.zero - surface.Axis.x)

        (
            true,
            { surface with Origin = origin },
            { camera with Origin = origin; Axis = axis }
        )
    | _ ->

    // project the origin onto the surface plane to get
    // an origin point we can rotate around
    let distance = (Vec3.dot entity.Origin plane.Normal) - plane.Distance
    let surface = { surface with Origin = Vec3.multiplyAdd -distance surface.Axis.x entity.Origin }

    // now get the camera origin and orientation
    let camera =
        { camera with
            Origin = entity.OldOrigin;
            Axis = entity.Axis.Set (x = Vec3.zero - entity.Axis.x, y = Vec3.zero - entity.Axis.y);
        }

    // optionally rotate and if a speed is specified
    match entity.OldFrame = 0 && entity.SkinId = 0 with
    | true -> (false, surface, camera)
    | _ ->

    let angle =
        match (entity.OldFrame <> 0, entity.Frame <> 0) with
        | (true, true) -> single refdef.Time / 1000.f * single entity.Frame // continuous rotate
        | (true, false) -> single entity.SkinId + (sin <| single refdef.Time * 0.003f) * 4.f // bobbing rotate, with skinId being the rotation offset
        | _ -> single entity.SkinId

    // We know the angle should be in degrees.
    let angle = angle * 1.f<deg>

    let y = Transform.rotateAroundPoint camera.Axis.y camera.Axis.x angle

    (
        false,
        surface,
        { camera with 
            Axis = camera.Axis.Set (y = y, z = Vec3.cross camera.Axis.x y)
        }
    )

/// Based on Q3: R_GetPortalOrientations
/// GetPortalOrientations
///
/// entityId is the entity that the portal surface is a part of, which may
/// be moving and rotating.
///
/// Returns true if it should be mirrored
[<Pure>]
let getPortalOrientations (surface: Surface) (entityId: int) (surfaceOr: Orientation) (cameraOr: Orientation) (pvsOrigin: vec3) (r: Renderer) =
    // create plane axis for the portal we are seeing
    let originalPlane = createPlaneAxis surface

    // rotate the plane if necessary
    match tryRotatePlane originalPlane entityId r with
    | (originalPlane, plane, tr) ->

    let surfaceOr = { surfaceOr with Axis = transformAxisOfNormal plane.Normal surfaceOr.Axis }

    // locate the portal entity closest to this plane.
    // origin will be the origin of the portal, origin2 will be
    // the origin of the camera
    match tryFindClosestPortalEntityByPlane originalPlane tr with
    | None ->
        // if we didn't locate a portal entity, don't render anything.
        // We don't want to just treat it as a mirror, because without a
        // portal entity the server won't have communicated a proper entity set
        // in the snapshot

        // unfortunately, with local movement prediction it is easily possible
        // to see a surface before the server has communicated the matching
        // portal surface entity, so we don't want to print anything here...

        //ri.Printf( PRINT_ALL, "Portal surface without a portal entity\n" );
        (false, false, surfaceOr, cameraOr, pvsOrigin, tr)
    | Some e ->

    match calculatePortalOrientation e.Entity plane surfaceOr cameraOr tr.Refdef with
    | (isMirror, surface, camera) ->
    (true, isMirror, surface, camera, e.Entity.OldOrigin, tr)

/// Based on Q3: IsMirror
/// IsMirror
/// Note: this is internal
[<Pure>]
let isMirror (surface: Surface) (entityId: int) (r: Renderer) =
    // create plane axis for the portal we are seeing
    let originalPlane = createPlaneAxis surface

    // rotate the plane if necessary
    match tryRotatePlane originalPlane entityId r with
    | (originalPlane, plane, r) ->

    // locate the portal entity closest to this plane.
    // origin will be the origin of the portal, origin2 will be
    // the origin of the camera
    match tryFindClosestPortalEntityByPlane originalPlane r with
    | None -> (false, r)
    | Some e ->

    // if the entity is just a mirror, don't use as a camera point
    match e.Entity.OldOrigin = e.Entity.Origin with
    | true -> (true, r)
    | _ -> (false, r)

/// Based on Q3: R_AddDrawSurf
/// AddDrawSurface
[<Pure>]
let addDrawSurface surfaceId shaderId entityId fogId dynamicLightMap (r: Renderer) =
    let drawSurface =
        {
        SurfaceId = surfaceId
        ShaderId = shaderId
        EntityId = entityId
        FogId = fogId
        DynamicLightMap = dynamicLightMap }

    { r with Refdef = { r.Refdef with DrawSurfaces = drawSurface :: r.Refdef.DrawSurfaces } }

/// AddEntitySurface
/// TODO: Not finished.
let addEntitySurface (rentity: TrRefEntity) (r: Renderer) =
    let rentity = { rentity with NeedDlights = false }

    //
    // the weapon model must be handled special --
    // we don't want the hacked weapon position showing in 
    // mirrors, because the true body position will already be drawn
    //
    match rentity.Entity.RenderFx.HasFlag RenderFxFlags.FirstPerson with
    | true -> rentity
    | _ ->

    match rentity.Entity.Type with
    | RefEntityType.PortalSurface -> rentity
    | RefEntityType.Sprite
    | RefEntityType.Beam
    | RefEntityType.Lightning
    | RefEntityType.RailCore
    | RefEntityType.RailRings ->
        // self blood sprites, talk balloons, etc should not be drawn in the primary
        // view.  We can't just do this check for all entities, because md3
        // entities may still want to cast shadows from them
        match rentity.Entity.RenderFx.HasFlag RenderFxFlags.ThirdPerson && not r.ViewParms.IsPortal with
        | true -> rentity
        | _ ->

        let shader = shaderById rentity.Entity.CustomShaderHandle
        rentity
    | _ -> rentity

/// Based on Q3: R_AddEntitySurfaces
/// AddEntitySurfaces
/// TODO: Not finished.
let addEntitySurfaces (r: Renderer) (r_drawentities: Cvar) =
    match r_drawentities.Integer = 0 with
    | true -> r
    | _ ->

    let entities =
        r.Refdef.Entities
        |> List.map (fun x -> addEntitySurface x r)

    { r with Refdef = { r.Refdef with Entities = entities } }