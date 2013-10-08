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

namespace Engine.Renderer

open System
open System.IO
open System.Threading
open System.Diagnostics
open System.Diagnostics.Contracts
open Engine.Core
open Engine.Math
open Engine.NativeInterop

module Main =
    let flipMatrix =
        // convert from our coordinate system (looking down X)
        // to OpenGL's coordinate system (looking down -Z)
        Matrix4x4.create
            0.f 0.f -1.f 0.f
           -1.f 0.f  0.f 0.f
            0.f 1.f  0.f 0.f
            0.f 0.f  0.f 1.f

    [<Literal>]
    let private TransformSize = 8

    /// <summary>
    /// Based on Q3: R_CullLocalBox
    /// CullLocalBox
    /// </summary>
    [<Pure>]
    let cullLocalBox (bounds: Bounds) (orientation: OrientationR) (frustum: Frustum) (noCull: Cvar) =
        match noCull.Integer = 1 with
        | true -> ClipType.Clip
        | _ ->

        // transform into world space
        let inline transform i =
            let v = Vector3.create bounds.[i &&& 1].X bounds.[(i >>> 1) &&& 1].Y bounds.[(i >>> 2) &&& 1].Z

            orientation.Origin
            |> Vector3.multiplyAdd v.X orientation.Axis.[0]
            |> Vector3.multiplyAdd v.Y orientation.Axis.[1]
            |> Vector3.multiplyAdd v.Z orientation.Axis.[2]

        let rec checkFrustumPlane (frust: Plane) front back isFront n =
            match n with
            | TransformSize -> (front, back)
            | _ ->
            match isFront with
            | true -> (front, back)
            | _ ->
                let distance = Vector3.dotProduct (transform n) frust.Normal

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
    let cullPointAndRadius (point: Vector3) (radius: single) (frustum: Frustum) (noCull: Cvar) =
        match noCull.Integer = 1 with
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
            let distance = (Vector3.dotProduct point frust.Normal) - frust.Distance

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
    let localPointToWorld (local: Vector3) (orientation: OrientationR) =
        let inline f i = Vector3.dotProduct local orientation.Axis.[i] + orientation.Origin.[i]
        Vector3.create (f 0) (f 1) (f 2)

    /// <summary>
    /// Based on Q3: R_LocalNormalToWorld
    /// LocalNormalToWorld
    /// </summary>
    [<Pure>]
    let localNormalToWorld (local: Vector3) (orientation: OrientationR) =
        let inline f i = Vector3.dotProduct local orientation.Axis.[i]
        Vector3.create (f 0) (f 1) (f 2)

    /// <summary>
    /// Based on Q3: R_WorldToLocal
    /// WorldToLocal
    /// </summary>
    [<Pure>]
    let worldToLocal (world: Vector3) (orientation: OrientationR) =
        let inline f i = Vector3.dotProduct world orientation.Axis.[i]
        Vector3.create (f 0) (f 1) (f 2)

    /// <summary>
    /// Based on Q3: R_CullLocalPointAndRadius
    /// CullLocalPointAndRadius
    /// </summary>
    [<Pure>]
    let cullLocalPointAndRadius (point: Vector3) (radius: single) (orientation: OrientationR) (frustum: Frustum) (noCull: Cvar) =
        let transformed = localPointToWorld point orientation
        cullPointAndRadius transformed radius frustum noCull

    /// <summary>
    /// Based on Q3: R_CullLocalPointAndRadius
    /// TransformModelToClip
    /// </summary>
    [<Pure>]
    let transformModelToClip (source: Vector3) (modelMatrix: Matrix4x4) (projectionMatrix: Matrix4x4) =
        let inline calculateEye i =
            (source.X * modelMatrix.[0, i]) +
            (source.Y * modelMatrix.[1, i]) +
            (source.Z * modelMatrix.[2, i]) +
            (1.f * modelMatrix.[3, i])
          
        let eye =
            Vector4.create
                (calculateEye 0)
                (calculateEye 1)
                (calculateEye 2)
                (calculateEye 3)

        let inline calculateDestination i =
            (eye.X * projectionMatrix.[0, i]) +
            (eye.Y * projectionMatrix.[1, i]) +
            (eye.Z * projectionMatrix.[2, i]) +
            (eye.W * projectionMatrix.[3, i])

        (eye,
            Vector4.create
                (calculateDestination 0)
                (calculateDestination 1)
                (calculateDestination 2)
                (calculateDestination 3)
        )
    
    /// <summary>
    /// Based on Q3: R_TransformClipToWindow
    /// TransformClipToWindow
    /// </summary>
    [<Pure>]
    let transformClipToWindow (clip: Vector4) (view: ViewParms) =
        let normalized =
            Vector4.create
                (clip.X / clip.W)
                (clip.Y / clip.W)
                ((clip.Z + clip.W) / (2.f * clip.W))
                0.f

        let window =
            Vector4.create
                (truncate ((0.5f * (1.0f + normalized.X) * (single view.ViewportWidth)) + 0.5f))
                (truncate ((0.5f * (1.0f + normalized.Y) * (single view.ViewportHeight)) + 0.5f))
                normalized.Z
                0.f

        (normalized, window)

    // TODO: This will need to go away eventually.
    let myGLMultMatrix (a: Matrix4x4) (b: Matrix4x4) =
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
            Matrix4x4.create
                axis.[0].[0]
                axis.[0].[1]
                axis.[0].[2]
                0.f
                axis.[1].[0]
                axis.[1].[1]
                axis.[1].[2]
                0.f
                axis.[2].[0]
                axis.[2].[1]
                axis.[2].[2]
                0.f
                origin.X
                origin.Y
                origin.Z
                1.f

        // calculate the viewer origin in the model's space
        // needed for fog, specular, and environment mapping
        let delta = viewParms.Orientation.Origin - origin

        // compensate for scale in the axes if necessary
        let axisLength =
            match entity.HasNonNormalizedAxes with
            | true ->
                // TODO: Is it ok to compare the single like this?
                match Vector3.length axis.X with
                | 0.f -> 0.f
                | axisLength ->
                    1.0f / axisLength
            | _ -> 1.0f

        let inline calculateOrigin i = (Vector3.dotProduct delta axis.[i]) * axisLength

        {
            Origin = origin;
            Axis = axis;
            ViewOrigin = Vector3.create (calculateOrigin 0) (calculateOrigin 1) (calculateOrigin 2);
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
            Matrix4x4.create
                axis.[0].[0]
                axis.[1].[0]
                axis.[2].[0]
                0.f
                axis.[0].[1]
                axis.[1].[1]
                axis.[2].[1]
                0.f
                axis.[0].[2]
                axis.[1].[2]
                axis.[2].[2]
                0.f
                (-origin.[0] * axis.[0].[0] + -origin.[1] * axis.[0].[1] + -origin.[2] * axis.[0].[2])
                (-origin.[0] * axis.[1].[0] + -origin.[1] * axis.[1].[1] + -origin.[2] * axis.[1].[2])
                (-origin.[0] * axis.[2].[0] + -origin.[1] * axis.[2].[1] + -origin.[2] * axis.[2].[2])
                1.f
        
        {
            Origin = Vector3.zero;
            Axis = Axis.identity;
            ViewOrigin = origin;
            // convert from our coordinate system (looking down X)
            // to OpenGL's coordinate system (looking down -Z)
            ModelMatrix = viewerMatrix * flipMatrix;
        }

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

            let v = Vector3.create x y z
            let possibleDistance = Vector3.lengthSquared <| v - orientation.Origin

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
            Matrix4x4.create
                (2.f * zNear / width) 0.f 0.f 0.f
                0.f (2.f * zNear / height) 0.f 0.f
                ((xMax + xMin) / width) ((yMax + yMin) / height) (-(zFar + zNear) / depth) -1.f
                0.f 0.f (-2.f * zFar * zNear / depth) 0.f,
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

        let leftNormal = Vector3.multiplyAdd xc view.Orientation.Axis.[1] xNormal
        let rightNormal = Vector3.multiplyAdd -xc view.Orientation.Axis.[1] xNormal
        let bottomNormal = Vector3.multiplyAdd yc view.Orientation.Axis.[2] yNormal
        let topNormal = Vector3.multiplyAdd -yc view.Orientation.Axis.[2] yNormal

        {
            Left =
                {
                    Normal = leftNormal;
                    Distance = Vector3.dotProduct view.Orientation.Origin leftNormal;
                    Type = PlaneType.NonAxial;
                    SignBits = Plane.CalculateSignBits leftNormal;
                };
            Right = 
                {
                    Normal = rightNormal;
                    Distance = Vector3.dotProduct view.Orientation.Origin rightNormal;
                    Type = PlaneType.NonAxial;
                    SignBits = Plane.CalculateSignBits rightNormal;
                };
            Bottom =
                {
                    Normal = bottomNormal;
                    Distance = Vector3.dotProduct view.Orientation.Origin bottomNormal;
                    Type = PlaneType.NonAxial;
                    SignBits = Plane.CalculateSignBits bottomNormal;
                };
            Top =
                {
                    Normal = topNormal;
                    Distance = Vector3.dotProduct view.Orientation.Origin topNormal;
                    Type = PlaneType.NonAxial;
                    SignBits = Plane.CalculateSignBits topNormal;
                }
        }

    /// <summary>
    /// Based on Q3: R_MirrorPoint
    /// MirrorPoint
    /// </summary>
    [<Pure>]
    let mirrorPoint (v: Vector3) (surface: Orientation) (camera: Orientation) =
        let local = v - surface.Origin
        let inline transform i transformed = Vector3.multiplyAdd (Vector3.dotProduct local surface.Axis.[i]) camera.Axis.[i] transformed
        transform 0 Vector3.zero |> transform 1 |> transform 2 |> (+) camera.Origin

    /// <summary>
    /// Based on Q3: R_MirrorVector
    /// MirrorVector
    /// </summary>
    [<Pure>]
    let mirrorVector (v: Vector3) (surface: Orientation) (camera: Orientation) =
        let inline transform i transformed = Vector3.multiplyAdd (Vector3.dotProduct v surface.Axis.[i]) camera.Axis.[i] transformed
        transform 0 Vector3.zero |> transform 1 |> transform 2

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
            { Normal = Vector3.create 1.f 0.f 0.f; Distance = 0.f; Type = PlaneType.X; SignBits = 0uy }

    /// <summary>
    /// create plane axis for the portal we are seeing
    /// </summary>
    [<Pure>]
    let createPlaneAxis (drawSurface: DrawSurface) =
        planeForSurface drawSurface.Surface Plane.zero

    /// <summary>
    /// rotate the plane if necessary
    /// </summary>
    [<Pure>]
    let tryRotatePlane (originalPlane: Plane) (entityId: int) (tr: TrGlobals) =
        match entityId <> Constants.EntityIdWorld with
        | false -> (originalPlane, originalPlane, tr)
        | _ ->

        let tr = TrGlobals.updateCurrentEntityById entityId tr
        match tr.CurrentEntity with
        | None -> raise <| Exception "Current entity does not exist"
        | Some (trEntity) ->

        // get the orientation of the entity
        let orientation = rotateForEntity tr.ViewParms trEntity.Entity

        // rotate the plane, but keep the non-rotated version for matching
        // against the portalSurface entities
        let normal = localNormalToWorld originalPlane.Normal orientation
        let distance = originalPlane.Distance + Vector3.dotProduct normal orientation.Origin

        // translate the original plane
        let originalDistance = originalPlane.Distance + Vector3.dotProduct originalPlane.Normal orientation.Origin

        (
            { originalPlane with Distance = originalDistance },
            { Normal = normal; Distance = distance; Type = PlaneType.X; SignBits = 0uy },
            { tr with Orientation = orientation }
        )

    /// <summary>
    /// Based on Q3: R_GetPortalOrientation
    /// GetPortalOrientation
    ///
    /// entityId is the entity that the portal surface is a part of, which may
    /// be moving and rotating.
    ///
    /// Returns true if it should be mirrored
    /// </summary>
    let getPortalOrientation (drawSurface: DrawSurface) (entityId: int) (surface: Orientation) (camera: Orientation) (pvsOrigin: Vector3) (tr: TrGlobals) =
        // create plane axis for the portal we are seeing
        let originalPlane = createPlaneAxis drawSurface

        // rotate the plane if necessary
        match tryRotatePlane originalPlane entityId tr with
        | (originalPlane, plane, tr) ->
        ()

    /// <summary>
    /// Based on Q3: IsMirror
    /// IsMirror
    /// </summary>
    // Note: this is internal
    let isMirror (drawSurface: DrawSurface) (entityId: int) (tr: TrGlobals) =
        // create plane axis for the portal we are seeing
        let originalPlane = createPlaneAxis drawSurface

        // rotate the plane if necessary
        match tryRotatePlane originalPlane entityId tr with
        | (originalPlane, plane, tr) ->
        ()