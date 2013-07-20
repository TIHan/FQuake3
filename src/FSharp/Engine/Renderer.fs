namespace Engine

#nowarn "9"

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading
open System.Diagnostics
open Microsoft.FSharp.NativeInterop

module private NativeRenderer =
    
    [<Literal>]
    let libQuake3 = "quake3.dll"

    [<Literal>]
    let callingConvention = CallingConvention.Cdecl

    [<DllImport (libQuake3, CallingConvention = callingConvention)>]
    extern bool Cvar_GetNoCull ()

type CullType =
    | In = 0
    | Clip = 1
    | Out = 2

type PlaneType =
    | X = 0
    | Y = 1
    | Z = 2
    | NonAxial = 3

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Orientation =
    val Origin : Vector3        // in world coordinates

    [<MarshalAs (UnmanagedType.ByValArray, SizeConst = 3)>]
    val Axis : Vector3[]        // orientation in world
    val ViewOrigin : Vector3    // viewParams->or.origin in local coordinates

    [<MarshalAs (UnmanagedType.ByValArray, SizeConst = 16)>]
    val ModelMatrix : float32[]

    new (origin, axis, viewOrigin, modelMatrix) = { Origin = origin; Axis = axis; ViewOrigin = viewOrigin; ModelMatrix = modelMatrix }


[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Plane =
    val Normal : Vector3
    val Distance : single

    [<MarshalAs (UnmanagedType.I8)>]
    val Type : PlaneType        // signx + (signy<<1) + (signz<<2), used as lookup during collision

    val SignBits : byte

    new (normal, distance, typ, signBits) = { Normal = normal; Distance = distance; Type = typ; SignBits = signBits }

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type ViewParms =
    val Orientation : Orientation
    val World : Orientation
    val PvsOrigin : Vector3
    val IsPortal : bool
    val IsMirror : bool
    val FrameSceneId : int
    val FrameCount : int
    val PortalPlane : Plane
    val ViewPortX : int
    val ViewPortY : int
    val ViewPortWidth : int
    val ViewPortHeight : int
    val FovX : float32
    val FovY : float32

    [<MarshalAs (UnmanagedType.ByValArray, SizeConst = 16)>]
    val ProjectionMatrix : float32[]

    [<MarshalAs (UnmanagedType.ByValArray, SizeConst = 4)>]
    val Frustum : Plane[]

    [<MarshalAs (UnmanagedType.ByValArray, SizeConst = 2)>]
    val VisibilityBounds : Vector3[]

    val ZFar : float32

    new (orientation, world, pvsOrigin, isPortal, isMirror, frameSceneId, frameCount, portalPlane, viewPortX, viewPortY, viewPortWidth, viewPortHeight, fovX, fovY, projectionMatrix, frustum, visibilityBounds, zFar) =
        {
            Orientation = orientation;
            World = world;
            PvsOrigin = pvsOrigin;
            IsPortal = isPortal;
            IsMirror = isMirror;
            FrameSceneId = frameSceneId;
            FrameCount = frameCount;
            PortalPlane = portalPlane;
            ViewPortX = viewPortX;
            ViewPortY = viewPortY;
            ViewPortWidth = viewPortWidth;
            ViewPortHeight = viewPortHeight;
            FovX = fovX;
            FovY = fovY;
            ProjectionMatrix = projectionMatrix;
            Frustum = frustum;
            VisibilityBounds = visibilityBounds;
            ZFar = zFar;
        }

module CvarModule =
    
    let GetNoCull () =
        NativeRenderer.Cvar_GetNoCull ()

module MainRenderer =

    /// <summary>
    /// Transform into world space.
    /// </summary>
    let TransformWorldSpace (bounds: Vector3[]) (orientation: Orientation) =
        let transformed : Vector3[] = Array.zeroCreate 8
        
        transformed |> Array.mapi (fun i x ->
            let v = Vector3 (bounds.[i &&& 1].X, bounds.[(i >>> 1) &&& 1].Y, bounds.[(i >>> 1) &&& 1].Z)

            orientation.Origin
            |> Vector3.MA v.X orientation.Axis.[0]
            |> Vector3.MA v.Y orientation.Axis.[1]
            |> Vector3.MA v.Z orientation.Axis.[2]
        )

    /// <summary>
    /// Check against frustum planes.
    /// </summary>
    let CheckFrustumPlanes (transformed: Vector3[]) (frustum: Plane[]) =
        let rec checkFrustumPlane (frust: Plane) front back isFront acc =
            match acc = Array.length transformed || isFront with
            | true -> (front, back)
            | _ ->
                let distance = Vector3.DotProduct transformed.[acc] frust.Normal
                Console.WriteLine (transformed.[acc].X)
                match distance > frust.Distance with
                | true -> checkFrustumPlane frust 1 back (back = 1) (acc + 1)
                | _ -> checkFrustumPlane frust front 1 false (acc + 1)



        let rec checkFrustumPlanes anyBack isFront acc =
            match acc = Array.length frustum || isFront = false with
            | true ->
                (anyBack, isFront)
            | _ ->
                let frust = frustum.[acc]
                let frontBack = checkFrustumPlane frust 0 0 false 0
                match frontBack with
                | (front, back) ->
                    checkFrustumPlanes (anyBack ||| back) (front = 1) (acc + 1)

        match checkFrustumPlanes 0 true 0 with
        | (_, false) -> CullType.Out
        | (0, _) -> CullType.In
        | _ -> CullType.Clip


