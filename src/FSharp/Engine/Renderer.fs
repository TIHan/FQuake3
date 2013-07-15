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
    val Distance : float32

    [<MarshalAs (UnmanagedType.I8)>]
    val Type : PlaneType        // signx + (signy<<1) + (signz<<2), used as lookup during collision

    val SignBits : byte

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

    let TransformWorldSpace (bounds: Vector3[]) (orientation: Orientation) =
        let transformed : Vector3[] = Array.zeroCreate 8
        
        transformed |> Array.mapi (fun i x ->
            let v =
                Vector3 (
                        bounds.[i &&& 1].X,
                        bounds.[(i >>> 1) &&& 1].Y,
                        bounds.[(i >>> 1) &&& 1].Z  
                )

            orientation.Origin
            |> Vector3.MA v.X orientation.Axis.[0]
            |> Vector3.MA v.Y orientation.Axis.[1]
            |> Vector3.MA v.Z orientation.Axis.[2]
        )


