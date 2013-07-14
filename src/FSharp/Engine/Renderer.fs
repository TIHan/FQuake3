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
    val Axis : Axis             // orientation in world
    val ViewOrigin : Vector3    // viewParams->or.origin in local coordinates
    val ModelMatrix : Matrix

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
type Bounds = 
    val Start : Vector3
    val End : Vector3

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Frustum =
    val X : Plane
    val Y : Plane
    val Z : Plane
    val W : Plane

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type ViewParams =
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
    val ProjectionMatrix : Matrix
    val Frustum : Frustum
    val VisibilityBounds : Bounds
    val ZFar : float32

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
            |> Vector3.MA v.X orientation.Axis.X
            |> Vector3.MA v.Y orientation.Axis.Y
            |> Vector3.MA v.Z orientation.Axis.Z
        )
        
    // TODO: Make this functional.
    (*
    let CheckFrustumPlanes (transformed: Vector3[]) (viewParams: ViewParams) =
        let planes = [| viewParams.Frustum.X; viewParams.Frustum.Y; viewParams.Frustum.Z; viewParams.Frustum.W |]
        let mutable anyBack = 0
        let mutable cull = CullType.Clip

        let mutable i = 0
        while i < 4 do
            let frust = planes.[i]

            let mutable front = 0
            let mutable back = 0
            let mutable j = 0
            while j < 8 do
                let distance = Vector3.DotProduct transformed.[j] frust.Normal
                if distance > frust.Distance then
                    front <- 1
                    if back = 1 then
                        j <- 8
                    j <- j + 1
                else
                    back <- 1
                    j <- j + 1

            if front = 0 then
                i <- 4
                anyBack <- 1
                cull <- CullType.Out

            anyBack <- anyBack ||| back
            i <- i + 1

        match anyBack = 0 with
        | true -> CullType.In
        | _ -> cull
        *)


