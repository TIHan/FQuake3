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

    [<MarshalAs (UnmanagedType.ByValArray, SizeConst = 2)>]
    val Axis : Vector3[]        // orientation in world

    val ViewOrigin : Vector3    // viewParams->or.origin in local coordinates

    [<MarshalAs (UnmanagedType.ByValArray, SizeConst = 16)>]
    val ModelMatrix : float[]

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Plane =
    val Normal : Vector3
    val Distance : float32
    val Type : PlaneType
    val SignBits : byte

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

    [<MarshalAs (UnmanagedType.ByValArray, SizeConst = 16)>]
    val ProjectionMatrix : float32[]

    [<MarshalAs (UnmanagedType.ByValArray, SizeConst = 4)>]
    val Frustum : Plane[]

    [<MarshalAs (UnmanagedType.ByValArray, SizeConst = 2)>]
    val VisBounds : Vector3[]

    val ZFar : float32


module Cvar =
    
    let GetNoCull () =
        NativeRenderer.Cvar_GetNoCull ()
