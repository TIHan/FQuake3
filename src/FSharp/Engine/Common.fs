namespace Engine

#nowarn "9"

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading
open System.Diagnostics
open Microsoft.FSharp.NativeInterop

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Cvar =
    [<MarshalAs (UnmanagedType.LPStr)>]
    val Name : string

    [<MarshalAs (UnmanagedType.LPStr)>]
    val String : string

    [<MarshalAs (UnmanagedType.LPStr)>]
    val ResetString : string

    [<MarshalAs (UnmanagedType.LPStr)>]
    val LatchedString : string

    val Flags : int
    val IsModified : bool
    val ModificationCount : int
    val Value : float
    val Integer : int
    val Next : nativeint
    val HashNext : nativeint

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Vector =
    val X : float32

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Vector2 =
    val X : float32
    val Y : float32

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Vector3 =
    val X : float32
    val Y : float32
    val Z : float32

    new (x, y, z) = { X = x; Y = y; Z = z }
    new (vector: Vector3) = { X = vector.X; Y = vector.Y; Z = vector.Z }

    static member Snap (vec: Vector3) =
        new Vector3 (float32 (int vec.X), float32 (int vec.Y), float32 (int vec.Z))

    static member MA (s: float32) (b: Vector3) (vec: Vector3) =
        new Vector3 (
            vec.X + (b.X * s),
            vec.Y + (b.Y * s),
            vec.Z + (b.Z * s)
        )

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Vector4 =
    val X : float32
    val Y : float32
    val Z : float32
    val W : float32

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Axis =
    val X : Vector3
    val Y : Vector3
    val Z : Vector3
    
[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Matrix =
    val X : Vector4
    val Y : Vector4
    val Z : Vector4
    val W : Vector4
