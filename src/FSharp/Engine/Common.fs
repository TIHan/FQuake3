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
    val Value : single
    val Integer : int
    val Next : nativeint
    val HashNext : nativeint

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Vector =
    val X : single

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Vector2 =
    val X : single
    val Y : single

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Vector3 =
    val X : single
    val Y : single
    val Z : single

    member this.Item
        with get (i) =
            match i with
            | 0 -> this.X
            | 1 -> this.Y
            | 2 -> this.Z
            | _ -> raise <| IndexOutOfRangeException ()
        

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

    static member DotProduct (x: Vector3) (y: Vector3) =
        (x.X * y.X) + (x.Y * y.Y) + (x.Z * y.Z)

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Vector4 =
    val X : single
    val Y : single
    val Z : single
    val W : single

    member this.Item
        with get (i) =
            match i with
            | 0 -> this.X
            | 1 -> this.Y
            | 2 -> this.Z
            | 3 -> this.W
            | _ -> raise <| IndexOutOfRangeException ()

    new (x, y, z, w) = { X = x; Y = y; Z = z; W = w }
