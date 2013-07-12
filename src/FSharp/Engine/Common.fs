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
    [<MarshalAs(UnmanagedType.LPStr)>]
    val Name : string

    [<MarshalAs(UnmanagedType.LPStr)>]
    val String : string

    [<MarshalAs(UnmanagedType.LPStr)>]
    val ResetString : string

    [<MarshalAs(UnmanagedType.LPStr)>]
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
type Vector3 =
    val X : float32
    val Y : float32
    val Z : float32
    
    new (x, y, z) = { X = x; Y = y; Z = z }
    new (vector: Vector3) = { X = vector.X; Y = vector.Y; Z = vector.Z }

    static member Snap (vec: Vector3) =
        new Vector3 (float32 (int vec.X), float32 (int vec.Y), float32 (int vec.Z))

        (*
    member this.Snap () =
        new Vector3 (float32 (int this.X), float32 (int this.Y), float32 (int this.Z))

    member this.MA (s: float32, b: Vector3) =
        new Vector3 (
            this.X + (b.X * s),
            this.Y + (b.Y * s),
            this.Z + (b.Z * s)
        )

    member this.DotProduct (v: Vector3) =
        (this.X * v.X) + (this.Y * v.Y) + (this.Z * v.Z)*)

