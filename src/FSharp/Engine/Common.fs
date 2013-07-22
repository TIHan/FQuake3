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
type Vector3 =
    val X : single
    val Y : single
    val Z : single

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.X
            | 1 -> this.Y
            | 2 -> this.Z
            | _ -> raise <| IndexOutOfRangeException ()
        

    new (x, y, z) = { X = x; Y = y; Z = z }
    new (vector: Vector3) = { X = vector.X; Y = vector.Y; Z = vector.Z }

    static member inline Snap (vec: Vector3) =
        new Vector3 (truncate vec.X, truncate vec.Y, truncate vec.Z)

    static member inline MA (s: float32) (b: Vector3) (vec: Vector3) =
        new Vector3 (
            vec.X + (b.X * s),
            vec.Y + (b.Y * s),
            vec.Z + (b.Z * s)
        )

    static member inline DotProduct (x: Vector3) (y: Vector3) =
        (x.X * y.X) + (x.Y * y.Y) + (x.Z * y.Z)

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Vector4 =
    val X : single
    val Y : single
    val Z : single
    val W : single

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.X
            | 1 -> this.Y
            | 2 -> this.Z
            | 3 -> this.W
            | _ -> raise <| IndexOutOfRangeException ()

    new (x, y, z, w) = { X = x; Y = y; Z = z; W = w }

[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = 64)>]
type Matrix16 =
    [<FieldOffset (0)>]
    val X : Vector4

    [<FieldOffset (16)>]
    val Y : Vector4

    [<FieldOffset (32)>]
    val Z : Vector4

    [<FieldOffset (48)>]
    val W : Vector4

    member inline this.Item
        with get (i, j) =
                match i with
                | 0 -> this.X.[j]
                | 1 -> this.Y.[j]
                | 2 -> this.Z.[j]
                | 3 -> this.W.[j]
                | _ -> raise <| IndexOutOfRangeException ()

    new (x, y, z, w) = { X = x; Y = y; Z = z; W = w }
    new (values: single[,]) = {
        X = Vector4 (values.[0, 0], values.[0, 1], values.[0, 2], values.[0, 3]);
        Y = Vector4 (values.[1, 0], values.[1, 1], values.[1, 2], values.[1, 3]);
        Z = Vector4 (values.[2, 0], values.[2, 1], values.[2, 2], values.[2, 3]);
        W = Vector4 (values.[3, 0], values.[3, 1], values.[3, 2], values.[3, 3]);
    }

    // Note: This is used heavily. Highly affects performance.
    static member inline (*) (matrix1: Matrix16, matrix2: Matrix16) =
        let inline dotProduct row column =
            (matrix1.[row, 0] * matrix2.[0, column]) +
            (matrix1.[row, 1] * matrix2.[1, column]) +
            (matrix1.[row, 2] * matrix2.[2, column]) +
            (matrix1.[row, 3] * matrix2.[3, column])

        let result = Array2D.init 4 4 (fun row column -> dotProduct row column)
        Matrix16 (result)

