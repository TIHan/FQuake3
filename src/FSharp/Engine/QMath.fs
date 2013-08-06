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

#nowarn "9"
#nowarn "51"

namespace Engine.QMath

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

module QMath =
    let PI = single Math.PI
    let E = single Math.E
        
    let inline Lerp (x: single) (y: single) (t: single) =
        x + (t * (y - x))

/// <summary>
/// Vector
/// </summary>
[<Struct>]
type Vector =
    val X : single

/// <summary>
/// Vector2
/// </summary>
[<Struct>]
type Vector2 =
    val X : single
    val Y : single

/// <summary>
/// Vector3
/// </summary>        
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


    new (vector: Vector3) =
        {
            X = vector.X;
            Y = vector.Y;
            Z = vector.Z;
        }
    
    new (x, y, z) =
        {
            X = x;
            Y = y;
            Z = z;
        }

    static member inline UnitX = Vector3 (1.f, 0.f, 0.f)
    static member inline UnitY = Vector3 (0.f, 1.f, 0.f)
    static member inline UnitZ = Vector3 (0.f, 0.f, 1.f)

    static member inline Init (f: int -> single) =
        Vector3 (f 0, f 1, f 2)

    static member inline ZeroCreate () =
        Vector3 ()

    static member inline Abs (v: Vector3) =
        Vector3 (abs v.X, abs v.Y, abs v.Z)

    static member inline MinDimension (v: Vector3) =
        match v.X < v.Y with
        | true ->
            match v.X < v.Z with
            | true -> 0
            | _ -> 2
        | _ ->
            match v.Y < v.Z with
            | true -> 1
            | _ -> 2

    static member inline (*) (v1: Vector3, v2: Vector3) =
        Vector3 (v1.X * v2.X, v1.Y * v2.Y, v1.Z * v2.Z)

    static member inline (*) (v: Vector3, s: single) =
        Vector3 (v.X * s, v.Y * s, v.Z * s)

    static member inline (*) (s: single, v: Vector3) =
        Vector3 (v.X * s, v.Y * s, v.Z * s)

    static member inline (+) (v1: Vector3, v2: Vector3) =
        Vector3 (v1.X + v2.X, v1.Y + v2.Y, v1.Z + v2.Z)

    static member inline (-) (v1: Vector3, v2: Vector3) =
        Vector3 (v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z)

    static member inline ( *+ ) ((s: single, v1: Vector3), v2: Vector3) =
        Vector3 (s * v1.X + v2.X, s * v1.Y + v2.Y, s * v1.Z + v2.Z)

    static member inline Snap (v: Vector3) =
        Vector3 (truncate v.X, truncate v.Y, truncate v.Z)

    static member inline DotProduct (v1: Vector3) (v2: Vector3) =
        (v1.X * v2.X) + (v1.Y * v2.Y) + (v1.Z * v2.Z)

    static member inline CrossProduct (v1: Vector3) (v2: Vector3) =
        Vector3 ((v1.Y * v2.Z) - (v1.Z * v2.Y), (v1.Z * v2.X) - (v1.X * v2.Z), (v1.X * v2.Y) - (v1.Y * v2.X))

    static member inline Scale (s: single) (v: Vector3) =
        Vector3 (v.X * s, v.Y * s, v.Z * s)

    static member inline MultiplyAdd (s: float32) (v1: Vector3) (v2: Vector3) =
        s * v1 + v2

    static member inline Length (v: Vector3) =
        sqrt <| Vector3.DotProduct v v

    static member inline Normalize (v: Vector3) =
        let length = 1.f / Vector3.Length v
        Vector3.Init (fun i -> v.[i] * length)

    static member inline Perpendicular (v: Vector3) =
        let uv =
            match Vector3.Abs v |> Vector3.MinDimension with
            | 0 -> Vector3.UnitX
            | 1 -> Vector3.UnitY
            | 2 -> Vector3.UnitZ
            | _ -> raise <| System.ArgumentOutOfRangeException ()

        let uvNormal = Vector3.Normalize uv
        Vector3.CrossProduct v uvNormal

/// <summary>
/// Vector4
/// </summary>        
[<Struct>]
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

    new (vector: Vector4) =
        {
            X = vector.X;
            Y = vector.Y;
            Z = vector.Z;
            W = vector.W;
        }
    
    new (x, y, z, w) =
        {
            X = x;
            Y = y;
            Z = z;
            W = w;
        }

    static member inline DotProduct (v1: Vector4) (v2: Vector4) =
        (v1.X * v2.X) + (v1.Y * v2.Y) + (v1.Z * v2.Z) + (v1.W * v2.W)

    static member inline (*) (v1: Vector4, v2: Vector4) =
        Vector4 (v1.X * v2.X, v1.Y * v2.Y, v1.Z * v2.Z, v1.W * v2.W)
        
    static member inline (+) (v1: Vector4, v2: Vector4) =
        Vector4 (v1.X + v2.X, v1.Y + v2.Y, v1.Z + v2.Z, v1.W + v2.W)

    static member inline (-) (v1: Vector4, v2: Vector4) =
        Vector4 (v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z, v1.W - v2.W)

/// <summary>
/// Matrix16
/// </summary>        
[<Struct>]
type Matrix16 =     
    val M00 : single
    val M01 : single
    val M02 : single
    val M03 : single
    val M10 : single
    val M11 : single
    val M12 : single
    val M13 : single
    val M20 : single
    val M21 : single
    val M22 : single
    val M23 : single
    val M30 : single
    val M31 : single
    val M32 : single
    val M33 : single
    
    member inline this.Item
            with get (i, j) =
                    match (i, j) with
                    | (0, 0) -> this.M00
                    | (0, 1) -> this.M01
                    | (0, 2) -> this.M02
                    | (0, 3) -> this.M03
                    | (1, 0) -> this.M10
                    | (1, 1) -> this.M11
                    | (1, 2) -> this.M12
                    | (1, 3) -> this.M13
                    | (2, 0) -> this.M20
                    | (2, 1) -> this.M21
                    | (2, 2) -> this.M22
                    | (2, 3) -> this.M23
                    | (3, 0) -> this.M30
                    | (3, 1) -> this.M31
                    | (3, 2) -> this.M32
                    | (3, 3) -> this.M33
                    | _ -> raise <| IndexOutOfRangeException ()
    
    new (
            m00, m01, m02, m03,
            m10, m11, m12, m13,
            m20, m21, m22, m23,
            m30, m31, m32, m33
        ) =
        {
            M00 = m00; M01 = m01; M02 = m02; M03 = m03;
            M10 = m10; M11 = m11; M12 = m12; M13 = m13;
            M20 = m20; M21 = m21; M22 = m22; M23 = m23;
            M30 = m30; M31 = m31; M32 = m32; M33 = m33;
        }    
    
    static member inline Init (f: int -> int -> single) =
        Matrix16 (
            f 0 0, f 0 1, f 0 2, f 0 3,
            f 1 0, f 1 1, f 1 2, f 1 3,
            f 2 0, f 2 1, f 2 2, f 2 3,
            f 3 0, f 3 1, f 3 2, f 3 3
        )
        
    static member inline ZeroCreate () =
        Matrix16 ()
        
    static member inline Iter (f: int -> int -> single -> unit) (m1: Matrix16) =
        for i = 0 to 3 do
            for j = 0 to 3 do
                f i j m1.[i, j]
                
    static member inline Map (f: int -> int -> single -> single) (m1: Matrix16) =
        Matrix16 (
            f 0 0 m1.[0, 0], f 0 1 m1.[0, 1], f 0 2 m1.[0, 2], f 0 3 m1.[0, 3],
            f 1 0 m1.[1, 0], f 1 1 m1.[1, 1], f 1 2 m1.[1, 2], f 1 3 m1.[1, 3],
            f 2 0 m1.[2, 0], f 2 1 m1.[2, 1], f 2 2 m1.[2, 2], f 2 3 m1.[2, 3],
            f 3 0 m1.[3, 0], f 3 1 m1.[3, 1], f 3 2 m1.[3, 2], f 3 3 m1.[3, 3]
        )

module NativeMatrix16 =
    [<DllImport ("Engine.Native.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void qmath_matrix16_multiply ([<In>] Matrix16* m1, [<In>] Matrix16* m2, [<Out>] Matrix16* m)

type Matrix16 with
    static member inline (*) (m1: Matrix16, m2: Matrix16) =
#if NOT_NATIVE
        let dotProduct row column =
            (m1.[row, 0] * m2.[0, column]) +
            (m1.[row, 1] * m2.[1, column]) +
            (m1.[row, 2] * m2.[2, column]) +
            (m1.[row, 3] * m2.[3, column])
        
        Matrix16.Init dotProduct
#else
        let mutable m = Matrix16.ZeroCreate ()
        let mutable cm1 = m1
        let mutable cm2 = m2
        NativeMatrix16.qmath_matrix16_multiply (&&cm1, &&cm2, &&m)
        m
#endif