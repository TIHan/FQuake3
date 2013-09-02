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

// Disable native interop warnings
#nowarn "9"
#nowarn "51"

namespace Engine
module Math =

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Engine.NativeInterop

module QMath =
    [<Literal>]
    let PI = 3.14159265358979323846f

    [<Literal>]
    let E = 2.7182818284590452354f
        
    let inline lerp (x: single) (y: single) (t: single) =
        x + (t * (y - x))

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
[<StructLayout (LayoutKind.Sequential)>]
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

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Vector3 =
    let zero = Vector3 ()

    let inline create x y z =
        let mutable v = zero
        let ptr = NativePtr.toNativePtr &&v
        NativePtr.set ptr 0 x
        NativePtr.set ptr 1 y
        NativePtr.set ptr 2 z
        v

    let inline setAt i value (v: Vector3) =
        match i with
        | _ when i >= 0 && i <= 2 ->
            let mutable cv = v
            let ptr = NativePtr.toNativePtr &&cv
            NativePtr.set ptr i value
            cv
        | _ -> raise <| IndexOutOfRangeException ()

    let inline set2At i1 value1 i2 value2 (v: Vector3) =
        match (i1, i2) with
        | _ when i1 >= 0 && i1 <= 2 && i2 >= 0 && i2 <= 2 ->
            let mutable cv = v
            let ptr = NativePtr.toNativePtr &&cv
            NativePtr.set ptr i1 value1
            NativePtr.set ptr i2 value2
            cv
        | _ -> raise <| IndexOutOfRangeException ()

    let unitX = create 1.f 0.f 0.f
    let unitY = create 0.f 1.f 0.f
    let unitZ = create 0.f 0.f 1.f

    let inline init (f: int -> single) =
        create (f 0) (f 1) (f 2)

    let inline abs (v: Vector3) =
        create (abs v.X) (abs v.Y) (abs v.Z)

    let inline minDimension (v: Vector3) =
        match v.X < v.Y with
        | true ->
            match v.X < v.Z with
            | true -> 0
            | _ -> 2
        | _ ->
            match v.Y < v.Z with
            | true -> 1
            | _ -> 2

    let inline map (f: single -> single) (v: Vector3) =
        create (f v.[0]) (f v.[1]) (f v.[2])

    let inline mapi (f: int -> single -> single) (v: Vector3) =
        create (f 0 v.[0]) (f 1 v.[1]) (f 2 v.[2])

    let inline map2 (f: single -> single -> single) (v1: Vector3) (v2: Vector3) =
        create (f v1.[0] v2.[0]) (f v1.[1] v2.[1]) (f v1.[2] v2.[2])
        
    let inline mapi2 (f: int -> single -> single -> single) (v1: Vector3) (v2: Vector3) =
        create (f 0 v1.[0] v2.[0]) (f 1 v1.[1] v2.[1]) (f 2 v1.[2] v2.[2])

    let inline snap (v: Vector3) =
        create (truncate v.X) (truncate v.Y) (truncate v.Z)
        
    let inline multiplyAdd (s: single) (v1: Vector3) (v2: Vector3) =
        create (s * v1.X + v2.X) (s * v1.Y + v2.Y) (s * v1.Z + v2.Z) 

    let inline dotProduct (v1: Vector3) (v2: Vector3) =
        (v1.X * v2.X) + (v1.Y * v2.Y) + (v1.Z * v2.Z)

    let inline crossProduct (v1: Vector3) (v2: Vector3) =
        create ((v1.Y * v2.Z) - (v1.Z * v2.Y)) ((v1.Z * v2.X) - (v1.X * v2.Z)) ((v1.X * v2.Y) - (v1.Y * v2.X))

    let inline length (v: Vector3) =
        sqrt <| dotProduct v v

    let inline normalize (v: Vector3) =
        let length = 1.f / length v
        mapi (fun i x -> x * length) v

    let inline perpendicular (v: Vector3) =
        let uv =
            match abs v |> minDimension with
            | 0 -> unitX
            | 1 -> unitY
            | 2 -> unitZ
            | _ -> raise <| System.ArgumentOutOfRangeException ()

        let uvNormal = normalize uv
        crossProduct v uvNormal

type Vector3 with
    static member inline (*) (v1: Vector3, v2: Vector3) =
        Vector3.create (v1.X * v2.X) (v1.Y * v2.Y) (v1.Z * v2.Z)

    static member inline (*) (v: Vector3, s: single) =
        Vector3.create (v.X * s) (v.Y * s) (v.Z * s)

    static member inline (*) (s: single, v: Vector3) =
        Vector3.create (v.X * s) (v.Y * s) (v.Z * s)

    static member inline (+) (v1: Vector3, v2: Vector3) =
        Vector3.create (v1.X + v2.X) (v1.Y + v2.Y) (v1.Z + v2.Z)

    static member inline (-) (v1: Vector3, v2: Vector3) =
        Vector3.create (v1.X - v2.X) (v1.Y - v2.Y) (v1.Z - v2.Z)

    static member inline (=) (v1: Vector3, v2: Vector3) =
        v1.X = v2.X && v1.Y = v2.Y && v1.Z = v2.Z    

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

    static member inline (*) (v1: Vector4, v2: Vector4) =
        Vector4 (v1.X * v2.X, v1.Y * v2.Y, v1.Z * v2.Z, v1.W * v2.W)
        
    static member inline (+) (v1: Vector4, v2: Vector4) =
        Vector4 (v1.X + v2.X, v1.Y + v2.Y, v1.Z + v2.Z, v1.W + v2.W)

    static member inline (-) (v1: Vector4, v2: Vector4) =
        Vector4 (v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z, v1.W - v2.W)

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Vector4 =
    let inline dotProduct (v1: Vector4) (v2: Vector4) =
        (v1.X * v2.X) + (v1.Y * v2.Y) + (v1.Z * v2.Z) + (v1.W * v2.W)


/// <summary>
/// Matrix4
/// </summary>
type Matrix4 =
    val M00 : single
    val M01 : single
    val M10 : single
    val M11 : single

    member inline this.Item
            with get (i, j) =
                    match (i, j) with
                    | (0, 0) -> this.M00
                    | (0, 1) -> this.M01
                    | (1, 0) -> this.M10
                    | (1, 1) -> this.M11
                    | _ -> raise <| IndexOutOfRangeException ()

    new (
            m00, m01,
            m10, m11
        ) =
        {
            M00 = m00; M01 = m01;
            M10 = m10; M11 = m11;
        }

/// <summary>
/// Matrix16
/// </summary>        
[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
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

module NativeMatrix16 =
    [<DllImport ("Engine.Native.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void math_matrix16_multiply ([<In>] Matrix16* m1, [<In>] Matrix16* m2, [<Out>] Matrix16* m)

type Matrix16 with
    static member inline (*) (m1: Matrix16, m2: Matrix16) =
        let mutable m = Matrix16 ()
        let mutable cm1 = m1
        let mutable cm2 = m2
        NativeMatrix16.math_matrix16_multiply (&&cm1, &&cm2, &&m)
        m

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Matrix16 =
    let zero = Matrix16 ()

    let inline init (f: int -> int -> single) =
        Matrix16 (
            f 0 0, f 0 1, f 0 2, f 0 3,
            f 1 0, f 1 1, f 1 2, f 1 3,
            f 2 0, f 2 1, f 2 2, f 2 3,
            f 3 0, f 3 1, f 3 2, f 3 3
        )

    let inline iter (f: single -> unit) (m: Matrix16) =
        for i = 0 to 3 do
            for j = 0 to 3 do
                f m.[i, j]
        
    let inline iteri (f: int -> int -> single -> unit) (m: Matrix16) =
        for i = 0 to 3 do
            for j = 0 to 3 do
                f i j m.[i, j]

    let inline map (f: single -> single) (m: Matrix16) =
        Matrix16 (
            f m.[0, 0], f m.[0, 1], f m.[0, 2], f m.[0, 3],
            f m.[1, 0], f m.[1, 1], f m.[1, 2], f m.[1, 3],
            f m.[2, 0], f m.[2, 1], f m.[2, 2], f m.[2, 3],
            f m.[3, 0], f m.[3, 1], f m.[3, 2], f m.[3, 3]
        )        
                
    let inline mapi (f: int -> int -> single -> single) (m: Matrix16) =
        Matrix16 (
            f 0 0 m.[0, 0], f 0 1 m.[0, 1], f 0 2 m.[0, 2], f 0 3 m.[0, 3],
            f 1 0 m.[1, 0], f 1 1 m.[1, 1], f 1 2 m.[1, 2], f 1 3 m.[1, 3],
            f 2 0 m.[2, 0], f 2 1 m.[2, 1], f 2 2 m.[2, 2], f 2 3 m.[2, 3],
            f 3 0 m.[3, 0], f 3 1 m.[3, 1], f 3 2 m.[3, 2], f 3 3 m.[3, 3]
        )

