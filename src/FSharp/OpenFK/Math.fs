(*
Copyright (c) 2013 OpenFK

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*)

#nowarn "9" // No warnings for interop; we know what we are doing.
#nowarn "51"

namespace OpenFK.Math

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

module Math =
    module Generic =       
        let inline Lerp x y t =
            x + (t * (y - x))
            
        let inline CosLerp x y t pi two half =
            (- cos (pi * t) / two) + half

    module Single =
        let PI = single Math.PI
        let E = single Math.E
        
        let inline Lerp (x: single) (y: single) (t: single) =
            Generic.Lerp x y t
            
        let inline CosLerp (x: single) (y: single) (t: single) =
            Generic.CosLerp x y t PI 2.f 0.f
        
        
    module Float =
        let PI = Math.PI
        let E = Math.E
        
        let inline Lerp (x: float) (y: float) (t: float) =
            Generic.Lerp x y t
            
        let inline CosLerp (x: float) (y : float) (t: float) =
            Generic.CosLerp x y t PI 2.0 0.0

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

    static member inline ZeroCreate () =
        Vector3 ()
        
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
        
[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = 64)>]
type Matrix16 =     
    [<FieldOffset (0)>] val M00 : single
    [<FieldOffset (4)>] val M01 : single
    [<FieldOffset (8)>] val M02 : single
    [<FieldOffset (12)>] val M03 : single
    [<FieldOffset (16)>] val M10 : single
    [<FieldOffset (20)>] val M11 : single
    [<FieldOffset (24)>] val M12 : single
    [<FieldOffset (28)>] val M13 : single
    [<FieldOffset (32)>] val M20 : single
    [<FieldOffset (36)>] val M21 : single
    [<FieldOffset (40)>] val M22 : single
    [<FieldOffset (44)>] val M23 : single
    [<FieldOffset (48)>] val M30 : single
    [<FieldOffset (52)>] val M31 : single
    [<FieldOffset (56)>] val M32 : single
    [<FieldOffset (60)>] val M33 : single
    
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
    [<DllImport ("OpenFK.Native.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void matrix16_multiply ([<In>] Matrix16* m1, [<In>] Matrix16* m2, [<Out>] Matrix16* m)

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
        NativeMatrix16.matrix16_multiply (&&cm1, &&cm2, &&m)
        m
#endif