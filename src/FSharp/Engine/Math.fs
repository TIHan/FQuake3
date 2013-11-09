(*
Copyright (c) 2013 William F. Smith

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

namespace Engine.Math

#nowarn "9"

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

/// Math Module
[<RequireQualifiedAccess>]
module Math =
    [<Literal>]
    let PI = 3.14159265358979323846f

    [<Literal>]
    let E = 2.7182818284590452354f

    [<Literal>]
    let ``PI / 180`` = 0.0174532925199433f

    [<Literal>]
    let ``PI / 360`` = 0.00872664625997165f

    [<Literal>]
    let ``180 / PI`` = 57.2957795130823f
        
    let inline lerp (x: single) (y: single) (t: single) =
        x + (t * (y - x))

/// Single with Units of Measure
type single<[<Measure>] 'Measure> = float32<'Measure>

/// Degrees
[<Measure>] type deg

/// Radians
[<Measure>] type rad

[<RequireQualifiedAccess>]
module Convert =
    [<Literal>]
    let ``PI / 180`` = 0.0174532925199433f<rad/deg>

    [<Literal>]
    let ``180 / PI`` = 57.2957795130823f<deg/rad>

    let inline degToRad x : single<rad> = x * ``PI / 180``
    let inline radToDeg x : single<deg> = x * ``180 / PI``


/// Vector2
[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Vector2 =
    val X : single
    val Y : single

    new (x, y) = { X = x; Y = y }

    static member inline Create (x, y) =
        Vector2 (x, y)

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.X | 1 -> this.Y
            | _ -> raise <| IndexOutOfRangeException ()

/// Vector2 Module
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Vector2 =
    let inline create x y = 
        Vector2.Create (x, y)
    
    let zero =  create 0.f 0.f
    let one =   create 1.f 1.f
    let unitX = create 1.f 0.f
    let unitY = create 0.f 1.f

/// Vector3
[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Vector3 =
    val X : single
    val Y : single
    val Z : single

    new (x, y, z) = { X = x; Y = y; Z = z }

    static member inline Create (x, y, z) =
        Vector3 (x, y, z)

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.X | 1 -> this.Y | 2 -> this.Z
            | _ -> raise <| IndexOutOfRangeException ()

    member inline this.Set (?X: single, ?Y: single, ?Z: single) =
        Vector3.Create (
            (match X with | Some x -> x | None -> this.X),
            (match Y with | Some y -> y | None -> this.Y),
            (match Z with | Some z -> z | None -> this.Z)
        )

    static member inline Abs (v: Vector3) =
        Vector3.Create (abs v.X, abs v.Y, abs v.Z)

    static member inline Truncate (v: Vector3) =
        Vector3.Create (truncate v.X, truncate v.Y, truncate v.Z)

    static member inline (*) (v1: Vector3, v2: Vector3) =
        Vector3.Create (v1.X * v2.X, v1.Y * v2.Y, v1.Z * v2.Z)

    static member inline (/) (v1: Vector3, v2: Vector3) =
        Vector3.Create (v1.X / v2.X, v1.Y / v2.Y, v1.Z / v2.Z)

    static member inline (+) (v1: Vector3, v2: Vector3) =
        Vector3.Create (v1.X + v2.X, v1.Y + v2.Y, v1.Z + v2.Z)

    static member inline (-) (v1: Vector3, v2: Vector3) =
        Vector3.Create (v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z)

    static member inline (*) (v: Vector3, s) =
        Vector3.Create (v.X * s, v.Y * s, v.Z * s)

    static member inline (/) (v: Vector3, s) =
        Vector3.Create (v.X / s, v.Y / s, v.Z / s)

    static member inline (+) (v: Vector3, s) =
        Vector3.Create (v.X + s, v.Y + s, v.Z + s)

    static member inline (-) (v: Vector3, s) =
        Vector3.Create (v.X - s, v.Y - s, v.Z - s)

    static member inline (*) (s, v) =
        v * s

    static member inline (/) (s, v) =
        v / s

    static member inline (+) (s, v) =
        v / s

    static member inline (-) (s, v) =
        v / s

/// Vector3 Module
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Vector3 =
    let inline create x y z =
        Vector3.Create (x, y, z)

    let zero =  create 0.f 0.f 0.f
    let one =   create 1.f 1.f 1.f
    let unitX = create 1.f 0.f 0.f
    let unitY = create 0.f 1.f 0.f
    let unitZ = create 0.f 0.f 1.f

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
        
    let inline multiplyAdd s (v1: Vector3) (v2: Vector3) =
        create (s * v1.X + v2.X) (s * v1.Y + v2.Y) (s * v1.Z + v2.Z) 

    let inline dot (v1: Vector3) (v2: Vector3) =
        (v1.X * v2.X) + (v1.Y * v2.Y) + (v1.Z * v2.Z)

    let inline cross (v1: Vector3) (v2: Vector3) =
        create
            ((v1.Y * v2.Z) - (v1.Z * v2.Y))
            ((v1.Z * v2.X) - (v1.X * v2.Z))
            ((v1.X * v2.Y) - (v1.Y * v2.X))

    let inline lengthSquared (v: Vector3) =
        (v.X * v.X) + (v.Y * v.Y) + (v.Z * v.Z)

    let inline length v =
        sqrt <| lengthSquared v

    let inline normalize v =
        let length = 1.f / length v
        create (v.X * length) (v.Y * length) (v.Z * length)

    let inline perpendicular v =
        let uv =
            match abs v |> minDimension with
            | 0 -> unitX | 1 -> unitY | 2 -> unitZ
            | _ -> raise <| System.ArgumentOutOfRangeException ()

        let vn = normalize uv
        cross v vn

    let inline lerp (v1: Vector3) (v2: Vector3) (t: single) =
        create (Math.lerp v1.X v2.X t) (Math.lerp v1.Y v2.Y t) (Math.lerp v1.Z v2.Z t)

/// Vector4
[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Vector4 =
    val X : single
    val Y : single
    val Z : single
    val W : single

    new (x, y, z, w) = { X = x; Y = y; Z = z; W = w }

    static member inline Create (x, y, z, w) =
        Vector4 (x, y, z, w)
    
    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.X | 1 -> this.Y | 2 -> this.Z | 3 -> this.W
            | _ -> raise <| IndexOutOfRangeException ()

    static member inline (*) (v1: Vector4, v2: Vector4) =
        Vector4.Create (v1.X * v2.X, v1.Y * v2.Y, v1.Z * v2.Z, v1.W * v2.W)
        
    static member inline (+) (v1: Vector4, v2: Vector4) =
        Vector4.Create (v1.X + v2.X, v1.Y + v2.Y, v1.Z + v2.Z, v1.W + v2.W)

    static member inline (-) (v1: Vector4, v2: Vector4) =
        Vector4.Create (v1.X - v2.X, v1.Y - v2.Y, v1.Z - v2.Z, v1.W - v2.W)  

// Vector4 Module
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Vector4 =
    let inline create x y z w =
        Vector4.Create (x, y, z, w)

    let zero = create 0.f 0.f 0.f 0.f

    let inline dot (v1: Vector4) (v2: Vector4) =
        (v1.X * v2.X) + (v1.Y * v2.Y) + (v1.Z * v2.Z) + (v1.W * v2.W)

/// Matrix2x2
[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Matrix2x2 =
    val M11 : single; val M12 : single
    val M21 : single; val M22 : single

    new (m11, m12, m21, m22) =
        { 
            M11 = m11; M12 = m12;
            M21 = m21; M22 = m22;
        }

    static member inline Create (m11, m12, m21, m22) =
        Matrix2x2 (
            m11, m12,
            m21, m22
        )

    member inline this.Item
            with get (i, j) =
                match (i, j) with
                | (0, 0) -> this.M11 | (0, 1) -> this.M12
                | (1, 0) -> this.M21 | (1, 1) -> this.M22
                | _ -> raise <| IndexOutOfRangeException ()

/// Matrix2x2 Module
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Matrix2x2 =
    let inline create m11 m12 m21 m22 =
        Matrix2x2.Create (m11, m12, m21, m22)

    let zero = create 0.f 0.f 0.f 0.f

/// Matrix3x3
[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Matrix3x3 =
    val M11 : single; val M12 : single; val M13 : single
    val M21 : single; val M22 : single; val M23 : single
    val M31 : single; val M32 : single; val M33 : single     

    new (m11, m12, m13, m21, m22, m23, m31, m32, m33) =
        {
            M11 = m11; M12 = m12; M13 = m13;
            M21 = m21; M22 = m22; M23 = m23;
            M31 = m31; M32 = m32; M33 = m33;
        }

    static member inline Create (m11, m12, m13, m21, m22, m23, m31, m32, m33) =
        Matrix3x3 (
            m11, m12, m13,
            m21, m22, m23,
            m31, m32, m33
        )
    
    member inline this.Item
            with get (i, j) =
                match (i, j) with
                | (0, 0) -> this.M11 | (0, 1) -> this.M12 | (0, 2) -> this.M13
                | (1, 0) -> this.M21 | (1, 1) -> this.M22 | (1, 2) -> this.M23
                | (2, 0) -> this.M31 | (2, 1) -> this.M32 | (2, 2) -> this.M33
                | _ -> raise <| IndexOutOfRangeException ()

/// Matrix3x3 Module
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Matrix3x3 =
    let inline create m11 m12 m13 m21 m22 m23 m31 m32 m33 =
        Matrix3x3.Create (m11, m12, m13, m21, m22, m23, m31, m32, m33)

    let zero = create 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f

/// Matrix4x4
[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Matrix4x4 =
    val M11 : single; val M12 : single; val M13 : single; val M14 : single
    val M21:  single; val M22 : single; val M23 : single; val M24 : single
    val M31 : single; val M32 : single; val M33 : single; val M34 : single
    val M41 : single; val M42 : single; val M43 : single; val M44 : single        

    new (m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33, m34, m41, m42, m43, m44) =
        {
            M11 = m11; M12 = m12; M13 = m13; M14 = m14;
            M21 = m21; M22 = m22; M23 = m23; M24 = m24;
            M31 = m31; M32 = m32; M33 = m33; M34 = m34;
            M41 = m41; M42 = m42; M43 = m43; M44 = m44;
        }

    static member inline Create (m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33, m34, m41, m42, m43, m44) =
        Matrix4x4 (
            m11, m12, m13, m14,
            m21, m22, m23, m24,
            m31, m32, m33, m34,
            m41, m42, m43, m44
        )
    
    member inline this.Item
            with get (i, j) =
                match (i, j) with
                | (0, 0) -> this.M11 | (0, 1) -> this.M12 | (0, 2) -> this.M13 | (0, 3) -> this.M14
                | (1, 0) -> this.M21 | (1, 1) -> this.M22 | (1, 2) -> this.M23 | (1, 3) -> this.M24
                | (2, 0) -> this.M31 | (2, 1) -> this.M32 | (2, 2) -> this.M33 | (2, 3) -> this.M34
                | (3, 0) -> this.M41 | (3, 1) -> this.M42 | (3, 2) -> this.M43 | (3, 3) -> this.M44
                | _ -> raise <| IndexOutOfRangeException ()

#if DEBUG
    static member (*) (m1: Matrix4x4, m2: Matrix4x4) =
#else
    static member inline (*) (m1: Matrix4x4, m2: Matrix4x4) =
#endif
        let inline f i j = (m1.[i, 0] * m2.[0, j]) + (m1.[i, 1] * m2.[1, j]) + (m1.[i, 2] * m2.[2, j]) + (m1.[i, 3] * m2.[3, j])
        Matrix4x4.Create (
            f 0 0, f 0 1, f 0 2, f 0 3,
            f 1 0, f 1 1, f 1 2, f 1 3,
            f 2 0, f 2 1, f 2 2, f 2 3,
            f 3 0, f 3 1, f 3 2, f 3 3
        )

/// Matrix4x4 Module
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Matrix4x4 =
    let inline create m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m34 m41 m42 m43 m44 =
        Matrix4x4.Create (m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33, m34, m41, m42, m43, m44)

    let zero = create 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f

/// Quaternion
[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Quaternion =
    val W : single
    val X : single
    val Y : single
    val Z : single

    new (w, x, y, z) = { W = w; X = x; Y = y; Z = z }

    static member inline Create (w, x, y, z) =
        Quaternion (w, x, y, z)
        
    static member inline Dot (q1: Quaternion, q2: Quaternion) =
        (q1.X * q2.X) + (q1.Y * q2.Y) + (q1.Z * q2.Z) + (q1.W * q2.W)

    member inline q.Conjugate
        with get () =
            Quaternion.Create (q.W, -q.X, -q.Y, -q.Z)

    member inline q.Length
        with get () =
            sqrt <| Quaternion.Dot (q, q)

    static member inline (*) (q1: Quaternion, q2: Quaternion) =
        Quaternion.Create (
            ((q1.W * q2.W) - (q1.X * q2.X) - (q1.Y * q2.Y) - (q1.Z * q2.Z)),
            ((q1.W * q2.X) + (q1.X * q2.W) + (q1.Y * q2.Z) - (q1.Z * q2.Y)),
            ((q1.W * q2.Y) + (q1.Y * q2.W) + (q1.Z * q2.X) - (q1.X * q2.Z)),
            ((q1.W * q2.Z) + (q1.Z * q2.W) + (q1.X * q2.Y) - (q1.Y * q2.X))
        )

    static member inline (*) (q: Quaternion, v) =
        let vn = Vector3.normalize v
        let vq = Quaternion.Create (0.f, vn.X, vn.Y, vn.Z)
        let result = q * (vq * q.Conjugate)

        Vector3.create result.X result.Y result.Z

/// Quaternion Module
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Quaternion =
    let inline create w x y z =
        Quaternion.Create (w, x, y, z)

    let inline dot (q1: Quaternion) (q2: Quaternion) =
        Quaternion.Dot (q1, q2)

    let inline conjugate (q: Quaternion) =
        q.Conjugate

    let inline length (q: Quaternion) =
        q.Length

    let inline normalize (q: Quaternion) =
        let ``1 / length`` = 1.f / length q
        create
            (q.W * ``1 / length``)
            (q.X * ``1 / length``)
            (q.Y * ``1 / length``)
            (q.Z * ``1 / length``)

    let inline ofEulerDegrees (v: Vector3) =
        let pitch = Math.``PI / 360`` * v.[0]
        let yaw =   Math.``PI / 360`` * v.[1]
        let roll =  Math.``PI / 360`` * v.[2]

        let sinRoll =   sin roll
        let sinPitch =  sin pitch
        let sinYaw =    sin yaw

        let cosRoll =   cos roll
        let cosPitch =  cos pitch
        let cosYaw =    cos yaw

        let cosPitchYaw = cosPitch * cosYaw
        let sinPitchYaw = sinPitch * sinYaw

        create
            ((cosRoll * cosPitchYaw) + (sinRoll * sinPitchYaw))
            ((sinRoll * cosPitchYaw) - (cosRoll * sinPitchYaw))
            ((cosRoll * sinPitch * cosYaw) + (sinRoll * cosPitch * sinYaw))
            ((cosRoll * cosPitch * sinYaw) - (sinRoll * sinPitch * cosYaw))

    let inline ofAxisAngle (axis: Vector3) (angle: single<rad>) =
        let angle = angle * 0.5f</rad>
        let sinAngle = sin angle

        create
            (cos angle)
            (axis.X * sinAngle)
            (axis.Y * sinAngle)
            (axis.Z * sinAngle)

/// Transform
[<RequireQualifiedAccess>]
module Transform =
    let inline rotateAroundPoint (point: Vector3) (axis: Vector3) (angle: single<deg>) =
        let q = Quaternion.ofAxisAngle axis (Convert.degToRad angle)
        q * point
