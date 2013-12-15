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

/// Math Module
[<RequireQualifiedAccess>]
module Math =
    [<Literal>]
    let PI = 3.14159265358979323846f

    [<Literal>]
    let E = 2.7182818284590452354f

    [<Literal>]
    let ``PI / 360`` = 0.00872664625997165f
        
    let inline lerp x y t = x + (y - x) * t

/// Single with Units of Measure
type single<[<Measure>] 'Measure> = float32<'Measure>

/// Degrees
[<Measure>] type deg

/// Radians
[<Measure>] type rad

[<RequireQualifiedAccess>]
module Deg =
    [<Literal>]
    let ``PI / 180`` = 0.0174532925199433f<rad/deg>

    let inline toRad x : single<rad> = x * ``PI / 180``

[<RequireQualifiedAccess>]
module Rad =
    [<Literal>]
    let ``180 / PI`` = 57.2957795130823f<deg/rad>    

    let inline toDeg x : single<deg> = x * ``180 / PI``

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Vector2 =
    val x : single
    val y : single

    new (x, y) = { x = x; y = y }
    new (x) = { x = x; y = x }

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.x | 1 -> this.y
            | _ -> raise <| IndexOutOfRangeException ()
and vec2 = Vector2

/// Vector2 Module
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Vec2 =    
    let zero =  vec2 (0.f)
    let one =   vec2 (1.f)
    let right = vec2 (1.f, 0.f)
    let up =    vec2 (0.f, 1.f)

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Vector3 =
    val x : single
    val y : single
    val z : single

    new (x, y, z) = { x = x; y = y; z = z }
    new (x) = { x = x; y = x; z = x }

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.x | 1 -> this.y | 2 -> this.z
            | _ -> raise <| IndexOutOfRangeException ()

    member inline this.Set (?x: single, ?y: single, ?z: single) =
        vec3 (
            (match x with | Some x -> x | None -> this.x),
            (match y with | Some y -> y | None -> this.y),
            (match z with | Some z -> z | None -> this.z)
        )

    static member inline Abs (v: vec3) =
        vec3 (abs v.x, abs v.y, abs v.z)

    static member inline Truncate (v: vec3) =
        vec3 (truncate v.x, truncate v.y, truncate v.z)

    static member inline Floor (v: vec3) =
        vec3 (floor v.x, floor v.y, floor v.z)

    static member inline (*) (v1: vec3, v2: vec3) =
        vec3 (v1.x * v2.x, v1.y * v2.y, v1.z * v2.z)

    static member inline (/) (v1: vec3, v2: vec3) =
        vec3 (v1.x / v2.x, v1.y / v2.y, v1.z / v2.z)

    static member inline (+) (v1: vec3, v2: vec3) =
        vec3 (v1.x + v2.x, v1.y + v2.y, v1.z + v2.z)

    static member inline (-) (v1: vec3, v2: vec3) =
        vec3 (v1.x - v2.x, v1.y - v2.y, v1.z - v2.z)

    static member inline (*) (v: vec3, s) =
        vec3 (v.x * s, v.y * s, v.z * s)

    static member inline (/) (v: vec3, s) =
        vec3 (v.x / s, v.y / s, v.z / s)

    static member inline (+) (v: vec3, s) =
        vec3 (v.x + s, v.y + s, v.z + s)

    static member inline (-) (v: vec3, s) =
        vec3 (v.x - s, v.y - s, v.z - s)

    static member inline (*) (s, v) =
        v * s

    static member inline (/) (s, v) =
        v / s

    static member inline (+) (s, v) =
        v / s

    static member inline (-) (s, v) =
        v / s
and vec3 = Vector3

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Vec3 =
    let zero =      vec3 (0.f)
    let one =       vec3 (1.f)
    let right =     vec3 (1.f, 0.f, 0.f)
    let up =        vec3 (0.f, 1.f, 0.f)
    let forward =   vec3 (0.f, 0.f, 1.f)

    let inline minDimension (v: vec3) =
        match v.x < v.y with
        | true ->
            match v.x < v.z with
            | true -> 0
            | _ -> 2
        | _ ->
            match v.y < v.z with
            | true -> 1
            | _ -> 2
        
    let inline multiplyAdd s (v1: vec3) (v2: vec3) =
        vec3 (s * v1.x + v2.x, s * v1.y + v2.y, s * v1.z + v2.z) 

    let inline dot (v1: vec3) (v2: vec3) =
        v1.x * v2.x + v1.y * v2.y + v1.z * v2.z

    let inline cross (v1: vec3) (v2: vec3) =
        vec3 (
            v1.y * v2.z - v1.z * v2.y,
            v1.z * v2.x - v1.x * v2.z,
            v1.x * v2.y - v1.y * v2.x
        )

    let inline lengthSquared (v: vec3) =
        v.x * v.x + v.y * v.y + v.z * v.z

    let inline length v =
        sqrt <| lengthSquared v

    let inline normalize v =
        let length = 1.f / length v
        vec3 (v.x * length, v.y * length, v.z * length)

    let inline perpendicular v =
        let uv =
            match abs v |> minDimension with
            | 0 -> right | 1 -> up | 2 -> forward
            | _ -> raise <| System.ArgumentOutOfRangeException ()

        let vn = normalize uv
        cross v vn

    let inline lerp (v1: vec3) (v2: vec3) (t: single) =
        vec3 (Math.lerp v1.x v2.x t, Math.lerp v1.y v2.y t, Math.lerp v1.z v2.z t)

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Vector4 =
    val x : single
    val y : single
    val z : single
    val w : single

    new (x, y, z, w) = { x = x; y = y; z = z; w = w }
    new (x) = { x = x; y = x; z = x; w = x }
    
    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.x | 1 -> this.y | 2 -> this.z | 3 -> this.w
            | _ -> raise <| IndexOutOfRangeException ()

    static member inline (*) (v1: vec4, v2: vec4) =
        vec4 (v1.x * v2.x, v1.y * v2.y, v1.z * v2.z, v1.w * v2.w)
        
    static member inline (+) (v1: vec4, v2: vec4) =
        vec4 (v1.x + v2.x, v1.y + v2.y, v1.z + v2.z, v1.w + v2.w)

    static member inline (-) (v1: vec4, v2: vec4) =
        vec4 (v1.x - v2.x, v1.y - v2.y, v1.z - v2.z, v1.w - v2.w) 
and vec4 = Vector4 

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Vec4 =
    let zero = vec4 (0.f)

    let inline dot (v1: vec4) (v2:vec4) =
        v1.x * v2.x + v1.y * v2.y + v1.z * v2.z + v1.w * v2.w

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Matrix2 =
    val m11 : single; val m12 : single
    val m21 : single; val m22 : single

    new (m11, m12, m21, m22) = { 
        m11 = m11; m12 = m12;
        m21 = m21; m22 = m22 }

    member inline this.Item
            with get (i, j) =
                match (i, j) with
                | (0, 0) -> this.m11 | (0, 1) -> this.m12
                | (1, 0) -> this.m21 | (1, 1) -> this.m22
                | _ -> raise <| IndexOutOfRangeException ()
and mat2 = Matrix2

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Mat2 =
    let zero = mat2 (0.f, 0.f, 0.f, 0.f)

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Matrix3 =
    val m11 : single; val m12 : single; val m13 : single
    val m21 : single; val m22 : single; val m23 : single
    val m31 : single; val m32 : single; val m33 : single     

    new (m11, m12, m13, m21, m22, m23, m31, m32, m33) =
        {
        m11 = m11; m12 = m12; m13 = m13;
        m21 = m21; m22 = m22; m23 = m23;
        m31 = m31; m32 = m32; m33 = m33 }
    
    member inline this.Item
            with get (i, j) =
                match (i, j) with
                | (0, 0) -> this.m11 | (0, 1) -> this.m12 | (0, 2) -> this.m13
                | (1, 0) -> this.m21 | (1, 1) -> this.m22 | (1, 2) -> this.m23
                | (2, 0) -> this.m31 | (2, 1) -> this.m32 | (2, 2) -> this.m33
                | _ -> raise <| IndexOutOfRangeException ()
and mat3 = Matrix3

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Mat3 =
    let zero = mat3 (0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f)

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Matrix4 =
    val m11 : single; val m12 : single; val m13 : single; val m14 : single
    val m21 : single; val m22 : single; val m23 : single; val m24 : single
    val m31 : single; val m32 : single; val m33 : single; val m34 : single
    val m41 : single; val m42 : single; val m43 : single; val m44 : single        

    new (m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33, m34, m41, m42, m43, m44) =
        {
        m11 = m11; m12 = m12; m13 = m13; m14 = m14;
        m21 = m21; m22 = m22; m23 = m23; m24 = m24;
        m31 = m31; m32 = m32; m33 = m33; m34 = m34;
        m41 = m41; m42 = m42; m43 = m43; m44 = m44 }
    
    member inline this.Item
            with get (i, j) =
                match (i, j) with
                | (0, 0) -> this.m11 | (0, 1) -> this.m12 | (0, 2) -> this.m13 | (0, 3) -> this.m14
                | (1, 0) -> this.m21 | (1, 1) -> this.m22 | (1, 2) -> this.m23 | (1, 3) -> this.m24
                | (2, 0) -> this.m31 | (2, 1) -> this.m32 | (2, 2) -> this.m33 | (2, 3) -> this.m34
                | (3, 0) -> this.m41 | (3, 1) -> this.m42 | (3, 2) -> this.m43 | (3, 3) -> this.m44
                | _ -> raise <| IndexOutOfRangeException ()

#if DEBUG
    static member (*) (m1: mat4, m2: mat4) =
#else
    static member inline (*) (m1: mat4, m2: mat4) =
#endif
        let inline f i j = m1.[i, 0] * m2.[0, j] + m1.[i, 1] * m2.[1, j] + m1.[i, 2] * m2.[2, j] + m1.[i, 3] * m2.[3, j]
        mat4 (
            f 0 0, f 0 1, f 0 2, f 0 3,
            f 1 0, f 1 1, f 1 2, f 1 3,
            f 2 0, f 2 1, f 2 2, f 2 3,
            f 3 0, f 3 1, f 3 2, f 3 3
        )
and mat4 = Matrix4

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Mat4 =
    let zero = mat4 (0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f)

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Quaternion =
    val w : single
    val x : single
    val y : single
    val z : single

    new (w, x, y, z) = { w = w; x = x; y = y; z = z }
        
    static member inline Dot (q1: quat, q2: quat) =
        q1.x * q2.x + q1.y * q2.y + q1.z * q2.z + q1.w * q2.w

    member inline q.Conjugate with get () = quat (q.w, -q.x, -q.y, -q.z)

    member inline q.Length with get () = sqrt <| quat.Dot (q, q)

    static member inline (*) (q1: quat, q2: quat) =
        quat (
            (q1.w * q2.w - q1.x * q2.x - q1.y * q2.y - q1.z * q2.z),
            (q1.w * q2.x + q1.x * q2.w + q1.y * q2.z - q1.z * q2.y),
            (q1.w * q2.y + q1.y * q2.w + q1.z * q2.x - q1.x * q2.z),
            (q1.w * q2.z + q1.z * q2.w + q1.x * q2.y - q1.y * q2.x)
        )

    /// Steps:
    /// normalize vector
    /// create quat based on normalized vector's x,y,z
    /// then multiply by the passed quat's conjugate (inverse)
    /// then multiply by the passed quat
    /// create vector based on result quat's x,y,z
    static member inline (*) (q: quat, v) =
        let vl = 1.f / Vec3.length v
        let vq = quat (0.f, v.x * vl, v.y * vl, v.z * vl)
        let result = q * (vq * q.Conjugate)

        vec3 (result.x, result.y, result.z)
and quat = Quaternion

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Quat =
    let inline dot q1 q2 = quat.Dot (q1, q2)

    let inline conjugate (q: quat) = q.Conjugate

    let inline length (q: quat) = q.Length

    let inline normalize (q: quat) =
        let ``1 / length`` = 1.f / length q
        quat (
            (q.w * ``1 / length``),
            (q.x * ``1 / length``),
            (q.y * ``1 / length``),
            (q.z * ``1 / length``)
        )

    let inline ofEulerDegrees (v: vec3) =
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

        quat (
            (cosRoll * cosPitchYaw + sinRoll * sinPitchYaw),
            (sinRoll * cosPitchYaw - cosRoll * sinPitchYaw),
            (cosRoll * sinPitch * cosYaw + sinRoll * cosPitch * sinYaw),
            (cosRoll * cosPitch * sinYaw - sinRoll * sinPitch * cosYaw)
        )

    let inline ofAxisAngle (axis: vec3) (angle: single<rad>) =
        let angle = angle * 0.5f</rad>
        let sinAngle = sin angle

        quat (
            (cos angle),
            (axis.x * sinAngle),
            (axis.y * sinAngle),
            (axis.z * sinAngle)
        )

[<RequireQualifiedAccess>]
module Transform =
    let inline rotateAroundPoint point axis angle : vec3 =
        let q = Quat.ofAxisAngle axis <| Deg.toRad angle
        q * point
