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

namespace Engine.Math

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Engine.NativeInterop

[<RequireQualifiedAccess>]
module Math =
    [<Literal>]
    let PI = 3.14159265358979323846f

    [<Literal>]
    let E = 2.7182818284590452354f
        
    let inline lerp (x: single) (y: single) (t: single) =
        x + (t * (y - x))

/// Vector2
type Vector2 =
    { X: single; Y: single }

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.X
            | 1 -> this.Y
            | _ -> raise <| IndexOutOfRangeException ()

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Vector2 =
    let inline create x y =
        { X = x; Y = y }

    let zero = { X = 0.f; Y = 0.f }

/// Vector3
type Vector3 =
    { X: single; Y: single; Z: single }

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.X
            | 1 -> this.Y
            | 2 -> this.Z
            | _ -> raise <| IndexOutOfRangeException ()

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Vector3 =
    let inline create x y z =
        { X = x; Y = y; Z = z }

    let zero =  create 0.f 0.f 0.f
    let one =   create 1.f 1.f 1.f
    let unitX = create 1.f 0.f 0.f
    let unitY = create 0.f 1.f 0.f
    let unitZ = create 0.f 0.f 1.f

    let inline abs v =
        create (abs v.X) (abs v.Y) (abs v.Z)

    let inline minDimension v =
        match v.X < v.Y with
        | true ->
            match v.X < v.Z with
            | true -> 0
            | _ -> 2
        | _ ->
            match v.Y < v.Z with
            | true -> 1
            | _ -> 2

    let inline snap v =
        create (truncate v.X) (truncate v.Y) (truncate v.Z)
        
    let inline multiplyAdd s v1 v2 =
        create (s * v1.X + v2.X) (s * v1.Y + v2.Y) (s * v1.Z + v2.Z) 

    let inline dotProduct v1 v2 =
        (v1.X * v2.X) + (v1.Y * v2.Y) + (v1.Z * v2.Z)

    let inline crossProduct v1 v2 =
        create
            ((v1.Y * v2.Z) - (v1.Z * v2.Y))
            ((v1.Z * v2.X) - (v1.X * v2.Z))
            ((v1.X * v2.Y) - (v1.Y * v2.X))

    let inline lengthSquared v =
        (v.X * v.X) + (v.Y * v.Y) + (v.Z * v.Z)

    let inline length v =
        sqrt <| lengthSquared v

    let inline normalize v =
        let length = 1.f / length v
        create (v.X * length) (v.Y * length) (v.Z * length)

    let inline perpendicular v =
        let uv =
            match abs v |> minDimension with
            | 0 -> unitX
            | 1 -> unitY
            | 2 -> unitZ
            | _ -> raise <| System.ArgumentOutOfRangeException ()

        let uvNormal = normalize uv
        crossProduct v uvNormal
        
    let (|XYZ|) v =
        (v.X, v.Y, v.Z)

type Vector3 with
    static member inline (*) (v1, v2) =
        Vector3.create (v1.X * v2.X) (v1.Y * v2.Y) (v1.Z * v2.Z)

    static member inline (/) (v1, v2) =
        Vector3.create (v1.X / v2.X) (v1.Y / v2.Y) (v1.Z / v2.Z)

    static member inline (+) (v1, v2) =
        Vector3.create (v1.X + v2.X) (v1.Y + v2.Y) (v1.Z + v2.Z)

    static member inline (-) (v1, v2) =
        Vector3.create (v1.X - v2.X) (v1.Y - v2.Y) (v1.Z - v2.Z)

    // RHS

    static member inline (*) (v, s) =
        Vector3.create (v.X * s) (v.Y * s) (v.Z * s)

    static member inline (/) (v, s) =
        Vector3.create (v.X / s) (v.Y / s) (v.Z / s)

    static member inline (+) (v, s) =
        Vector3.create (v.X + s) (v.Y + s) (v.Z + s)

    static member inline (-) (v, s) =
        Vector3.create (v.X - s) (v.Y - s) (v.Z - s)

    // LHS

    static member inline (*) (s, v) =
        Vector3.create (s * v.X) (s * v.Y) (s * v.Z)

    static member inline (/) (s, v) =
        Vector3.create (s / v.X) (s / v.Y) (s / v.Z)

    static member inline (+) (s, v) =
        Vector3.create (s + v.X) (s + v.Y) (s + v.Z)

    static member inline (-) (s, v) =
        Vector3.create (s - v.X) (s - v.Y) (s - v.Z)

/// Vector4      
type Vector4 =
    { X: single; Y: single; Z: single; W: single }
    
    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.X
            | 1 -> this.Y
            | 2 -> this.Z
            | 3 -> this.W
            | _ -> raise <| IndexOutOfRangeException ()

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Vector4 =
    let inline create x y z w =
        { X = x; Y = y; Z = z; W = w }

    let zero = create 0.f 0.f 0.f 0.f

    let inline dotProduct v1 v2 =
        (v1.X * v2.X) + (v1.Y * v2.Y) + (v1.Z * v2.Z) + (v1.W * v2.W)

type Vector4 with
    static member inline (*) (v1, v2) =
        Vector4.create (v1.X * v2.X) (v1.Y * v2.Y) (v1.Z * v2.Z) (v1.W * v2.W)
        
    static member inline (+) (v1, v2) =
        Vector4.create (v1.X + v2.X) (v1.Y + v2.Y) (v1.Z + v2.Z) (v1.W + v2.W)

    static member inline (-) (v1, v2) =
        Vector4.create (v1.X - v2.X) (v1.Y - v2.Y) (v1.Z - v2.Z) (v1.W - v2.W)    

/// Matrix2x2
type Matrix2x2 =
    {
        M0_0: single;
        M0_1: single;
        M1_0: single;
        M1_1: single;
    }

    member inline this.Item
            with get (i, j) =
                match (i, j) with
                | (0, 0) -> this.M0_0
                | (0, 1) -> this.M0_1
                | (1, 0) -> this.M1_0
                | (1, 1) -> this.M1_1
                | _ -> raise <| IndexOutOfRangeException ()

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Matrix2x2 =
    let zero =
        {
            M0_0 = 0.f;
            M0_1 = 0.f;
            M1_0 = 0.f;
            M1_1 = 0.f;
        }

/// Matrix3x3        
type Matrix3x3 =     
    {
        M0_0: single;
        M0_1: single;
        M0_2: single;
        M1_0: single;
        M1_1: single;
        M1_2: single;
        M2_0: single;
        M2_1: single;
        M2_2: single;
    }
    
    member inline this.Item
            with get (i, j) =
                match (i, j) with
                | (0, 0) -> this.M0_0
                | (0, 1) -> this.M0_1
                | (0, 2) -> this.M0_2
                | (1, 0) -> this.M1_0
                | (1, 1) -> this.M1_1
                | (1, 2) -> this.M1_2
                | (2, 0) -> this.M2_0
                | (2, 1) -> this.M2_1
                | (2, 2) -> this.M2_2
                | _ -> raise <| IndexOutOfRangeException ()

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Matrix3x3 =
    let inline create m1 m2 m3 m4 m5 m6 m7 m8 m9 =
        {
            M0_0 = m1;
            M0_1 = m2;
            M0_2 = m3;
            M1_0 = m4;
            M1_1 = m5;
            M1_2 = m6;
            M2_0 = m7;
            M2_1 = m8;
            M2_2 = m9;
        }

    let zero = create 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f

/// Matrix4x4       
type Matrix4x4 =     
    {
        M0_0: single;
        M0_1: single;
        M0_2: single;
        M0_3: single;
        M1_0: single;
        M1_1: single;
        M1_2: single;
        M1_3: single;
        M2_0: single;
        M2_1: single;
        M2_2: single;
        M2_3: single;
        M3_0: single;
        M3_1: single;
        M3_2: single;
        M3_3: single;
    }
    
    member inline this.Item
            with get (i, j) =
                match (i, j) with
                | (0, 0) -> this.M0_0
                | (0, 1) -> this.M0_1
                | (0, 2) -> this.M0_2
                | (0, 3) -> this.M0_3
                | (1, 0) -> this.M1_0
                | (1, 1) -> this.M1_1
                | (1, 2) -> this.M1_2
                | (1, 3) -> this.M1_3
                | (2, 0) -> this.M2_0
                | (2, 1) -> this.M2_1
                | (2, 2) -> this.M2_2
                | (2, 3) -> this.M2_3
                | (3, 0) -> this.M3_0
                | (3, 1) -> this.M3_1
                | (3, 2) -> this.M3_2
                | (3, 3) -> this.M3_3
                | _ -> raise <| IndexOutOfRangeException ()

#if DEBUG
    static member (*) (m1: Matrix4x4, m2: Matrix4x4) =
#else
    static member inline (*) (m1: Matrix4x4, m2: Matrix4x4) =
#endif
        {
            M0_0 = (m1.[0, 0] * m2.[0, 0]) + (m1.[0, 1] * m2.[1, 0]) + (m1.[0, 2] * m2.[2, 0]) + (m1.[0, 3] * m2.[3, 0]);
            M0_1 = (m1.[0, 0] * m2.[0, 1]) + (m1.[0, 1] * m2.[1, 1]) + (m1.[0, 2] * m2.[2, 1]) + (m1.[0, 3] * m2.[3, 1]);
            M0_2 = (m1.[0, 0] * m2.[0, 2]) + (m1.[0, 1] * m2.[1, 2]) + (m1.[0, 2] * m2.[2, 2]) + (m1.[0, 3] * m2.[3, 2]);
            M0_3 = (m1.[0, 0] * m2.[0, 3]) + (m1.[0, 1] * m2.[1, 3]) + (m1.[0, 2] * m2.[2, 3]) + (m1.[0, 3] * m2.[3, 3]);
            M1_0 = (m1.[1, 0] * m2.[0, 0]) + (m1.[1, 1] * m2.[1, 0]) + (m1.[1, 2] * m2.[2, 0]) + (m1.[1, 3] * m2.[3, 0]);
            M1_1 = (m1.[1, 0] * m2.[0, 1]) + (m1.[1, 1] * m2.[1, 1]) + (m1.[1, 2] * m2.[2, 1]) + (m1.[1, 3] * m2.[3, 1]);
            M1_2 = (m1.[1, 0] * m2.[0, 2]) + (m1.[1, 1] * m2.[1, 2]) + (m1.[1, 2] * m2.[2, 2]) + (m1.[1, 3] * m2.[3, 2]);
            M1_3 = (m1.[1, 0] * m2.[0, 3]) + (m1.[1, 1] * m2.[1, 3]) + (m1.[1, 2] * m2.[2, 3]) + (m1.[1, 3] * m2.[3, 3]);
            M2_0 = (m1.[2, 0] * m2.[0, 0]) + (m1.[2, 1] * m2.[1, 0]) + (m1.[2, 2] * m2.[2, 0]) + (m1.[2, 3] * m2.[3, 0]);
            M2_1 = (m1.[2, 0] * m2.[0, 1]) + (m1.[2, 1] * m2.[1, 1]) + (m1.[2, 2] * m2.[2, 1]) + (m1.[2, 3] * m2.[3, 1]);
            M2_2 = (m1.[2, 0] * m2.[0, 2]) + (m1.[2, 1] * m2.[1, 2]) + (m1.[2, 2] * m2.[2, 2]) + (m1.[2, 3] * m2.[3, 2]);
            M2_3 = (m1.[2, 0] * m2.[0, 3]) + (m1.[2, 1] * m2.[1, 3]) + (m1.[2, 2] * m2.[2, 3]) + (m1.[2, 3] * m2.[3, 3]);
            M3_0 = (m1.[3, 0] * m2.[0, 0]) + (m1.[3, 1] * m2.[1, 0]) + (m1.[3, 2] * m2.[2, 0]) + (m1.[3, 3] * m2.[3, 0]);
            M3_1 = (m1.[3, 0] * m2.[0, 1]) + (m1.[3, 1] * m2.[1, 1]) + (m1.[3, 2] * m2.[2, 1]) + (m1.[3, 3] * m2.[3, 1]);
            M3_2 = (m1.[3, 0] * m2.[0, 2]) + (m1.[3, 1] * m2.[1, 2]) + (m1.[3, 2] * m2.[2, 2]) + (m1.[3, 3] * m2.[3, 2]);
            M3_3 = (m1.[3, 0] * m2.[0, 3]) + (m1.[3, 1] * m2.[1, 3]) + (m1.[3, 2] * m2.[2, 3]) + (m1.[3, 3] * m2.[3, 3]);
        }

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Matrix4x4 =
    let inline create m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 m16 =
        {
            M0_0 = m1;
            M0_1 = m2;
            M0_2 = m3;
            M0_3 = m4;
            M1_0 = m5;
            M1_1 = m6;
            M1_2 = m7;
            M1_3 = m8;
            M2_0 = m9;
            M2_1 = m10;
            M2_2 = m11;
            M2_3 = m12;
            M3_0 = m13;
            M3_1 = m14;
            M3_2 = m15;
            M3_3 = m16;
        }

    let zero = create 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f 0.f

/// Quaternion
type Quaternion =
    { X: single; Y: single; Z: single; W: single }

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.W
            | 1 -> this.X
            | 2 -> this.Y
            | 3 -> this.Z
            | _ -> raise <| IndexOutOfRangeException ()

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Quaternion =
    let inline create w x y z =
        { Quaternion.W = w; X = x; Y = y; Z = z }

    let inline conjugate (q: Quaternion) =
        create q.W -q.X -q.Y -q.Z

    let ofEuler (v: Vector3) =
        let pitch = Math.PI / 360.f * v.[0]
        let yaw = Math.PI / 360.f * v.[1]
        let roll = Math.PI / 360.f * v.[2]

        let sinRoll = sin roll
        let sinPitch = sin pitch
        let sinYaw = sin yaw

        let cosRoll = cos roll
        let cosPitch = cos pitch
        let cosYaw = cos yaw

        let cosPitchYaw = cosPitch * cosYaw
        let sinPitchYaw = sinPitch * sinYaw

        create
            (cosRoll * cosPitchYaw + sinRoll * sinPitchYaw)
            (sinRoll * cosPitchYaw - cosRoll * sinPitchYaw)
            (cosRoll * sinPitch * cosYaw + sinRoll * cosPitch * sinYaw)
            (cosRoll * cosPitch * sinYaw - sinRoll * sinPitch * cosYaw)

    let ofAxisAngle (axis: Vector3) (angle: single) =
        let angle = angle * 0.5f
        let sinAngle = sin angle
        let cosAngle = cos angle

        create
            (axis.X * sinAngle)
            (axis.Y * sinAngle)
            (axis.Z * sinAngle)
            cosAngle

type Quaternion with
    static member inline (*) (q1, q2) =
        Quaternion.create
            (q1.W * q2.W - q1.X * q2.X - q1.Y * q2.Y - q1.Z * q2.Z)
            (q1.W * q2.X + q1.X * q2.W + q1.Y * q2.Z - q1.Z * q2.Y)
            (q1.W * q2.Y + q1.Y * q2.W + q1.Z * q2.X - q1.X * q2.Z)
            (q1.W * q2.Z + q1.Z * q2.W + q1.X * q2.Y - q1.Y * q2.X)

    static member inline (*) (q, v) =
        let vn = Vector3.normalize v
        let vq = Quaternion.create 0.f vn.X vn.Y vn.Z
        let result = q * (vq * Quaternion.conjugate q)

        Vector3.create result.X result.Y result.Z
