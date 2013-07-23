(*
Copyright © 2013 OpenFK

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
Copyright © 1999-2005 Id Software, Inc.
*)

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

