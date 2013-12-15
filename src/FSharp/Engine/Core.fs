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

module Engine.Core

#nowarn "9"

open System
open System.Runtime.InteropServices
open Engine.Math

module Constants =
    [<Literal>]
    let GEntityIdBits = 10

    let MaxGEntities = (1 <<< GEntityIdBits)

    // entityIds are communicated with GEntityIdBits, so any reserved
    // values that are going to be communcated over the net need to
    // also be in this range
    let EntityIdNone = MaxGEntities - 1
    let EntityIdWorld = MaxGEntities - 2
    let EntityIdMaxNormal = MaxGEntities - 2

/// Axis
[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Axis =
    val X : vec3
    val Y : vec3
    val Z : vec3

    new (x, y, z) = { X = x; Y = y; Z = z }

    static member inline Create (x, y, z) =
        Axis (x, y, z)

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.X | 1 -> this.Y | 2 -> this.Z
            | _ -> raise <| IndexOutOfRangeException ()

    member inline this.Set (?X: vec3, ?Y: vec3, ?Z: vec3) =
        Axis.Create (
            (match X with | Some x -> x | None -> this.X),
            (match Y with | Some y -> y | None -> this.Y),
            (match Z with | Some z -> z | None -> this.Z)
        )

/// Axis Module
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Axis =
    let inline create x y z =
        Axis.Create (x, y, z)

    let zero =      create Vec3.zero Vec3.zero Vec3.zero
    let identity =  create Vec3.right Vec3.up Vec3.forward

/// <summary>
/// Based on Q3: cvar_t
/// Cvar
/// </summary>
type Cvar =
    {
        Name: string;
        String: string;
        ResetString: string;        // cvar_restart will reset to this value
        LatchedString: string;      // for CVAR_LATCH vars
        Flags: int;
        IsModified: bool;           // set each time the cvar is changed
        ModificationCount: int;     // incremented each time the cvar is changed
        Value: single;              // atof( string )
        Integer: int;               // atoi( string )
    }

/// Bounds
type Bounds = 
    { Mins: vec3; Maxs: vec3 }   

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.Mins | 1 -> this.Maxs
            | _ -> raise <| IndexOutOfRangeException ()

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Bounds =
    let radius (bounds: Bounds) =
        let inline f i =
            let a = abs bounds.Mins.[i]
            let b = abs bounds.Maxs.[i]
            if a > b then a else b
        
        Vec3.length <| vec3 (f 0, f 1, f 2)

