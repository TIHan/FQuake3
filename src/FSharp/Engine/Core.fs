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

namespace Engine.Core

// Disable native interop warnings
#nowarn "9"
#nowarn "51"

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading
open System.Diagnostics
open Microsoft.FSharp.NativeInterop
open Engine.Math
open Engine.NativeInterop

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
    { Bounds1: Vector3; Bounds2: Vector3 }   

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.Bounds1
            | 1 -> this.Bounds2
            | _ -> raise <| IndexOutOfRangeException ()

/// Axis
type Axis =
    { X: Vector3; Y: Vector3; Z: Vector3 }

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.X
            | 1 -> this.Y
            | 2 -> this.Z
            | _ -> raise <| IndexOutOfRangeException ()

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Axis =
    let zero = { X = Vector3.zero; Y = Vector3.zero; Z = Vector3.zero }
    let identity = { X = Vector3.unitX; Y = Vector3.unitY; Z = Vector3.unitZ }

module Option =
    let inline ofString str = match str with null -> None | "" -> None | _ -> Some str

