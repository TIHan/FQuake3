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

namespace Engine

#nowarn "9"

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading
open System.Diagnostics
open Microsoft.FSharp.NativeInterop

module NativePtr =
    let inline toStructure<'T,'U when 'T : struct and 'U : unmanaged> (native: nativeptr<'U>) =
        let mutable s = Unchecked.defaultof<'T>
        Marshal.PtrToStructure (NativePtr.toNativeInt native, s)
        s

module List =
    let inline ofNativePtrArray<'T when 'T : unmanaged> size (native: nativeptr<'T>) =
        List.init size (fun i ->
            NativePtr.get native i
        )

    let inline ofNativePtrObjArray<'T,'U when 'T : (static member ofNative : 'U -> 'T) and 'U : unmanaged> size (native: nativeptr<'U>) =
        List.init size (fun i ->
            (^T : (static member ofNative : 'U -> 'T) (NativePtr.get native i))
        )

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
        ModificiationCount: int;    // incremented each time the cvar is changed
        Value: single;              // atof( string )
        Integer: int;               // atoi( string )
    }