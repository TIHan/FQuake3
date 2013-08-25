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

namespace Engine.Native

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading
open System.Diagnostics
open Microsoft.FSharp.NativeInterop
open Engine.Core
open Engine.NativeInterop

type qboolean =
    | qfalse = 0
    | qtrue = 1

type qhandle_t = int

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type cvar_t =
    val mutable name : nativeptr<sbyte>
    val mutable string : nativeptr<sbyte>
    val mutable resetString : nativeptr<sbyte>
    val mutable latchedString : nativeptr<sbyte>
    val mutable flags : int
    val mutable modified : qboolean
    val mutable modificationCount : int
    val mutable value : single
    val mutable integer : int
    val mutable next : nativeptr<cvar_t>
    val mutable hashNext : nativeptr<cvar_t>

(*
=======================================================================================================================
Mappings
=======================================================================================================================
*)

module Cvar =
    let inline ofNative (native: cvar_t) =
        {
            Name = NativePtr.toString native.name;
            String = NativePtr.toString native.string;
            ResetString = NativePtr.toString native.resetString;
            LatchedString = NativePtr.toString native.latchedString;
            Flags = native.flags;
            IsModified = Convert.ToBoolean native.modified;
            ModificationCount = native.modificationCount;
            Value = native.value;
            Integer = native.integer;
        }

