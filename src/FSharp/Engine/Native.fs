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
open FSharpx.Collections
open Engine.Core
open Engine.Net
open Engine.Math
open Engine.NativeInterop

type qboolean =
    | qfalse = 0
    | qtrue = 1

type qhandle_t = int

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type vec2_t =
    val mutable value : single
    val mutable value1 : single

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type vec3_t =
    val mutable value : single
    val mutable value1 : single
    val mutable value2 : single

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type vec4_t =
    val mutable value : single
    val mutable value1 : single
    val mutable value2 : single
    val mutable value3 : single

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

type netadrtype_t =
    | NA_BOT = 0
    | NA_BAD = 1
    | NA_LOOPBACK = 2
    | NA_BROADCAST = 3
    | NA_IP = 4
    | NA_IPX = 5
    | NA_BROADCAST_IPX = 6

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type netadr_t =
    val mutable type' : netadrtype_t
    val mutable ip : byte
    val private ip1 : byte
    val private ip2 : byte
    val private ip3 : byte
    val mutable ipx : byte
    val private ipx1 : byte
    val private ipx2 : byte
    val private ipx3 : byte
    val private ipx4 : byte
    val private ipx5 : byte
    val private ipx6 : byte
    val private ipx7 : byte
    val private ipx8 : byte
    val private ipx9 : byte
    val mutable port : uint16

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type msg_t =
    val mutable allowoverflow : qboolean
    val mutable overflowed : qboolean
    val mutable oob : qboolean
    val mutable data : nativeptr<byte>
    val mutable maxsize : int
    val mutable cursize : int
    val mutable readcount : int
    val mutable bit : int

