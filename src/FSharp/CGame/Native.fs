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

namespace CGame.Native

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading
open System.Diagnostics
open System.Diagnostics.Contracts
open Microsoft.FSharp.NativeInterop
open Engine.Core
open Engine.Math
open Engine.Native
open Engine.NativeInterop

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type snapshot_t =
    val mutable snapFlags : int
    val mutable pint : int
    val mutable serverTime : int
    // TODO:

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type cg_t =
    val mutable clientFrame : int
    val mutable clientNum : int
    val mutable demoPlayback : qboolean
    val mutable levelShot : qboolean
    val mutable deferredPlayerLoading : int
    val mutable laoding : qboolean
    val mutable intermissionStarted : qboolean
    val mutable latestSnapshotNum : int
    val mutable latestSnapshotTime : int
    // TODO:
