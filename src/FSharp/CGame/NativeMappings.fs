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
open Engine.Renderer.Native
open CGame.Core

module CGame =
    let inline ofNativePtr (ptr: nativeptr<cg_t>) =
        let mutable native = NativePtr.read ptr
        {
            Time = native.time;
            LandChange = native.landChange;
            LandTime = native.landTime;
            Refdef = Refdef.ofNative native.refdef;
            RefdefViewAngles = Vector3.ofNative native.refdefViewAngles
            BobCycle = native.bobcycle;
            BobFractionSin = native.bobfracsin;
            XYSpeed = native.xyspeed;
        }