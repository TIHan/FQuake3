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

namespace CGame.Native

// Disable native interop warnings
#nowarn "9"
#nowarn "51"

open Microsoft.FSharp.NativeInterop
open Engine.Native
open Engine.NativeInterop
open Engine.Renderer.Native
open CGame.Core

module PlayerState =
    let inline ofNativePtr (ptr: nativeptr<playerState_t>) =
        let mutable native = NativePtr.read ptr

        {
            Origin = Vector3.ofNativePtr &&native.origin;
            Velocity = Vector3.ofNativePtr &&native.velocity;
            ViewAngles = Vector3.ofNativePtr &&native.viewangles
        }

module Snapshot =
    let inline ofNativePtr (ptr: nativeptr<snapshot_t>) =
        let mutable native = NativePtr.read ptr

        {
            PlayerState = PlayerState.ofNativePtr &&native.ps
        }

module CGame =
    let inline ofNativePtr (ptr: nativeptr<cg_t>) =
        let mutable native = NativePtr.read ptr

        {
            CurrentSnapshot = Option.ofNativePtr (fun x -> Snapshot.ofNativePtr x) native.snap;
            NextSnapshot = Option.ofNativePtr (fun x -> Snapshot.ofNativePtr x) native.nextSnap;
            FrameInterpolation = native.frameInterpolation;
            Time = native.time;
            OldTime = native.oldTime;
            PredictedPlayerState = PlayerState.ofNativePtr &&native.predictedPlayerState;
            LandChange = native.landChange;
            LandTime = native.landTime;
            Refdef = Refdef.ofNativePtr &&native.refdef;
            RefdefViewAngles = Vector3.ofNativePtr &&native.refdefViewAngles
            BobCycle = native.bobcycle;
            BobFractionSin = native.bobfracsin;
            XYSpeed = native.xyspeed;
        }