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
type playerState_t =
    val mutable commandTime : int
    val mutable pm_type : int
    val mutable bobCycle : int
    val mutable pm_flags : int
    val mutable pm_time : int
    val mutable origin : vec3_t
    val mutable velocity : vec3_t
    val mutable weaponTime : int
    val mutable gravity : int
    val mutable speed : int

    val mutable delta_angles : int
    val private delta_angles1 : int
    val private delta_angles2 : int

    val mutable groundEntityNum : int
    val mutable legsTimer : int
    val mutable legsAnim : int
    val mutable torsoTimer : int
    val mutable torsoAnim : int
    val mutable movementDir : int
    val mutable grapplePoint : vec3_t
    val mutable eFlags : int
    val mutable eventSequence : int

    val mutable events : int
    val private events1 : int
    
    val mutable eventParms : int
    val private eventParms1 : int

    val mutable externalEvent : int
    val mutable externalEventParm : int
    val mutable externalEventTime : int
    val mutable clientNum : int
    val mutable weapon : int
    val mutable weaponstate : int
    val mutable viewangles : vec3_t
    val mutable viewheight : int
    val mutable damageEvent : int
    val mutable damageYaw : int
    val mutable damagePitch : int
    val mutable damageCount : int
    
    val mutable stats : int
    val private stats1 : int
    val private stats2 : int
    val private stats3 : int
    val private stats4 : int
    val private stats5 : int
    val private stats6 : int
    val private stats7 : int
    val private stats8 : int
    val private stats9 : int
    val private stats10 : int
    val private stats11 : int
    val private stats12 : int
    val private stats13 : int
    val private stats14 : int
    val private stats15 : int

    val mutable persistant : int
    val private persistant1 : int
    val private persistant2 : int
    val private persistant3 : int
    val private persistant4 : int
    val private persistant5 : int
    val private persistant6 : int
    val private persistant7 : int
    val private persistant8 : int
    val private persistant9 : int
    val private persistant10 : int
    val private persistant11 : int
    val private persistant12 : int
    val private persistant13 : int
    val private persistant14 : int
    val private persistant15 : int

    val mutable powerups : int
    val private powerups1 : int
    val private powerups2 : int
    val private powerups3 : int
    val private powerups4 : int
    val private powerups5 : int
    val private powerups6 : int
    val private powerups7 : int
    val private powerups8 : int
    val private powerups9 : int
    val private powerups10 : int
    val private powerups11 : int
    val private powerups12 : int
    val private powerups13 : int
    val private powerups14 : int
    val private powerups15 : int

    val mutable ammo : int
    val private ammo1 : int
    val private ammo2 : int
    val private ammo3 : int
    val private ammo4 : int
    val private ammo5 : int
    val private ammo6 : int
    val private ammo7 : int
    val private ammo8 : int
    val private ammo9 : int
    val private ammo10 : int
    val private ammo11 : int
    val private ammo12 : int
    val private ammo13 : int
    val private ammo14 : int
    val private ammo15 : int

    val mutable ping : int
    val mutable pmove_framecount : int
    val mutable jumppad_frame : int
    val mutable entityEventSequence : int

type trType_t =
    | TR_STATIONARY = 0
    | TR_INTERPOLATE = 1
    | TR_LINEAR = 2
    | TR_LINEAR_STOP = 3
    | TR_SINE = 4
    | TR_GRAVITY = 5

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type trajectory_t =
    val mutable trType : trType_t
    val mutable trTime : int
    val mutable trDuration : int
    val mutable trBase : vec3_t
    val mutable trDelta : vec3_t

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type entityState_t =
    val mutable number : int
    val mutable eType : int
    val mutable eFlags : int
    val mutable pos : trajectory_t
    val mutable apos : trajectory_t
    val mutable time : int
    val mutable time2 : int
    val mutable origin : vec3_t
    val mutable origin2 : vec3_t
    val mutable angles : vec3_t
    val mutable angles2 : vec3_t
    val mutable otherEntityNum : int
    val mutable otherentityNum2 : int
    val mutable groundEntityNum : int
    val mutable constantLight : int
    val mutable loopsound : int
    val mutable modelindex : int
    val mutable modelindex2 : int
    val mutable clientNum : int
    val mutable frame : int
    val mutable solid : int
    val mutable event : int
    val mutable eventParm : int
    val mutable powerups : int
    val mutable weapon : int
    val mutable legsAnim : int
    val mutable torsoanim : int
    val mutable generic1 : int

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type snapshot_t =
    val mutable snapFlags : int
    val mutable pint : int
    val mutable serverTime : int

    val mutable areamask : byte
    val private areamask1 : byte
    val private areamask2 : byte
    val private areamask3 : byte
    val private areamask4 : byte
    val private areamask5 : byte
    val private areamask6 : byte
    val private areamask7 : byte
    val private areamask8 : byte
    val private areamask9 : byte
    val private areamask10 : byte
    val private areamask11 : byte
    val private areamask12 : byte
    val private areamask13 : byte
    val private areamask14 : byte
    val private areamask15 : byte
    val private areamask16 : byte
    val private areamask17 : byte
    val private areamask18 : byte
    val private areamask19 : byte
    val private areamask20 : byte
    val private areamask21 : byte
    val private areamask22 : byte
    val private areamask23 : byte
    val private areamask24 : byte
    val private areamask25 : byte
    val private areamask26 : byte
    val private areamask27 : byte
    val private areamask28 : byte
    val private areamask29 : byte
    val private areamask30 : byte
    val private areamask31 : byte

    val mutable ps : playerState_t
    val mutable numEntities : int
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
