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

    val mutable generic1 : int
    val mutable loopSound : int
    val mutable jumppad_ent : int

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

// Size: 36
[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type trajectory_t =
    val mutable trType : trType_t
    val mutable trTime : int
    val mutable trDuration : int
    val mutable trBase : vec3_t
    val mutable trDelta : vec3_t

// Size: 208
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

// 256 Entities
[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = 53248)>]
type snapshot_t_entities =
    [<FieldOffset (0)>]
    val private value : entityState_t

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
    val mutable entities : snapshot_t_entities
    val mutable numServerCommands : int
    val mutable serverCommandSequence : int

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type animation_t =
    val mutable firstFrame : int
    val mutable numFrames : int
    val mutable loopFrames : int
    val mutable frameLerp : int
    val mutable initialLerp : int
    val mutable reversed : int
    val mutable flipflop : int

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type lerpFrame_t =
    val mutable oldFrame : int
    val mutable oldFrameTime : int
    val mutable frame : int
    val mutable frameTime : int
    val mutable backLerp : single
    val mutable yawAngle : single
    val mutable yawing : qboolean
    val mutable pitchAngle : single
    val mutable pitching : qboolean
    val mutable animationNumber : int
    val mutable animation : nativeptr<animation_t>
    val mutable animationTime : int

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type playerEntity_t =
    val legs : lerpFrame_t
    val torso : lerpFrame_t
    val flag : lerpFrame_t
    val painTime : int
    val painDirection : int
    val lightningFiring : int
    val railgunImpact : vec3_t
    val railgunFlash : qboolean
    val barrelAngle : single
    val barrelTime : single
    val barrelSpinning : qboolean

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type centity_t =
    val mutable currentState : entityState_t
    val mutable nextState : entityState_t
    val mutable interpolate : qboolean
    val mutable currentValid : qboolean
    val mutable muzzleFlashtime : int
    val mutable previousEvent : int
    val mutable teleportFlag : int
    val mutable trailTime : int
    val mutable dustTrailTime : int
    val mutable miscTime : int
    val mutable snapShotTime : int
    val mutable pe : playerEntity_t
    val mutable errorTime : int
    val mutable errorOrigin : vec3_t
    val mutable errorAngles : vec3_t
    val mutable extrapolated : qboolean
    val mutable rawOrigin : vec3_t
    val mutable rawAngles : vec3_t
    val mutable beamEnd : vec3_t
    val mutable lerpOrigin : vec3_t
    val mutable lerpAngles : vec3_t

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
    val mutable snap : nativeptr<snapshot_t>
    val mutable nextSnap : nativeptr<snapshot_t>
    
    val mutable activeSnapshots : snapshot_t
    val private activeSnapshots1 : snapshot_t

    val mutable frameInterpolation : single
    val mutable thisFrameTeleport : qboolean
    val mutable nextFrameTeleport : qboolean
    val mutable frametime : int
    val mutable time : int
    val mutable oldTime : int
    val mutable physicsTime : int
    val mutable timelimitWarnings : int
    val mutable fraglimitWarnings : int
    val mutable mapRestart : qboolean
    val mutable renderingThirdPerson : qboolean
    val mutable hyperspace : qboolean
    val mutable predictedPlayerState : playerState_t
    val mutable predictedPlayerEntity : centity_t
    val mutable validPPS : qboolean
    val mutable predictedErrorTime : int
    val mutable predictedError : vec3_t
    val mutable eventSequence : int

    val mutable predictableEvents : int
    val private predictableEvents1 : int
    val private predictableEvents2 : int
    val private predictableEvents3 : int
    val private predictableEvents4 : int
    val private predictableEvents5 : int
    val private predictableEvents6 : int
    val private predictableEvents7 : int
    val private predictableEvents8 : int
    val private predictableEvents9 : int
    val private predictableEvents10 : int
    val private predictableEvents11 : int
    val private predictableEvents12 : int
    val private predictableEvents13 : int
    val private predictableEvents14 : int
    val private predictableEvents15 : int

    val mutable stepChange : single
    val mutable stepTime : int
    val mutable duckChange : single
    val mutable duckTime : single
    val mutable landChange : single
    val mutable landTime : int
    val mutable weaponSelect : int
    val mutable autoAngles : vec3_t

    val mutable autoAxis : vec3_t
    val private autoAxis1 : vec3_t
    val private autoAxis2 : vec3_t

    val mutable autoAnglesFast : vec3_t
    
    val mutable autoAxisFast : vec3_t
    val private autoAxisFast1 : vec3_t
    val private autoAxisFast2 : vec3_t

    val mutable refdef : refdef_t
    val mutable refdefViewAngles : vec3_t
    // TODO:
