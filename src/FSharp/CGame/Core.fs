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

module CGame.Core

open Engine.Core
open Engine.Math
open Engine.Renderer.Core

module Constants =
    [<Literal>]
    let LandDeflectTime = 150

    [<Literal>]
    let LandReturnTime = 300

/// Based on Q3: playerState_t
/// PlayerState
///
/// playerState_t is the information needed by both the client and server
/// to predict player motion and actions
/// nothing outside of pmove should modify these, or some degree of prediction error
/// will occur

/// you can't add anything to this without modifying the code in msg.c

/// playerState_t is a full superset of entityState_t as it is used by players,
/// so if a playerState_t is transmitted, the entityState_t can be fully derived
/// from it.
/// TODO: Move this where to a "Shared" project.
/// TODO: Not finished.
type PlayerState =
    {
        Origin: Vector3;
        Velocity: Vector3;

        /// for fixed views
        ViewAngles: Vector3; 
    }

/// Based on Q3: snapshot_t
/// Snapshot
///
/// snapshots are a view of the server at a given time

/// Snapshots are generated at regular time intervals by the server,
/// but they may not be sent if a client's rate level is exceeded, or
/// they may be dropped by the network.
/// TODO: Not finished.
type Snapshot =
    {
        /// complete information about the current player at this time
        PlayerState: PlayerState;
    }

/// Based on Q3: cg_t
/// CGame
///
/// types are currently unordered, sort of
/// TODO: No-where near finished with this record.
type CGame =
    {
        /// cg.snap->serverTime <= cg.time
        CurrentSnapshot: Snapshot option;

        /// cg.nextSnap->serverTime > cg.time, or NULL
        NextSnapshot: Snapshot option;

        /// (float)( cg.time - cg.frame->serverTime ) / (cg.nextFrame->serverTime - cg.frame->serverTime)
        FrameInterpolation: single;

        /// this is the time value that the client
        /// is rendering at.
        Time: int;

        PredictedPlayerState: PlayerState;

        // for landing hard
        LandChange: single;
        LandTime: int;

        Refdef: Refdef;
        RefdefViewAngles: Vector3;

        // temp working variables for player view
        BobCycle: int;
        BobFractionSin: single;
        XYSpeed: single;
    }