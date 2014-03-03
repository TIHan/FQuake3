(*
Copyright (C) 2013-2014 William F. Smith

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

module Engine.Renderer.Surface

open FSharp.Game.Math
open Engine.Core
open Engine.Renderer.Core
open Engine.Renderer.Shader
open FQuake3.Md3
open FQuake3.Math

/// Based on Q3: RB_SurfaceMesh
/// SurfaceMesh
/// TODO: Finish.
let surfaceMesh (rentity: TrRefEntity) (surface: Md3Surface) =
    let entity = rentity.Entity
    let backLerp =
        match entity.OldFrame = entity.Frame with
        | true -> 0.f
        | _ -> entity.BackLerp
    ()

/// Based on Q3: LerpMeshVertexes
/// TODO: Finish.
let lerpMeshVertexes (backLerp: single) (surface: Md3Surface) =
    ()