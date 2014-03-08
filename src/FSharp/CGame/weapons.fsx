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

module CGame.Weapons

open System.Diagnostics.Contracts
open FSharp.Game.Math
open Engine.Renderer
open CGame.Core

/// Based on Q3: CG_CalculateWeaponPosition
/// CalculateWeaponPosition
[<Pure>]
let calculateWeaponPosition (cg: CGame) =
    let origin = cg.Refdef.ViewOrigin
    let angles = cg.RefdefViewAngles

    // on odd legs, invert some angles
    let scale =
        match cg.BobCycle &&& 1 with
        | 0 -> -cg.XYSpeed
        | _ -> cg.XYSpeed

    // gun angles from bobbing
    let angles =
        vec3 (
            (angles.x + (cg.XYSpeed * cg.BobFractionSin * 0.005f)), // PITCH
            (angles.y + (scale * cg.BobFractionSin * 0.01f)), // YAW
            (angles.z + (scale * cg.BobFractionSin * 0.005f)) // ROLL
        )
        
    let originZ =
        // drop the weapon when landing
        match cg.DeltaLandTime with
        | x when x < Constants.LandDeflectTime ->
            origin.z + (cg.LandChange * 0.25f * (single x / single Constants.LandDeflectTime))
        | x when x < Constants.LandDeflectTime + Constants.LandReturnTime ->
            origin.z + (cg.LandChange * 0.25f * single (Constants.LandDeflectTime + Constants.LandReturnTime - x) / single Constants.LandReturnTime)
        | _ -> origin.z

    // idle drift
    let scale = cg.XYSpeed + 40.f
    let fractionSin = sin <| single cg.Time * 0.001f
    let angles =
        vec3 (
            (angles.x + (scale * fractionSin * 0.01f)), // PITCH
            (angles.y + (scale * fractionSin * 0.01f)), // YAW
            (angles.z + (scale * fractionSin * 0.01f)) // ROLL
        )

    (origin.Set (z = originZ), angles)

