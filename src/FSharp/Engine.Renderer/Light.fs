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

module Engine.Renderer.Light

open System
open System.Diagnostics.Contracts
open Engine.Core
open Engine.Files
open Engine.Math
open Engine.Renderer.Core

/// Based on Q3: setupEntityLightingGrid
/// SetupEntityLightingGrid
let setupEntityLightingGrid (rentity: TrRefEntity) (lightGrid: LightGrid) (r_ambientScale: Cvar) (r_directedScale: Cvar) =
    let entity = rentity.Entity

    let lightOrigin =
        match entity.RenderFx.HasFlag RenderFxFlags.LightingOrigin with
        // seperate lightOrigins are needed so an object that is
        // sinking into the ground can still be lit, and so
        // multi-part models can be lit identically
        | true -> entity.LightingOrigin
        | _ -> entity.Origin
        |> (-) lightGrid.Origin

    let inline f i = lightOrigin.[i] * lightGrid.InverseSize.[i]

    let v = vec3 (f 0, f 1, f 2)
    let pos = floor v
    let frac = v - pos
    let pos = 
        LightGridBounds (
            clamp (int pos.x) 0 (lightGrid.Bounds.x - 1),
            clamp (int pos.y) 0 (lightGrid.Bounds.y - 1),
            clamp (int pos.z) 0 (lightGrid.Bounds.z - 1))

    // trilerp the light value
    let gridStepX = 8
    let gridStepY = 8 * lightGrid.Bounds.x
    let gridStepZ = 8 * lightGrid.Bounds.x * lightGrid.Bounds.y
    let gridIndex = (pos.x * gridStepX) + (pos.y * gridStepY) + (pos.z * gridStepZ)

    let inline gridStep i =
        match i with
        | 0 -> gridStepX
        | 1 -> gridStepY
        | 2 -> gridStepZ
        | _ -> raise <| System.ArgumentOutOfRangeException ()

    let rec calculateFactor i factor gridIndex n =
        match n with
        | 3 -> factor, gridIndex
        | _ ->
            match (i &&& (1 <<< n)) <> 0 with
            | true ->
                calculateFactor i (factor * frac.[n]) (gridIndex + gridStep n) (n + 1)
            | _ ->
                calculateFactor i (factor * (1.f - frac.[n])) gridIndex (n + 1)
            
    let rec calculateTotalFactor totalFactor direction rentity n =
        match n with
        | 8 -> totalFactor, direction, rentity
        | _ ->
            let factor, gridIndex = calculateFactor n 1.f gridIndex 0

            match lightGrid.Data.[gridIndex] + lightGrid.Data.[gridIndex + 1] + lightGrid.Data.[gridIndex + 2] with
            // ignore samples in walls
            | 0uy ->
                calculateTotalFactor totalFactor direction rentity (n + 1)
            | _ ->

            let ambientLight =
                vec3 (
                    single <| lightGrid.Data.[gridIndex],
                    single <| lightGrid.Data.[gridIndex + 1],
                    single <| lightGrid.Data.[gridIndex + 2])

            let directedLight =
                vec3 (
                    single <| lightGrid.Data.[gridIndex + 3],
                    single <| lightGrid.Data.[gridIndex + 4],
                    single <| lightGrid.Data.[gridIndex + 5])

            let rentity = 
                { rentity with
                    AmbientLight = rentity.AmbientLight + ambientLight * factor
                    DirectedLight = rentity.DirectedLight + directedLight * factor }

            let long = single lightGrid.Data.[gridIndex + 6]
            let lat = single lightGrid.Data.[gridIndex + 7]

            let normal =
                vec3 (
                    cos lat * sin long,
                    sin lat * sin long,
                    cos long)
            
            let direction = Vec3.multiplyAdd factor normal direction

            calculateTotalFactor (totalFactor + factor) direction rentity (n + 1)

    let totalFactor, direction, rentity = calculateTotalFactor 0.f Vec3.zero rentity 0

    let rentity =
        match totalFactor > 0.f && totalFactor < 0.99f with
        | false -> rentity
        | _ ->
            let factor = 1.f / totalFactor
            { rentity with 
                AmbientLight = rentity.AmbientLight * factor 
                DirectedLight = rentity.DirectedLight * factor }

    { rentity with
        AmbientLight = rentity.AmbientLight * r_ambientScale.Value
        DirectedLight = rentity.DirectedLight * r_directedScale.Value
        LightDirection = direction }


/// Based on Q3: R_SetupEntityLighting
/// SetupEntityLighting
///
/// Calculates all the lighting values that will be used
/// by the Calc_* functions
let setupEntityLighting (rentity: TrRefEntity) =
    // lighting calculations
    match rentity.IsLightingCalculated with
    | true -> rentity
    | _ ->

    let rentity = { rentity with IsLightingCalculated = true }
    let entity = rentity.Entity

    //
    // trace a sample point down to find ambient light
    //
    let lightOrigin =
        match entity.RenderFx.HasFlag RenderFxFlags.LightingOrigin with
        // seperate lightOrigins are needed so an object that is
        // sinking into the ground can still be lit, and so
        // multi-part models can be lit identically
        | true -> entity.LightingOrigin
        | _ -> entity.Origin

    // TODO:
    rentity