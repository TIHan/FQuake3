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
open FSharp.Game.Math
open Engine.Renderer.Core

/// Based on Q3: setupEntityLightingGrid
/// SetupEntityLightingGrid
/// impure due to lightgrid data being a .net array
let setupEntityLightingGrid (rentity: TrRefEntity) (lightGrid: LightGrid) (r_ambientScale: Cvar) (r_directedScale: Cvar) =
    let entity = rentity.Entity

    let lightOrigin =
        match entity.RenderFx.HasFlag RenderFxFlags.LightingOrigin with
        // seperate lightOrigins are needed so an object that is
        // sinking into the ground can still be lit, and so
        // multi-part models can be lit identically
        | true -> entity.LightingOrigin
        | _ -> entity.Origin
        - lightGrid.Origin

    let inline f i = lightOrigin.[i] * lightGrid.InverseSize.[i]

    let v = vec3 (f 0, f 1, f 2)
    let pos = Vec3.floor v
    let frac = v - pos

    let pos = 
        LightGridBounds (
            clamp (int pos.X) 0 (lightGrid.Bounds.X - 1),
            clamp (int pos.Y) 0 (lightGrid.Bounds.Y - 1),
            clamp (int pos.Z) 0 (lightGrid.Bounds.Z - 1))

    // trilerp the light value
    let gridStepX = 8
    let gridStepY = 8 * lightGrid.Bounds.X
    let gridStepZ = 8 * lightGrid.Bounds.X * lightGrid.Bounds.Y
    let gridIndex = (pos.X * gridStepX) + (pos.Y * gridStepY) + (pos.Z * gridStepZ)

    let inline gridStep i =
        match i with
        | 0 -> gridStepX
        | 1 -> gridStepY
        | 2 -> gridStepZ
        | _ -> raise <| System.ArgumentOutOfRangeException ()

    let rec calculateFactor i factor dataIndex n =
        match n with
        | 3 -> factor, dataIndex
        | _ ->
            match (i &&& (1 <<< n)) <> 0 with
            | true ->
                calculateFactor i (factor * frac.[n]) (dataIndex + gridStep n) (n + 1)
            | _ ->
                calculateFactor i (factor * (1.f - frac.[n])) dataIndex (n + 1)
            
    let rec calculateTotalFactor totalFactor direction rentity n =
        match n with
        | 8 -> totalFactor, direction, rentity
        | _ ->
            let factor, dataIndex = calculateFactor n 1.f gridIndex 0

            match lightGrid.Data.[dataIndex] + lightGrid.Data.[dataIndex + 1] + lightGrid.Data.[dataIndex + 2] with
            // ignore samples in walls
            | 0uy ->
                calculateTotalFactor totalFactor direction rentity (n + 1)
            | _ ->

            let ambientLight =
                vec3 (
                    single <| lightGrid.Data.[dataIndex],
                    single <| lightGrid.Data.[dataIndex + 1],
                    single <| lightGrid.Data.[dataIndex + 2])

            let directedLight =
                vec3 (
                    single <| lightGrid.Data.[dataIndex + 3],
                    single <| lightGrid.Data.[dataIndex + 4],
                    single <| lightGrid.Data.[dataIndex + 5])

            let rentity = 
                { rentity with
                    AmbientLight = rentity.AmbientLight + ambientLight * factor
                    DirectedLight = rentity.DirectedLight + directedLight * factor }

            let long = single lightGrid.Data.[dataIndex + 6] * 4.f
            let lat = single lightGrid.Data.[dataIndex + 7] * 4.f

            // FIXME: this is aweful - fix this sutpid garbage
            let sin x = 
                x * 360.f / 1023.f
                |> (*) 1.f<deg>
                |> Deg.toRad
                |> (*) 1.f</rad>
                |> sin
            
            // FIXME: this is aweful - fix this sutpid garbage
            let cos x =
                (int x + 256) &&& 1023
                |> single
                |> sin

            let normal =
                vec3 (
                    cos lat * sin long,
                    sin lat * sin long,
                    cos long)
            
            let direction = Vec3.multiplyAdd factor normal direction

            calculateTotalFactor (totalFactor + factor) direction rentity (n + 1)

    let totalFactor, direction, rentity =
        calculateTotalFactor
            0.f
            Vec3.zero
            { rentity with AmbientLight = Vec3.zero; DirectedLight = Vec3.zero }
            0

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
        LightDirection = Vec3.normalize direction }


/// Based on Q3: LogLight
/// LogLight
/// note: internal
let logLight (rentity: TrRefEntity) =
    match rentity.Entity.RenderFx.HasFlag RenderFxFlags.FirstPerson with
    | false -> ()
    | _ ->

    let ambientLight = rentity.AmbientLight
    let directedLight = rentity.DirectedLight

    let max1 =
        if ambientLight.Y > ambientLight.X then
            ambientLight.Y
        elif ambientLight.Z > ambientLight.X then
            ambientLight.Z
        else
            ambientLight.X

    let max2 =
        if directedLight.Y > directedLight.X then
            directedLight.Y
        elif directedLight.Z > directedLight.X then
            directedLight.Z
        else
            directedLight.X

    // impurity
    printfn "amb:%i  dir:%i" (int max1) (int max2)


/// Based on Q3: R_SetupEntityLighting
/// SetupEntityLighting
///
/// Calculates all the lighting values that will be used
/// by the Calc_* functions
/// impure due to loglight and lightgrid data being a .net array
let setupEntityLighting (refdef: TrRefdef) (identityLight: single) (sunDirection: vec3) (rentity: TrRefEntity) (lightGrid: LightGrid option) (r_ambientScale: Cvar) (r_directedScale: Cvar) (r_debugLight: Cvar) =
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

    let rentity =
        // if NOWORLDMODEL, only use dynamic lights (menu system, etc)
        // FIXME: This is kinda of hacky using the light grid here.
        match refdef.RdFlags.HasFlag RdFlags.NoWorldModel with
        | false when lightGrid.IsSome && lightGrid.Value.Data.Length <> 0 ->
            setupEntityLightingGrid rentity lightGrid.Value r_ambientScale r_directedScale
        | _ ->
            { rentity with
                AmbientLight = vec3 (identityLight * 150.f)
                DirectedLight = vec3 (identityLight * 150.f)
                LightDirection = sunDirection }

    // bonus items and view weapons have a fixed minimum add
    let rentity =
        { rentity with
            // give everything a minimum light add
            AmbientLight = rentity.AmbientLight + ((identityLight * 32.f) * Vec3.one) }

    //
    // modify the light by dynamic lights
    //
    let lightDirection = 
        rentity.DirectedLight
        |> Vec3.magnitude 
        |> (*) rentity.LightDirection

    let directedLight, lightDirection =
        refdef.Dlights
        |> List.fold (fun (directedLight, lightDirection) x ->
            let direction = (x.Origin - lightOrigin)
            let normal = Vec3.normalize direction
            let power = Dlight.AtRadius * (x.Radius * x.Radius)

            let length = 
                direction
                |> Vec3.magnitude
                |> function
                | y when y < Dlight.MinRadius -> Dlight.MinRadius
                | y -> y
            
            let length = power / (length * length)

            Vec3.multiplyAdd length x.Color directedLight,
            Vec3.multiplyAdd length normal lightDirection) (rentity.DirectedLight, lightDirection)

    let identityLightByte = identityLight * 255.f

    let clampAmbient x =
        match x > identityLightByte with
        | true -> identityLightByte
        | _ -> x

    let rentity =
        { rentity with
            AmbientLight =
                vec3 (
                    clampAmbient rentity.AmbientLight.X,
                    clampAmbient rentity.AmbientLight.Y,
                    clampAmbient rentity.AmbientLight.Z) }

    // impure function call for logLight
    if r_debugLight.Integer <> 0 then
        logLight rentity

    // save out the byte packet version
    let rentity = TrRefEntity.calculateAmbientLightPacket rentity

    // transform the direction to local space
    let normal = Vec3.normalize lightDirection
    { rentity with
        LightDirection =
            vec3 (
                Vec3.dot normal entity.Axis.X,
                Vec3.dot normal entity.Axis.Y,
                Vec3.dot normal entity.Axis.Z)
        DirectedLight = directedLight }