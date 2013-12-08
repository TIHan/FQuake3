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

module Engine.Renderer.Mesh

open System
open System.Diagnostics.Contracts
open Engine.Core
open Engine.Files
open Engine.Math
open Engine.Renderer.Core

/// CalculateCullLocalBox
[<Pure>]
let calculateCullLocalBox (newFrame: Md3Frame) (oldFrame: Md3Frame) (r_nocull: Cvar) (r: Renderer) =
    let inline calculateBounds i j =
        match oldFrame.Bounds.[i].[j] < newFrame.Bounds.[i].[j] with
        | true -> oldFrame.Bounds.[i].[j]
        | _ -> newFrame.Bounds.[i].[j]

    // calculate a bounding box in the current coordinate system
    let bounds =
        {
            From =
                Vector3.create
                    (calculateBounds 0 0)
                    (calculateBounds 0 1)
                    (calculateBounds 0 2);
            To =
                Vector3.create
                    (calculateBounds 1 0)
                    (calculateBounds 1 1)
                    (calculateBounds 1 2)
        }

    let clip = Main.cullLocalBox bounds r.Orientation r.ViewParms.Frustum r_nocull
    let perfCounters = PerfCounter.incrementBoxMd3 clip r.PerfCounters
    let tr = { r with PerfCounters = perfCounters }

    match clip with
    | ClipType.In ->
        (ClipType.In, tr)
    | ClipType.Clip ->
        (ClipType.Clip, tr)
    | _ ->
        (ClipType.Out, tr)

/// Based on Q3: R_CullModel
/// CullModel
/// Note: This is internal.
[<Pure>]
let cullModelByFrames (newFrame: Md3Frame) (oldFrame: Md3Frame) (entity: RefEntity) (r_nocull: Cvar) (r: Renderer) =
    // cull bounding sphere ONLY if this is not an upscaled entity
    match not entity.HasNonNormalizedAxes with
    | true ->
        let sphereCull = Main.cullLocalPointAndRadius newFrame.LocalOrigin newFrame.Radius r.Orientation r.ViewParms.Frustum r_nocull

        match entity.Frame = entity.OldFrame with
        | true ->
            let perfCounters = PerfCounter.incrementSphereMd3 sphereCull r.PerfCounters
            let tr = { r with PerfCounters = perfCounters }

            match sphereCull with
            | ClipType.Out ->
                (ClipType.Out, tr)
            | ClipType.In ->
                (ClipType.In, tr)
            | _ ->
                calculateCullLocalBox newFrame oldFrame r_nocull tr
        | _ ->
            let sphereCullB =
                match newFrame = oldFrame with
                | true -> sphereCull
                | _ -> Main.cullLocalPointAndRadius oldFrame.LocalOrigin oldFrame.Radius r.Orientation r.ViewParms.Frustum r_nocull
                
            match sphereCull = sphereCullB with
            | true ->
                let perfCounters = PerfCounter.incrementSphereMd3 sphereCull r.PerfCounters
                let tr = { r with PerfCounters = perfCounters }

                match sphereCull with
                | ClipType.Out ->
                    (ClipType.Out, tr)
                | ClipType.In ->
                    (ClipType.In, tr)
                | _ ->
                    calculateCullLocalBox newFrame oldFrame r_nocull tr
            | _ ->
                calculateCullLocalBox newFrame oldFrame r_nocull r
    | _ ->
        calculateCullLocalBox newFrame oldFrame r_nocull r

/// Based on Q3: R_ComputeLOD
/// ComputeLod
let computeLod (entity: TrRefEntity) (model: Model) (r_lodbias: Cvar) =
    match model.Md3Lods.Length = 0 with
    | true -> 0
    | _ ->

    // TODO:
    0

/// Based on Q3: R_AddMD3Surfaces
/// AddMd3Surfaces
let addMd3Surfaces (entity: RefEntity) (r_nocull: Cvar) (r: Renderer) =
    // don't add third_person objects if not in a portal
    let isPersonalModel =
        entity.RenderFx.HasFlag RenderFxFlags.ThirdPerson
        && not r.ViewParms.IsPortal

    let canWrapFrames = entity.RenderFx.HasFlag RenderFxFlags.WrapFrames

    let model =
        match r.CurrentModel with
        | None -> raise <| Exception "Current model does not exist."
        | Some x -> x

    let frameCount = model.Md3.Header.FrameCount

    let frame =
        match canWrapFrames with
        | false -> entity.Frame
        | true -> entity.Frame % frameCount

    let oldFrame =
        match canWrapFrames with
        | false -> entity.OldFrame
        | true -> entity.OldFrame % frameCount

    //
    // Validate the frames so there is no chance of a crash.
    // This will write directly into the entity structure, so
    // when the surfaces are rendered, they don't need to be
    // range checked again.
    //
    let inline validateFrame frame' =
        match frame' >= frameCount || frame' < 0 with
        | true ->
            printfn "R_AddMd3Surfaces: no such frame %d to %d for '%s'" frame oldFrame model.Name
            0
        | _ -> frame'

    let frame = validateFrame frame
    let oldFrame = validateFrame oldFrame

    // TODO:
    ()
