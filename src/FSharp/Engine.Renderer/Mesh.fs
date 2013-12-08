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

/// Based on Q3: ProjectRadius
/// ProjectRadius
[<Pure>]
let projectRadius radius location (view: ViewParms) =
    let axis = view.Orientation.Axis
    let origin = view.Orientation.Origin

    let c = Vector3.dot axis.X origin
    let distance = Vector3.dot axis.X location - c

    match distance <= 0.f with
    | true -> 0.f
    | _ ->

    let p = Vector3 (0.f, abs radius, -distance)

    let inline f i = 
        p.X * view.ProjectionMatrix.[0, i] +
        p.Y * view.ProjectionMatrix.[1, i] +
        p.Z * view.ProjectionMatrix.[2, i] +
        view.ProjectionMatrix.[3, i]

    let pr = f 1 / f 3

    match pr > 1.f with
    | true -> 1.f
    | _ -> pr


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
            Bounds.Mins =
                Vector3.create
                    (calculateBounds 0 0)
                    (calculateBounds 0 1)
                    (calculateBounds 0 2);
            Maxs =
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
[<Pure>]
let computeLod (entity: RefEntity) (model: Model) (r_lodscale: Cvar) (r_lodbias: Cvar) (r: Renderer) =
    let lodCount = model.Md3Lods.Length
    match lodCount = 0 with
    // model has only 1 LOD level, skip computations and bias
    | true -> 0
    | _ ->

    // multiple LODs exist, so compute projected bounding sphere
    // and use that as a criteria for selecting LOD

    let frame = model.Md3.Frames.[entity.Frame]
    let radius = Bounds.radius frame.Bounds
    
    let projectedRadius = projectRadius radius entity.Origin r.ViewParms
    
    let lod =
        match projectedRadius <> 0.f with
        | true ->
            let lodscale = if r_lodscale.Value > 20.f then 20.f else r_lodscale.Value
            1.f - projectedRadius * lodscale
        // object intersects near view plane, e.g. view weapon
        | _ -> 0.f
        |> (*) (single <| lodCount + 1)
        |> int
        |> (+) r_lodbias.Integer

    // Clamp lod
    if lodCount < 0 then 0 elif lod > lodCount then lodCount else lod

/// Based on Q3: R_AddMD3Surfaces
/// AddMd3Surfaces
let addMd3Surfaces (entity: RefEntity) (r_lodscale: Cvar) (r_lodbias: Cvar) (r_nocull: Cvar) (r: Renderer) =
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
    let lod = computeLod entity model r_lodscale r_lodbias r
    let md3 = if lod > 0 then model.Md3Lods.[lod - 1] else model.Md3
    
    //
    // cull the entire model if merged bounding box of both frames
    // is outside the view frustum.
    //
    let cull,r' = cullModelByFrames md3.Frames.[frame] md3.Frames.[oldFrame] entity r_nocull r

    match cull = ClipType.Out with
    | true -> r'
    | _ ->

    // TODO:
    r'
