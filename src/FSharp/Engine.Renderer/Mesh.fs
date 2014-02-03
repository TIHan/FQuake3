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
open Engine.Renderer.Shader

/// Based on Q3: ProjectRadius
/// ProjectRadius
[<Pure>]
let projectRadius radius location (view: ViewParms) =
    let axis = view.Orientation.Axis
    let origin = view.Orientation.Origin

    let c = Vec3.dot axis.x origin
    let distance = Vec3.dot axis.x location - c

    match distance <= 0.f with
    | true -> 0.f
    | _ ->

    let p = vec3 (0.f, abs radius, -distance)

    let inline f i = 
        p.x * view.ProjectionMatrix.[0, i] +
        p.y * view.ProjectionMatrix.[1, i] +
        p.z * view.ProjectionMatrix.[2, i] +
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
                vec3 (
                    (calculateBounds 0 0),
                    (calculateBounds 0 1),
                    (calculateBounds 0 2)
                 );
            Maxs =
                vec3 (
                    (calculateBounds 1 0),
                    (calculateBounds 1 1),
                    (calculateBounds 1 2)
                )
        }

    let clip = Main.cullLocalBox bounds r.Orientation r.ViewParms.Frustum r_nocull
    let perfCounters = PerfCounter.incrementBoxMd3 clip r.PerfCounters
    let r' = { r with PerfCounters = perfCounters }

    match clip with
    | ClipType.In ->
        (ClipType.In, r')
    | ClipType.Clip ->
        (ClipType.Clip, r')
    | _ ->
        (ClipType.Out, r')

/// Based on Q3: R_CullModel
/// CullModel
/// Note: This is internal.
[<Pure>]
let cullModel (md3: Md3) (entity: RefEntity) (r: Renderer) (r_nocull: Cvar) =
    let newFrame = md3.Frames.[entity.Frame]
    let oldFrame = md3.Frames.[entity.OldFrame]
    // cull bounding sphere ONLY if this is not an upscaled entity
    match not entity.HasNonNormalizedAxes with
    | true ->
        let sphereCull = Main.cullLocalPointAndRadius newFrame.LocalOrigin newFrame.Radius r.Orientation r.ViewParms.Frustum r_nocull

        match entity.Frame = entity.OldFrame with
        | true ->
            let perfCounters = PerfCounter.incrementSphereMd3 sphereCull r.PerfCounters
            let r' = { r with PerfCounters = perfCounters }

            match sphereCull with
            | ClipType.Out ->
                (ClipType.Out, r')
            | ClipType.In ->
                (ClipType.In, r')
            | _ ->
                calculateCullLocalBox newFrame oldFrame r_nocull r'
        | _ ->
            let sphereCullB =
                match newFrame = oldFrame with
                | true -> sphereCull
                | _ -> Main.cullLocalPointAndRadius oldFrame.LocalOrigin oldFrame.Radius r.Orientation r.ViewParms.Frustum r_nocull
                
            match sphereCull = sphereCullB with
            | true ->
                let perfCounters = PerfCounter.incrementSphereMd3 sphereCull r.PerfCounters
                let r' = { r with PerfCounters = perfCounters }

                match sphereCull with
                | ClipType.Out ->
                    (ClipType.Out, r')
                | ClipType.In ->
                    (ClipType.In, r')
                | _ ->
                    calculateCullLocalBox newFrame oldFrame r_nocull r'
            | _ ->
                calculateCullLocalBox newFrame oldFrame r_nocull r
    | _ ->
        calculateCullLocalBox newFrame oldFrame r_nocull r

/// Based on Q3: R_ComputeLOD
/// ComputeLod
[<Pure>]
let computeLod (entity: RefEntity) (model: Model) (r_lodscale: Cvar) (r_lodbias: Cvar) (r: Renderer) =
    let lodCount = model.Md3Lods.Length

    let inline clampLod lod = if lod < 0 then 0 elif lod > lodCount then lodCount else lod

    match lodCount = 0 with
    // model has only 1 LOD level, skip computations and bias
    | true -> 0
    | _ ->

    // multiple LODs exist, so compute projected bounding sphere
    // and use that as a criteria for selecting LOD

    let frame = model.Md3.Frames.[entity.Frame]
    let radius = Bounds.radius frame.Bounds

    let projectedRadius = projectRadius radius entity.Origin r.ViewParms
    match projectedRadius <> 0.f with
    | true ->
        let lodscale = if r_lodscale.Value > 20.f then 20.f else r_lodscale.Value
        1.f - projectedRadius * lodscale
    // object intersects near view plane, e.g. view weapon
    | _ -> 0.f
    |> (*) (single <| lodCount + 1)
    |> int
    |> clampLod
    |> (+) r_lodbias.Integer
    |> clampLod

/// Based on Q3: R_ComputeFogNum
/// FogId
[<Pure>]
let fogId (md3: Md3) (entity: RefEntity) (r: Renderer) =
    match r.Refdef.RdFlags.HasFlag RdFlags.NoWorldModel with
    | true -> 0
    | _ ->

    // FIXME: non-normalized axis issues
    let frame = md3.Frames.[entity.Frame]
    let localOrigin = entity.Origin + frame.LocalOrigin

    match r.World with
    | None -> failwith "Renderer does not have a world."
    | Some world ->

    let v1 = localOrigin - frame.Radius
    let v2 = localOrigin + frame.Radius

    let fog =
        world.Fogs
        |> List.tryFindIndex (fun x ->
            v1 < x.Bounds.Maxs && v2 > x.Bounds.Mins)

    match fog with
    | None -> 0
    | Some x -> x

/// FindShaderIdByMd3surface
let findShaderIdByMd3Surface (entity: RefEntity) (surface: Md3Surface) (r: Renderer) =
    match entity.CustomShaderHandle <> 0 with
    | true -> entity.CustomShaderHandle
    |_ ->

    r.DefaultShaderId

/// Based on Q3: R_AddMD3Surfaces
/// AddMd3Surfaces
let addMd3Surfaces
    (rentity: TrRefEntity)
    (lightGrid: LightGrid option)
    (r: Renderer)
    (r_lodscale: Cvar)
    (r_lodbias: Cvar)
    (r_nocull: Cvar)
    (r_shadows: Cvar)
    (r_ambientScale: Cvar)
    (r_directedScale: Cvar) 
    (r_debugLight: Cvar) =
    let entity = rentity.Entity

    // don't add third_person objects if not in a portal
    let isPersonalModel =
        entity.RenderFx.HasFlag RenderFxFlags.ThirdPerson
        && not r.ViewParms.IsPortal

    let canWrapFrames = entity.RenderFx.HasFlag RenderFxFlags.WrapFrames

    let model =
        match r.CurrentModel with
        | None -> failwith "Current model does not exist."
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

    //
    // compute LOD
    //
    let lod = computeLod entity model r_lodscale r_lodbias r
    let md3 = if lod > 0 then model.Md3Lods.[lod - 1] else model.Md3
    
    //
    // cull the entire model if merged bounding box of both frames
    // is outside the view frustum.
    //
    let cull, r' = cullModel md3 entity r r_nocull

    match cull = ClipType.Out with
    | true -> r'
    | _ ->

    //
    // set up lighting now that we know we aren't culled
    //
    let rentity =
        if isPersonalModel || (r_shadows.Integer > 1) then
            Light.setupEntityLighting r.Refdef r.IdentityLight r.SunDirection rentity lightGrid r_ambientScale r_directedScale r_debugLight
        else
            rentity

    //
    // see if we are in a fog volume
    //
    let fogId = fogId md3 entity r

    let drawSurfaces =
        md3.Surfaces
        |> List.map (fun x ->
            {
            DrawSurface.Surface = Surface.Md3 x
            DrawSurface.ShaderId = findShaderIdByMd3Surface entity x r
            EntityId = r.CurrentEntityId
            FogId = fogId
            DynamicLightMap = 0 })

    { r with Refdef = { r.Refdef with DrawSurfaces = r.Refdef.DrawSurfaces @ drawSurfaces } }
