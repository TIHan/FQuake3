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
let calculateCullLocalBox (newFrame: Md3Frame) (oldFrame: Md3Frame) (noCull: Cvar) (tr: TrGlobals) =
    let inline calculateBounds i j =
        match oldFrame.Bounds.[i].[j] < newFrame.Bounds.[i].[j] with
        | true -> oldFrame.Bounds.[i].[j]
        | _ -> newFrame.Bounds.[i].[j]

    // calculate a bounding box in the current coordinate system
    let bounds =
        {
            Bounds.Bounds1 =
                {
                    X = calculateBounds 0 0;
                    Y = calculateBounds 0 1;
                    Z = calculateBounds 0 2;
                };
            Bounds2 =
                {
                    X = calculateBounds 1 0;
                    Y = calculateBounds 1 1;
                    Z = calculateBounds 1 2;
                }
        }

    let clip = Main.cullLocalBox bounds tr.Orientation tr.ViewParms.Frustum noCull
    let perfCounters = PerfCounter.incrementBoxMd3 clip tr.PerfCounters
    let tr = { tr with PerfCounters = perfCounters }

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
let cullModelByFrames (newFrame: Md3Frame) (oldFrame: Md3Frame) (entity: RefEntity) (noCull: Cvar) (tr: TrGlobals) =
    // cull bounding sphere ONLY if this is not an upscaled entity
    match not entity.HasNonNormalizedAxes with
    | true ->
        let sphereCull = Main.cullLocalPointAndRadius newFrame.LocalOrigin newFrame.Radius tr.Orientation tr.ViewParms.Frustum noCull

        match entity.Frame = entity.OldFrame with
        | true ->
            let perfCounters = PerfCounter.incrementSphereMd3 sphereCull tr.PerfCounters
            let tr = { tr with PerfCounters = perfCounters }

            match sphereCull with
            | ClipType.Out ->
                (ClipType.Out, tr)
            | ClipType.In ->
                (ClipType.In, tr)
            | _ ->
                calculateCullLocalBox newFrame oldFrame noCull tr
        | _ ->
            let sphereCullB =
                match newFrame = oldFrame with
                | true -> sphereCull
                | _ -> Main.cullLocalPointAndRadius oldFrame.LocalOrigin oldFrame.Radius tr.Orientation tr.ViewParms.Frustum noCull
                
            match sphereCull = sphereCullB with
            | true ->
                let perfCounters = PerfCounter.incrementSphereMd3 sphereCull tr.PerfCounters
                let tr = { tr with PerfCounters = perfCounters }

                match sphereCull with
                | ClipType.Out ->
                    (ClipType.Out, tr)
                | ClipType.In ->
                    (ClipType.In, tr)
                | _ ->
                    calculateCullLocalBox newFrame oldFrame noCull tr
            | _ ->
                calculateCullLocalBox newFrame oldFrame noCull tr
    | _ ->
        calculateCullLocalBox newFrame oldFrame noCull tr