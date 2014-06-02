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

module Engine.Renderer.Backend

// Disable native interop warnings
#nowarn "9"
#nowarn "51"

open System
open System.Security
open System.Diagnostics.Contracts
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Engine.Core
open FSharp.Game.Math
open Engine.Renderer.Core
open Engine.NativeInterop

open Ferop.TypeProvider
open Ferop

open Engine.Renderer.Native
type Native = FeropProvider<"Engine.Renderer.Native", "../../../build">

[<RequireQualifiedAccess>]
module GL =
    open GLS

    /// Based on Q3: GL_State
    /// state
    ///
    /// This routine is responsible for setting the most commonly changed state
    /// in Q3.
    let state (stateBits: uint32) (state: GLState) =
        let diff = stateBits ^^^ state.GLStateBits
        
        match diff with
        | 0u -> state
        | _ ->

        //
        // check depthFunc bits
        //
        if diff &&& uint32 GLS.DepthFuncEqual <> 0u then
            stateBits &&& uint32 GLS.DepthFuncEqual <> 0u
            |> Native.Backend.glDepthFunc

        //
        // check blend bits
        //
        if diff &&& (uint32 GLS.SrcBlend.Bits ||| uint32 GLS.DstBlend.Bits) <> 0u then
            if stateBits &&& (uint32 GLS.SrcBlend.Bits ||| uint32 GLS.DstBlend.Bits) <> 0u then
                let srcBlend = enum<GLS.SrcBlend> (int (stateBits &&& uint32 GLS.SrcBlend.Bits))
                let dstBlend = enum<GLS.DstBlend> (int (stateBits &&& uint32 GLS.DstBlend.Bits))

                Native.Backend.glEnableBlend (srcBlend, dstBlend)
            else
                Native.Backend.glDisableBlend ()

        //
        // check depthmask
        //
        if diff &&& uint32 GLS.DepthMaskTrue <> 0u then
            stateBits &&& uint32 GLS.DepthMaskTrue <> 0u
            |> Native.Backend.glDepthMask
        
        //
        // fill/line mode
        //
        if diff &&& uint32 GLS.PolyModeLine <> 0u then
            stateBits &&& uint32 GLS.PolyModeLine <> 0u
            |> Native.Backend.glPolygonMode

        //
        // depthtest
        //
        if diff &&& uint32 GLS.DepthTestDisable <> 0u then
            stateBits &&& uint32 GLS.DepthTestDisable <> 0u
            |> Native.Backend.glDepthTest

        //
        // alpha test
        //
        if diff &&& uint32 GLS.ATest.Bits <> 0u then
            let atest = enum<GLS.ATest> (int (stateBits &&& uint32 GLS.ATest.Bits))
            match atest with
            | GLS.ATest.None -> Native.Backend.glDisableAlphaTest ()
            | _ -> Native.Backend.glEnableAlphaTest atest
        
        { state with GLStateBits = stateBits }

/// Based on Q3: SetViewportAndScissor
/// SetViewportAndScissor
/// Internal
let setViewportAndScissor (backend: Backend) =
    let view = backend.View
    
    view.ProjectionMatrix
    |> fixed' (fun (ptr: nativeptr<single>) -> 
        Native.Backend.glSetViewportAndScissor (NativePtr.toNativeInt ptr, view.ViewportX, view.ViewportY, view.ViewportWidth, view.ViewportHeight))

/// Based on Q3: RB_Hyperspace
/// Hyperspace
///
/// A player has predicted a teleport, but hasn't arrived yet
/// Internal
let hyperspace (backend: Backend) =
    let color = single (backend.Refdef.Time &&& 255) / 255.f

    Native.Backend.glHyperspaceClear color
    { backend with IsHyperspace = true }

/// SyncGLState
let private syncGLState (r_finish: Cvar) (state: GLState) =
    match r_finish.Integer with
    | 1 when not state.HasFinishCalled ->
        Native.Backend.glFinish ()
        { state with HasFinishCalled = true }
    | 0 ->
        { state with HasFinishCalled = true }
    | _ ->
        state

/// Based on Q3: RB_BeginDrawingView
/// BeginDrawingView
///
/// Any mirrored or portaled views have already been drawn, so prepare
/// to actually render the visible surfaces for this view
let beginDrawingView (r_finish: Cvar) (r_measureOverdraw: Cvar) (r_shadows: Cvar) (r_fastsky: Cvar) (glState: GLState) (backend: Backend) =
    // sync with gl if needed
    let glState = syncGLState r_finish glState

    // we will need to change the projection matrix before drawing
    // 2D images again
    let backend = { backend with IsProjection2D = false }

    // set the modelview matrix for the viewer
    setViewportAndScissor backend |> ignore

    // ensures that depth writes are enabled for the depth clear
    let glState = GL.state (uint32 GLS.Default) glState

    let useStencilBuf = r_measureOverdraw.Integer <> 0 || r_shadows.Integer = 2
    let useColorBuf = r_fastsky.Integer <> 0 && not (backend.Refdef.RdFlags.HasFlag RdFlags.NoWorldModel)
    let clearBits = Native.Backend.glGetClearBits (useStencilBuf, useColorBuf)

    if useColorBuf then
#if DEBUG
        Native.Backend.glClearWithColor (clearBits, 0.8f, 0.7f, 0.4f) // FIXME: get color of sky
#else
        Native.Backend.glClearWithColor (clearBits, 0.f, 0.f, 0.f) // FIXME: get color of sky
#endif
    else
        Native.Backend.glClear clearBits

    match backend.Refdef.RdFlags.HasFlag RdFlags.Hyperspace with
    | true -> glState, hyperspace backend
    | _ ->

    if backend.View.IsPortal then
        let axis = backend.View.Orientation.Axis
        let origin = backend.View.Orientation.Origin
        let normal = backend.View.PortalPlane.Normal
        let distance = backend.View.PortalPlane.Distance
        let plane =
            Plane (
                double <| Vec3.dot axis.[0] normal,
                double <| Vec3.dot axis.[1] normal,
                double <| Vec3.dot axis.[2] normal,
                double <| Vec3.dot normal origin - distance
            )

        fixed2' (fun (ptr: nativeptr<single>) (ptr2: nativeptr<single>) ->
            Native.Backend.glEnableClipPlane (NativePtr.toNativeInt ptr, NativePtr.toNativeInt ptr2)
        ) Main.flipMatrix plane
    else
        Native.Backend.glDisableClipPlane ()

    // force face culling to set next time
    { glState with FaceCulling = -1 },
    // we will only draw a sun if there was sky rendered in this view
    { backend with
        HasSkyRenderedThisView = false
        IsHyperspace = false
    }

/// Based on Q3: RB_BeginSurface
/// RB_BeginSurface
///
/// We must set some things up before beginning any tesselation,
/// because a surface may be forced to perform a RB_End due
/// to overflow.
/// TODO: Finish.
let beginSurface (shader: Shader) (fogId: int) =
    ()

/// Based on Q3: RB_RenderDrawSurfList
/// RenderDrawSurfaces
/// TODO: Finish.
let renderDrawSurfaces (r_finish: Cvar) (r_measureOverdraw: Cvar) (r_shadows: Cvar) (r_fastsky: Cvar) (glState: GLState) (backend: Backend) (rentity: TrRefEntity) (drawSurfaces: DrawSurface list) =
    // save original time for entity shader offsets
    let originalTime = backend.Refdef.FloatTime

    // clear the z buffer, set the modelview, etc
    let glState, backend = beginDrawingView r_finish r_measureOverdraw r_shadows r_fastsky glState backend

    let backend = { backend with CurrentEntity = Some rentity }


    ()