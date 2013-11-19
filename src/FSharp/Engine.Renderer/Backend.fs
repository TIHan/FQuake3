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
open Engine.Math
open Engine.Renderer.Core
open Engine.NativeInterop

[<RequireQualifiedAccess>]
module GL =
    [<RequireQualifiedAccess>]
    module GLS =
        type SrcBlend =
            | None =                0x00000000
            | Zero =                0x00000001
            | One =                 0x00000002
            | DstColor =            0x00000003
            | OneMinusDstColor =    0x00000004
            | SrcAlpha =            0x00000005
            | OneMinusSrcAlpha =    0x00000006
            | DstAlpha =            0x00000007
            | OneMinusDstAlpha =    0x00000008
            | AlphaSaturate =       0x00000009
            | Bits =                0x0000000f

        type DstBlend =
            | None =                0x00000000
            | Zero =                0x00000010
            | One =                 0x00000020
            | SrcColor =            0x00000030
            | OneMinusSrcColor =    0x00000040
            | SrcAlpha =            0x00000050
            | OneMinusSrcAlpha =    0x00000060
            | DstAlpha =            0x00000070
            | OneMinusDstAlpha =    0x00000080
            | Bits =                0x000000f0

        [<Literal>]
        let DepthMaskTrue =         0x00000100

        [<Literal>]
        let PolyModeLine =          0x00001000

        [<Literal>]
        let DepthTestDisable =      0x00010000

        [<Literal>]
        let DepthFuncEqual =        0x00020000

        type ATest =
            | None =                0x00000000
            | Gt0 =                 0x10000000
            | Lt80 =                0x20000000
            | Ge80 =                0x40000000
            | Bits =                0x70000000

        [<Literal>]
        let Default = DepthMaskTrue

    /// Based on Q3: GL_State
    /// state
    ///
    /// This routine is responsible for setting the most commonly changed state
    /// in Q3.
    let state (stateBits: uint64) (state: GLState) =
        let diff = stateBits ^^^ state.GLStateBits
        
        match diff with
        | 0UL -> state
        | _ ->

        //
        // check depthFunc bits
        //
        if diff &&& uint64 GLS.DepthFuncEqual <> 0UL then
            stateBits &&& uint64 GLS.DepthFuncEqual <> 0UL
            |> Internal.er_gl_depth_func

        //
        // check blend bits
        //
        if diff &&& (uint64 GLS.SrcBlend.Bits ||| uint64 GLS.DstBlend.Bits) <> 0UL then
            if stateBits &&& (uint64 GLS.SrcBlend.Bits ||| uint64 GLS.DstBlend.Bits) <> 0UL then
                let srcBlend = enum<GLS.SrcBlend> (int (stateBits &&& uint64 GLS.SrcBlend.Bits))
                let dstBlend = enum<GLS.DstBlend> (int (stateBits &&& uint64 GLS.DstBlend.Bits))

                let srcBits =
                    match srcBlend with
                    | GLS.SrcBlend.Zero
                    | GLS.SrcBlend.One
                    | GLS.SrcBlend.DstColor
                    | GLS.SrcBlend.OneMinusDstColor
                    | GLS.SrcBlend.SrcAlpha
                    | GLS.SrcBlend.OneMinusSrcAlpha
                    | GLS.SrcBlend.DstAlpha
                    | GLS.SrcBlend.OneMinusDstAlpha
                    | GLS.SrcBlend.AlphaSaturate -> uint32 srcBlend
                    | _ -> raise <| Exception "Invalid src blend state bits."
                
                let dstBits =
                    match dstBlend with
                    | GLS.DstBlend.Zero
                    | GLS.DstBlend.One
                    | GLS.DstBlend.SrcColor
                    | GLS.DstBlend.OneMinusSrcColor
                    | GLS.DstBlend.SrcAlpha
                    | GLS.DstBlend.OneMinusSrcAlpha
                    | GLS.DstBlend.DstAlpha
                    | GLS.DstBlend.OneMinusDstAlpha -> uint32 dstBlend
                    | _ -> raise <| Exception "Invalid dst blend state bits."

                Internal.er_gl_enable_blend (srcBits, dstBits)
            else
                Internal.er_gl_disable_blend ()

        //
        // check depthmask
        //
        if diff &&& uint64 GLS.DepthMaskTrue <> 0UL then
            stateBits &&& uint64 GLS.DepthMaskTrue <> 0UL
            |> Internal.er_gl_depth_mask
        
        //
        // fill/line mode
        //
        if diff &&& uint64 GLS.PolyModeLine <> 0UL then
            stateBits &&& uint64 GLS.PolyModeLine <> 0UL
            |> Internal.er_gl_polygon_mode

        //
        // depthtest
        //
        if diff &&& uint64 GLS.DepthTestDisable <> 0UL then
            stateBits &&& uint64 GLS.DepthTestDisable <> 0UL
            |> Internal.er_gl_polygon_mode

        //
        // alpha test
        //
        if diff &&& uint64 GLS.ATest.Bits <> 0UL then
            let atest = enum<GLS.ATest> (int (stateBits &&& uint64 GLS.ATest.Bits))
            match atest with
            | GLS.ATest.None -> Internal.er_gl_disable_alpha_test ()
            | GLS.ATest.Gt0
            | GLS.ATest.Lt80
            | GLS.ATest.Ge80 -> Internal.er_gl_enable_alpha_test <| uint32 atest
            | _ ->
                raise <| Exception "Invalid alpha test state bits."
        
        { state with GLStateBits = stateBits }

/// Based on Q3: SetViewportAndScissor
/// SetViewportAndScissor
/// Internal
let setViewportAndScissor (backend: Backend) =
    let view = backend.View
    fixed' (fun ptr -> 
        Internal.er_gl_set_viewport_and_scissor (ptr, view.ViewportX, view.ViewportY, view.ViewportWidth, view.ViewportHeight)
    ) view.ProjectionMatrix

/// Based on Q3: RB_Hyperspace
/// Hyperspace
///
/// A player has predicted a teleport, but hasn't arrived yet
/// Internal
let hyperspace (backend: Backend) =
    let color = single (backend.Refdef.Time &&& 255) / 255.f

    Internal.er_gl_hyperspace_clear color
    { backend with IsHyperspace = true }

/// SyncGLState
let private syncGLState (r_finish: Cvar) (state: GLState) =
    match r_finish.Integer with
    | 1 when not state.HasFinishCalled ->
        Internal.er_gl_finish ()
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
    setViewportAndScissor backend

    // ensures that depth writes are enabled for the depth clear
    let glState = GL.state (uint64 GL.GLS.Default) glState

    let useStencilBuf = r_measureOverdraw.Integer <> 0 || r_shadows.Integer = 2
    let useColorBuf = r_fastsky.Integer <> 0 && not (backend.Refdef.RdFlags.HasFlag RdFlags.NoWorldModel)
    let clearBits = Internal.er_gl_get_clear_bits (useStencilBuf, useColorBuf)

    if useColorBuf then
#if DEBUG
        Internal.er_gl_clear_with_color (clearBits, 0.8f, 0.7f, 0.4f) // FIXME: get color of sky
#else
        Internal.er_gl_clear_with_color (clearBits, 0.f, 0.f, 0.f) // FIXME: get color of sky
#endif
    else
        Internal.er_gl_clear clearBits

    match backend.Refdef.RdFlags.HasFlag RdFlags.Hyperspace with
    | true -> glState, hyperspace backend
    | _ ->

    if backend.View.IsPortal then
        () // TODO:
    else
        () // TODO:

    // force face culling to set next time
    { glState with FaceCulling = -1 },
    // we will only draw a sun if there was sky rendered in this view
    { backend with
        HasSkyRenderedThisView = false
        IsHyperspace = false
    }
