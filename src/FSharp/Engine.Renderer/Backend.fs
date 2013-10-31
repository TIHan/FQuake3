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
open System.Diagnostics.Contracts
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Engine.Core
open Engine.Math
open Engine.Renderer.Core
open GL

// Hmm, I wonder if this is ok...
let inline fixedPtr (f: nativeptr<_> -> unit) (a: obj) =
    let handle = GCHandle.Alloc (a, GCHandleType.Pinned)
    let addr = handle.AddrOfPinnedObject ()

    f <| NativePtr.ofNativeInt addr

    handle.Free ()

/// Based on Q3: SetViewportAndScissor
/// SetViewportAndScissor
let setViewportAndScissor (backend: Backend) =
    let view = backend.View

    glMatrixMode <| GLenum GL_PROJECTION
    fixedPtr (fun ptr -> glLoadMatrixf ptr) view.ProjectionMatrix
    glMatrixMode <| GLenum GL_MODELVIEW

    // set the window clipping
    glViewport (view.ViewportX, view.ViewportY, view.ViewportWidth, view.ViewportHeight)
    glScissor (view.ViewportX, view.ViewportY, view.ViewportWidth, view.ViewportHeight)