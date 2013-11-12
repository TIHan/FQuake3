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

module Engine.Renderer.Window

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
open SDL

let create width height colorbits isFullscreen =
    SDL_GL_SetAttribute (SDL_GLattr.SDL_GL_CONTEXT_MAJOR_VERSION, 1) |> ignore
    SDL_GL_SetAttribute (SDL_GLattr.SDL_GL_CONTEXT_MINOR_VERSION, 0) |> ignore
 
    let flags = SDL_WindowFlags.SDL_WINDOW_OPENGL ||| SDL_WindowFlags.SDL_WINDOW_SHOWN
    let handle = SDL_CreateWindow ("FQuake3", 0, 0, width, height, uint32 flags)
    handle