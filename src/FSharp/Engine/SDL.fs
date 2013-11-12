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

module SDL

// Disable native interop warnings
#nowarn "9"
#nowarn "51"

open System
open System.Security
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

[<Literal>]
let LibOpenSDL = "SDL2.dll"

[<Literal>]
let OpenSDLCallingConvention = CallingConvention.Cdecl

[<Literal>]
let SDL_INIT_TIMER = 0x00000001

[<Literal>]
let SDL_INIT_AUDIO = 0x00000010

[<Literal>]
let SDL_INIT_VIDEO = 0x00000020

[<Literal>]
let SDL_INIT_JOYSTICK = 0x00000200

[<Literal>]
let SDL_INIT_HAPTIC = 0x00001000

[<Literal>]
let SDL_INIT_GAMECONTROLLER = 0x00002000

[<Literal>]
let SDL_INIT_EVENTS = 0x00004000

[<Literal>]
let SDL_INIT_NOPARACHUTE = 0x00100000

[<Literal>]
let SDL_INIT_EVERYTHING =
    SDL_INIT_TIMER ||| SDL_INIT_AUDIO ||| SDL_INIT_VIDEO ||| SDL_INIT_JOYSTICK |||
    SDL_INIT_HAPTIC ||| SDL_INIT_GAMECONTROLLER ||| SDL_INIT_EVENTS ||| SDL_INIT_NOPARACHUTE

//

type SDL_GLattr =
    | SDL_GL_RED_SIZE                   = 0
    | SDL_GL_GREEN_SIZE                 = 1
    | SDL_GL_BLUE_SIZE                  = 2
    | SDL_GL_ALPHA_SIZE                 = 3
    | SDL_GL_BUFFER_SIZE                = 4
    | SDL_GL_DOUBLEBUFFER               = 5
    | SDL_GL_DEPTH_SIZE                 = 6
    | SDL_GL_STENCIL_SIZE               = 7
    | SDL_GL_ACCUM_RED_SIZE             = 8
    | SDL_GL_ACCUM_GREEN_SIZE           = 9
    | SDL_GL_ACCUM_BLUE_SIZE            = 10
    | SDL_GL_ACCUM_ALPHA_SIZE           = 11
    | SDL_GL_STEREO                     = 12
    | SDL_GL_MULTISAMPLEBUFFERS         = 13
    | SDL_GL_MULTISAMPLESAMPLES         = 14
    | SDL_GL_ACCELERATED_VISUAL         = 15
    | SDL_GL_RETAINED_BACKING           = 16
    | SDL_GL_CONTEXT_MAJOR_VERSION      = 17
    | SDL_GL_CONTEXT_MINOR_VERSION      = 18
    | SDL_GL_CONTEXT_EGL                = 19
    | SDL_GL_CONTEXT_FLAGS              = 20
    | SDL_GL_CONTEXT_PROFILE_MASK       = 21
    | SDL_GL_SHARE_WITH_CURRENT_CONTEXT = 22
    | SDL_GL_FRAMEBUFFER_SRGB_CAPABLE   = 23

[<Flags>]
type SDL_WindowFlags =
    | SDL_WINDOW_FULLSCREEN = 0x00000001
    | SDL_WINDOW_OPENGL = 0x00000002
    | SDL_WINDOW_SHOWN = 0x00000004
    | SDL_WINDOW_HIDDEN = 0x00000008
    | SDL_WINDOW_BORDERLESS = 0x00000010
    | SDL_WINDOW_RESIZABLE = 0x00000020
    | SDL_WINDOW_MINIMIZED = 0x00000040
    | SDL_WINDOW_MAXIMIZED = 0x00000080
    | SDL_WINDOW_INPUT_GRABBED = 0x00000100
    | SDL_WINDOW_INPUT_FOCUS = 0x00000200
    | SDL_WINDOW_MOUSE_FOCUS = 0x00000400
    | SDL_WINDOW_FULLSCREEN_DESKTOP = 0x00001001 // Is this right?
    | SDL_WINDOW_FOREIGN = 0x00000800
    | SDL_WINDOW_ALLOW_HIGHDPI = 0x00002000

type SDL_Window = nativeint

[<SuppressUnmanagedCodeSecurity>]
[<DllImport (LibOpenSDL, CallingConvention = OpenSDLCallingConvention)>]
extern int SDL_Init (uint32 flags)

[<SuppressUnmanagedCodeSecurity>]
[<DllImport (LibOpenSDL, CallingConvention = OpenSDLCallingConvention)>]
extern int SDL_GL_SetAttribute (SDL_GLattr attr, int value)

[<SuppressUnmanagedCodeSecurity>]
[<DllImport (LibOpenSDL, CallingConvention = OpenSDLCallingConvention)>]
extern SDL_Window SDL_CreateWindow (string title, int x, int y, int w, int h, uint32 flags)