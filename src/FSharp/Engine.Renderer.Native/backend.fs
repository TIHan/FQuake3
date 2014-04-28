(*
Copyright (C) 2013-2014 William F. Smith

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

namespace Engine.Renderer.Native

open Ferop.Code

[<Struct>]
type Test =
    val x : int

[<Ferop>]
[<Include ("<windows.h>")>]
[<Include ("<gl/gl.h>")>]
[<MsvcLibsWin ("opengl32.lib")>]
[<MsvcIncludesWin ("")>]
module Backend =
    let glDepthFunc (isEqual: bool) : unit =
        C """glDepthFunc (isEqual == GL_TRUE ? GL_EQUAL : GL_LEQUAL);"""

    let wut (x: Test) : unit = C """ """