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

namespace Engine.Renderer.Native.Backend

open Ferop.Code

[<Struct>]
type Plane =
    val X : double
    val Y : double
    val Z : double
    val W : double

    new (x, y, z, w) = { X = x; Y = y; Z = z; W = w }

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

open GLS

[<Ferop>]
[<Include ("<windows.h>")>]
[<Include ("<gl/gl.h>")>]
[<MsvcLibsWin ("opengl32.lib")>]
[<MsvcIncludesWin ("")>]
module Backend =
    let glDepthFunc (isEqual: bool) : unit =
        C """glDepthFunc (isEqual == GL_TRUE ? GL_EQUAL : GL_LEQUAL);"""

    let glEnableBlend (src_bits: SrcBlend) (dst_bits: DstBlend) : unit =
        C """
GLenum src_factor;
GLenum dst_factor;

switch (src_bits)
{
case Backend_SrcBlend_Zero: src_factor =				GL_ZERO; break;
case Backend_SrcBlend_One: src_factor =					GL_ONE; break;
case Backend_SrcBlend_DstColor: src_factor =			GL_DST_COLOR; break;
case Backend_SrcBlend_OneMinusDstColor: src_factor = GL_ONE_MINUS_DST_COLOR; break;
case Backend_SrcBlend_SrcAlpha: src_factor =			GL_SRC_ALPHA; break;
case Backend_SrcBlend_OneMinusSrcAlpha: src_factor = GL_ONE_MINUS_SRC_ALPHA; break;
case Backend_SrcBlend_DstAlpha: src_factor =			GL_DST_ALPHA; break;
case Backend_SrcBlend_OneMinusDstAlpha: src_factor = GL_ONE_MINUS_DST_ALPHA; break;
case Backend_SrcBlend_AlphaSaturate: src_factor =		GL_SRC_ALPHA_SATURATE; break;
default: return; // shouldn't happen
}

switch (dst_bits)
{
case Backend_DstBlend_Zero: dst_factor =				GL_ZERO; break;
case Backend_DstBlend_One: dst_factor =					GL_ONE; break;
case Backend_DstBlend_SrcColor: dst_factor =			GL_SRC_COLOR; break;
case Backend_DstBlend_OneMinusSrcColor: dst_factor = GL_ONE_MINUS_SRC_COLOR; break;
case Backend_DstBlend_SrcAlpha: dst_factor =			GL_SRC_ALPHA; break;
case Backend_DstBlend_OneMinusSrcAlpha: dst_factor = GL_ONE_MINUS_SRC_ALPHA; break;
case Backend_DstBlend_DstAlpha: dst_factor =			GL_DST_ALPHA; break;
case Backend_DstBlend_OneMinusDstAlpha: dst_factor = GL_ONE_MINUS_DST_ALPHA; break;
default: return; // shouldn't happen
}

glEnable (GL_BLEND);
glBlendFunc (src_factor, dst_factor);
"""

    let glDisableBlend () : unit = C """glDisable (GL_BLEND);"""

    let glDepthMask (isTrue: bool) : unit = C """glDepthMask (isTrue);"""

    let glPolygonMode (isLine: bool) : unit = 
        C """glPolygonMode (GL_FRONT_AND_BACK, isLine == GL_TRUE ? GL_LINE : GL_FILL);"""