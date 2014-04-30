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

namespace Engine.Renderer.Native.FeropBackend

open Engine.Renderer.Native
open Ferop.Code

open GLS

[<Ferop>]
[<Include ("<windows.h>")>]
[<Include ("<gl/gl.h>")>]
[<MsvcLibsWin ("opengl32.lib")>]
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

    let glDepthTest (willDisable: bool) : unit =
        C """
if (willDisable == GL_TRUE)
{
	glDisable (GL_DEPTH_TEST);
}
else
{
	glEnable (GL_DEPTH_TEST);
}
"""

    let glEnableAlphaTest (alphaTest: ATest) : unit =
        C """
glEnable (GL_ALPHA_TEST);

switch (alphaTest)
{
case Backend_ATest_Gt0: glAlphaFunc (GL_GREATER, 0.0f); break;
case Backend_ATest_Lt80: glAlphaFunc (GL_LESS, 0.5f); break;
case Backend_ATest_Ge80: glAlphaFunc (GL_GEQUAL, 0.5f); break;
default: return; // shouldn't happen
}
"""

    let glDisableAlphaTest () : unit = C """glDisable (GL_ALPHA_TEST);"""

    let glHyperspaceClear (color: single) : unit =
        C """
glClearColor (color, color, color, 1.0f);
glClear (GL_COLOR_BUFFER_BIT);
"""

    let glFinish () : unit = C """glFinish ();"""

    let glGetClearBits (useStencilBuffer: bool) (useColorBuffer: bool) : uint32 =
        C """
// clear relevant buffers
GLbitfield clear_bits = GL_DEPTH_BUFFER_BIT;

clear_bits = useStencilBuffer ? clear_bits | GL_STENCIL_BUFFER_BIT : clear_bits;
clear_bits = useColorBuffer ? clear_bits | GL_COLOR_BUFFER_BIT : clear_bits;

return clear_bits;
"""

    let glClearWithColor (clearBits: uint32) (r: single) (g: single) (b: single) : unit =
        C """
glClearColor (r, g, b, 1.0f);
glClear (clearBits);
"""

    let glClear (clearBits: uint32) : unit = C """glClear (clearBits);"""

    let glEnableClipPlane (flipMatrix: nativeint) (plane: nativeint) : unit =
        C """
glLoadMatrixf ((GLfloat*)flipMatrix);
glClipPlane (GL_CLIP_PLANE0, (GLdouble*)plane);
glEnable (GL_CLIP_PLANE0);
"""

    let glDisableClipPlane () : unit = C """glDisable (GL_CLIP_PLANE0);"""

    let glSetViewportAndScissor (projectionMatrix: nativeint) (viewportX: int) (viewportY: int) (viewportWidth: int) (viewportHeight: int) : unit =
        C """
glMatrixMode (GL_PROJECTION);
glLoadMatrixf (projectionMatrix);
glMatrixMode (GL_MODELVIEW);

// set the window clipping
glViewport (viewportX, viewportY, viewportWidth, viewportHeight);
glScissor (viewportX, viewportY, viewportWidth, viewportHeight);
"""