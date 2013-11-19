/*
Copyright(C) 2013 William F.Smith

This program is free software; you can redistribute it
and / or modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the License,
or(at your option) any later version.

This program is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 - 1307 USA

Derivative of Quake III Arena source :
Copyright(C) 1999 - 2005 Id Software, Inc.
*/

#include "backend.h"

ER_EXPORT void ER_DECL
er_gl_depth_func (GLboolean is_equal)
{
	glDepthFunc (is_equal == GL_TRUE ? GL_EQUAL : GL_LEQUAL);
}

ER_EXPORT void ER_DECL
er_gl_enable_blend (GLbitfield src_bits, GLbitfield dst_bits)
{
	GLenum src_factor;
	GLenum dst_factor;

	switch (src_bits)
	{
	case GLS_SRCBLEND_ZERO: src_factor =				GL_ZERO; break;
	case GLS_SRCBLEND_ONE: src_factor =					GL_ONE; break;
	case GLS_SRCBLEND_DST_COLOR: src_factor =			GL_DST_COLOR; break;
	case GLS_SRCBLEND_ONE_MINUS_DST_COLOR: src_factor = GL_ONE_MINUS_DST_COLOR; break;
	case GLS_SRCBLEND_SRC_ALPHA: src_factor =			GL_SRC_ALPHA; break;
	case GLS_SRCBLEND_ONE_MINUS_SRC_ALPHA: src_factor = GL_ONE_MINUS_SRC_ALPHA; break;
	case GLS_SRCBLEND_DST_ALPHA: src_factor =			GL_DST_ALPHA; break;
	case GLS_SRCBLEND_ONE_MINUS_DST_ALPHA: src_factor = GL_ONE_MINUS_DST_ALPHA; break;
	case GLS_SRCBLEND_ALPHA_SATURATE: src_factor =		GL_SRC_ALPHA_SATURATE; break;
	default:
		src_factor = GL_ZERO;
	}

	switch (dst_bits)
	{
	case GLS_DSTBLEND_ZERO: dst_factor =				GL_ZERO; break;
	case GLS_DSTBLEND_ONE: dst_factor =					GL_ONE; break;
	case GLS_DSTBLEND_SRC_COLOR: dst_factor =			GL_SRC_COLOR; break;
	case GLS_DSTBLEND_ONE_MINUS_SRC_COLOR: dst_factor = GL_ONE_MINUS_SRC_COLOR; break;
	case GLS_DSTBLEND_SRC_ALPHA: dst_factor =			GL_SRC_ALPHA; break;
	case GLS_DSTBLEND_ONE_MINUS_SRC_ALPHA: dst_factor = GL_ONE_MINUS_SRC_ALPHA; break;
	case GLS_DSTBLEND_DST_ALPHA: dst_factor =			GL_DST_ALPHA; break;
	case GLS_DSTBLEND_ONE_MINUS_DST_ALPHA: dst_factor = GL_ONE_MINUS_DST_ALPHA; break;
	default:
		dst_factor = GL_ZERO;
	}

	glEnable (GL_BLEND);
	glBlendFunc (src_factor, dst_factor);
}

ER_EXPORT void ER_DECL
er_gl_disable_blend ()
{
	glDisable (GL_BLEND);
}

ER_EXPORT void ER_DECL
er_gl_depth_mask (GLboolean is_true)
{
	glDepthMask (is_true == GL_TRUE ? GL_TRUE : GL_FALSE);
}

ER_EXPORT void ER_DECL
er_gl_polygon_mode (GLboolean is_line)
{
	glPolygonMode (GL_FRONT_AND_BACK, is_line == GL_TRUE ? GL_LINE : GL_FILL);
}

ER_EXPORT void ER_DECL
er_gl_depth_test (GLboolean will_disable)
{
	if (will_disable == GL_TRUE)
	{
		glDisable (GL_DEPTH_TEST);
	}
	else
	{
		glEnable (GL_DEPTH_TEST);
	}
}

ER_EXPORT void ER_DECL
er_set_viewport_and_scissor (
		const GLfloat *projection_matrix,
		GLint viewport_x, GLint viewport_y,
		GLsizei viewport_width, GLsizei viewport_height)
{
	glMatrixMode (GL_PROJECTION);
	glLoadMatrixf (projection_matrix);
	glMatrixMode (GL_MODELVIEW);

	// set the window clipping
	glViewport (viewport_x, viewport_y, viewport_width, viewport_height);
	glScissor (viewport_x, viewport_y, viewport_width, viewport_height);
}

