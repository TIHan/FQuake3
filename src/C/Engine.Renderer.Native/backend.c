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
er_gl_enable_alpha_test (GLbitfield atest_bits)
{
	glEnable (GL_ALPHA_TEST);

	switch (atest_bits)
	{
	case GLS_ATEST_GT_0: glAlphaFunc (GL_GREATER, 0.0f); break;
	case GLS_ATEST_LT_80: glAlphaFunc (GL_LESS, 0.5f); break;
	case GLS_ATEST_GE_80: glAlphaFunc (GL_GEQUAL, 0.5f); break;
	default: return; // shouldn't happen
	}
}

ER_EXPORT void ER_DECL
er_gl_disable_alpha_test ()
{
	glDisable (GL_ALPHA_TEST);
}

ER_EXPORT void ER_DECL
er_gl_hyperspace_clear (GLclampf color)
{
	glClearColor (color, color, color, 1.0f);
	glClear (GL_COLOR_BUFFER_BIT);
}

ER_EXPORT void ER_DECL
er_gl_finish ()
{
	glFinish ();
}

ER_EXPORT GLbitfield ER_DECL
er_gl_get_clear_bits (GLboolean use_stencil_buffer, GLboolean use_color_buffer)
{
	// clear relevant buffers
	GLbitfield clear_bits = GL_DEPTH_BUFFER_BIT;

	clear_bits = use_stencil_buffer ? clear_bits | GL_STENCIL_BUFFER_BIT : clear_bits;
	clear_bits = use_color_buffer ? clear_bits | GL_COLOR_BUFFER_BIT : clear_bits;

	return clear_bits;
}

ER_EXPORT void ER_DECL
er_gl_clear_with_color (GLbitfield clear_bits, GLclampf red, GLclampf green, GLclampf blue)
{
	glClearColor (red, green, blue, 1.0f);
	glClear (clear_bits);
}

ER_EXPORT void ER_DECL
er_gl_clear (GLbitfield clear_bits)
{
	glClear (clear_bits);
}

ER_EXPORT void ER_DECL
er_gl_enable_clip_plane (const GLfloat *flip_matrix, const GLdouble *plane)
{
	glLoadMatrixf (flip_matrix);
	glClipPlane (GL_CLIP_PLANE0, plane);
	glEnable (GL_CLIP_PLANE0);
}

ER_EXPORT void ER_DECL
er_gl_disable_clip_plane ()
{
	glDisable (GL_CLIP_PLANE0);
}

ER_EXPORT void ER_DECL
er_gl_set_viewport_and_scissor (
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

