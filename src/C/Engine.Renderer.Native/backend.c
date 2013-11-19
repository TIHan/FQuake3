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
#include <gl\glew.h>

ER_EXPORT void ER_DECL
er_gl_depth_func(GLboolean is_equal)
{
	glDepthFunc(is_equal == GL_TRUE ? GL_EQUAL : GL_LEQUAL);
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

