/*
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
*/

#include "Engine.h"

#define __SIMD__ 0

M_EXPORT
void
M_DECL math_matrix16_multiply (const matrix16_t const* m1, const matrix16_t const* m2, matrix16_t* m)
{
	int i, j;

	for (i = 0; i < 4; ++i)
	{
		for (j = 0; j < 4; ++j)
		{
#if __SIMD__
			__m128 row = _mm_set_ps (m1->m [i][0], m1->m [i][1], m1->m [i][2], m1->m [i][3]);
			__m128 col = _mm_set_ps (m2->m [0][j], m2->m [1][j], m2->m [2][j], m2->m [3][j]);
			__m128 result = _mm_dp_ps (row, col, 0xFF);
			m->m [i][j] = *(gfloat *)&result;
#else
			m->m [i][j] =
				(m1->m [i][0] * m2->m [0][j]) +
				(m1->m [i][1] * m2->m [1][j]) +
				(m1->m [i][2] * m2->m [2][j]) +
				(m1->m [i][3] * m2->m [3][j]);
#endif
		}
	}
}

