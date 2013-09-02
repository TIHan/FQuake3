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
	m->m00 = (m1->m00 * m2->m00) + (m1->m01 * m2->m10) + (m1->m02 * m2->m20) + (m1->m03 * m2->m30);
	m->m01 = (m1->m00 * m2->m01) + (m1->m01 * m2->m11) + (m1->m02 * m2->m21) + (m1->m03 * m2->m31);
	m->m02 = (m1->m00 * m2->m02) + (m1->m01 * m2->m12) + (m1->m02 * m2->m22) + (m1->m03 * m2->m32);
	m->m03 = (m1->m00 * m2->m03) + (m1->m01 * m2->m13) + (m1->m02 * m2->m23) + (m1->m03 * m2->m33);
	m->m10 = (m1->m10 * m2->m00) + (m1->m11 * m2->m10) + (m1->m12 * m2->m20) + (m1->m13 * m2->m30);
	m->m11 = (m1->m10 * m2->m01) + (m1->m11 * m2->m11) + (m1->m12 * m2->m21) + (m1->m13 * m2->m31);
	m->m12 = (m1->m10 * m2->m02) + (m1->m11 * m2->m12) + (m1->m12 * m2->m22) + (m1->m13 * m2->m32);
	m->m13 = (m1->m10 * m2->m03) + (m1->m11 * m2->m13) + (m1->m12 * m2->m23) + (m1->m13 * m2->m33);
	m->m20 = (m1->m20 * m2->m00) + (m1->m21 * m2->m10) + (m1->m22 * m2->m20) + (m1->m23 * m2->m30);
	m->m21 = (m1->m20 * m2->m01) + (m1->m21 * m2->m11) + (m1->m22 * m2->m21) + (m1->m23 * m2->m31);
	m->m22 = (m1->m20 * m2->m02) + (m1->m21 * m2->m12) + (m1->m22 * m2->m22) + (m1->m23 * m2->m32);
	m->m23 = (m1->m20 * m2->m03) + (m1->m21 * m2->m13) + (m1->m22 * m2->m23) + (m1->m23 * m2->m33);
	m->m30 = (m1->m30 * m2->m00) + (m1->m31 * m2->m10) + (m1->m32 * m2->m20) + (m1->m33 * m2->m30);
	m->m31 = (m1->m30 * m2->m01) + (m1->m31 * m2->m11) + (m1->m32 * m2->m21) + (m1->m33 * m2->m31);
	m->m32 = (m1->m30 * m2->m02) + (m1->m31 * m2->m12) + (m1->m32 * m2->m22) + (m1->m33 * m2->m32);
	m->m33 = (m1->m30 * m2->m03) + (m1->m31 * m2->m13) + (m1->m32 * m2->m23) + (m1->m33 * m2->m33);
/*
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
*/
}

