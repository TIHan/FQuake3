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

#ifndef __ENGINE_H__
#define __ENGINE_H__

#include <m.h>

#ifdef _WIN32
#include <intrin.h>
#endif

typedef struct
{
	float x;
	float y;
	float z;
	float w;
} vector4_t;

typedef union
{
	struct
	{
		__m128 m0, m1, m2, m3;
	};

	struct
	{
		gfloat seq[16];
	};

	struct
	{
		gfloat m[4][4];
	};

	struct
	{
		gfloat m00, m01, m02, m03;
		gfloat m10, m11, m12, m13;
		gfloat m20, m21, m22, m23;
		gfloat m30, m31, m32, m33;
	};

	struct
	{
		vector4_t v[4];
	};
} matrix16_t;
    

#endif /* __ENGINE_H__ */