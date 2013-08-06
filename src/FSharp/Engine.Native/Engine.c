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

#ifdef _WIN32
#include <intrin.h>
#endif

#include <stdio.h>
#include <string.h>

// http://stackoverflow.com/questions/2901694/programatically-detect-number-of-physical-processors-cores-or-if-hyper-threading/3082553#3082553
static void
cpu_id (unsigned i, unsigned regs[4])
{
#ifdef _WIN32
  __cpuid ((int *)regs, (int)i);
#else
	asm volatile
	("cpuid" : "=a" (regs[0]), "=b" (regs[1]), "=c" (regs[2]), "=d" (regs[3])
		: "a" (i), "c" (0));
	// ECX is set to zero for CPUID function 4
#endif
}

ENGINE_EXPORT
void
ENGINE_DECL qmath_matrix16_multiply (const matrix16_t const* m1, const matrix16_t const* m2, matrix16_t* m)
{
	int i, j;

	for (i = 0; i < 4; ++i)
	{
		for (j = 0; j < 4; ++j)
		{
			m->values [i][j] =
				(m1->values [i][0] * m2->values [0][j]) +
				(m1->values [i][1] * m2->values [1][j]) +
				(m1->values [i][2] * m2->values [2][j]) +
				(m1->values [i][3] * m2->values [3][j]);
		}
	}
}

ENGINE_EXPORT
void
ENGINE_DECL system_cpu_get_vendor_name (char vendor[12])
{
	unsigned regs[4];

	cpu_id (0, regs);
	((unsigned *)vendor) [0] = regs [1]; // EBX
	((unsigned *)vendor) [1] = regs [3]; // EDX
	((unsigned *)vendor) [2] = regs [2]; // ECX
}

ENGINE_EXPORT
int
ENGINE_DECL system_cpu_get_physical_core_count (void)
{
	int cores = 0;

	unsigned regs[4];
	char vendor[12];

	system_cpu_get_vendor_name (&vendor [0]);

	if (strncmp (vendor, "GenuineIntel", 12) == 0)
	{
		// Get DCP cache info
		cpu_id (4, regs);
		cores = ((regs[0] >> 26) & 0x3f) + 1; // EAX[31:26] + 1
	} else if (strncmp (vendor, "AuthenticAMD", 12) == 0)
	{
		// Get NC: Number of CPU cores - 1
		cpu_id (0x80000008, regs);
		cores = ((unsigned)(regs [2] & 0xff)) + 1; // ECX[7:0] + 1
	}

	return cores;
}