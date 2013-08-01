/*
Copyright (c) 2013 OpenFK

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

#include "OpenFK.h"

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

FK_EXPORT
void
FK_DECL matrix16_multiply (const matrix16_t const* m1, const matrix16_t const* m2, matrix16_t* m)
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

FK_EXPORT
void
FK_DECL fk_cpu_get_vendor_name (char vendor[12])
{
	unsigned regs[4];

	cpu_id (0, regs);
	((unsigned *)vendor) [0] = regs [1]; // EBX
	((unsigned *)vendor) [1] = regs [3]; // EDX
	((unsigned *)vendor) [2] = regs [2]; // ECX
}

FK_EXPORT
int
FK_DECL fk_cpu_get_physical_core_count (void)
{
	int cores = 0;

	unsigned regs[4];
	char vendor[12];

	fk_cpu_get_vendor_name (&vendor [0]);

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