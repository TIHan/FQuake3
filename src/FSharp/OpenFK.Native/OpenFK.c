#include "OpenFK.h"

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