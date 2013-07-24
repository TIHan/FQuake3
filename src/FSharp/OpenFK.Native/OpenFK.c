#include "OpenFK.h"

FK_EXPORT
matrix16_t
FK_DECL matrix16_multiply (matrix16_t m1, matrix16_t m2)
{
	int i, j;
	matrix16_t m;

	for (i = 0; i < 4; ++i)
	{
		for (j = 0; j < 4; ++j)
		{
			m.values [i][j] = 
				(m1.values [i][0] * m2.values [0][j]) +
				(m1.values [i][1] * m2.values [1][j]) +
				(m1.values [i][2] * m2.values [2][j]) +
				(m1.values [i][3] * m2.values [3][j]);

		}
	}

	return m;
}