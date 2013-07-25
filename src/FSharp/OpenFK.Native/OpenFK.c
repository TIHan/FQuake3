#include "OpenFK.h"

#include <smmintrin.h>

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
#if 0
			// This uses SSE4. May need more work in the future.
			__declspec(align(16)) vector4_t v1;
			__declspec(align(16)) vector4_t v2;
			__m128 result;

			v1.x = m1.values [i][0];
			v1.y = m1.values [i][1];
			v1.z = m1.values [i][2];
			v1.w = m1.values [i][3];

			v2.x = m2.values [0][j];
			v2.y = m2.values [1][j];
			v2.z = m2.values [2][j];
			v2.w = m2.values [3][j];
			result = _mm_dp_ps (*(__m128 *)&v1, *(__m128 *)&v2, 0xFF);
			m.values [i][j] = *(float *)&result;
#else
			m.values [i][j] =
				(m1.values [i][0] * m2.values [0][j]) +
				(m1.values [i][1] * m2.values [1][j]) +
				(m1.values [i][2] * m2.values [2][j]) +
				(m1.values [i][3] * m2.values [3][j]);
#endif

		}
	}

	return m;
}