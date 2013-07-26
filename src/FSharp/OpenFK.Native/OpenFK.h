#ifndef __OPENFK_H__
#define __OPENFK_H__

#if defined(_WIN32)
#	define FK_IMPORT __declspec(dllimport)
#	define FK_EXPORT	__declspec(dllexport)
#	define FK_DECL __cdecl
#elif defined(__GNUC__)
#	define FK_EXPORT __attribute__((visibility("default")))
#	define FK_IMPORT
#	define FK_DECL __attribute__((cdecl))
#else
#	error Compiler not supported.
#endif

typedef struct {
	float x;
	float y;
	float z;
	float w;
} vector4_t;

typedef struct {
	float values[4][4];
} matrix16_t;

FK_EXPORT
void
FK_DECL matrix16_multiply (matrix16_t* m1, matrix16_t* m2, matrix16_t* m);




#endif /* __OPENFK_H__ */