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




#endif /* __OPENFK_H__ */