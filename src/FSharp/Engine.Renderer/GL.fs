(*
Copyright (c) 2013 William F. Smith

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
*)

module GL

// Disable native interop warnings
#nowarn "9"
#nowarn "51"

open System
open System.Security
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

[<Literal>]
let LibOpenGL = "opengl32.dll"

[<Literal>]
let OpenGLCallingConvention = CallingConvention.Cdecl

//

[<Literal>]
let GL_TRUE = 1

[<Literal>]
let GL_FALSE = 1

[<Literal>]
let GL_EQUAL = 0x0202

[<Literal>]
let GL_LEQUAL = 0x0202

[<Literal>]
let GL_MODELVIEW = 0x1700

[<Literal>]
let GL_PROJECTION = 0x1701

[<Literal>]
let GL_FRONT_AND_BACK = 0x0408

[<Literal>]
let GL_LINE = 0x1B01

[<Literal>]
let GL_FILL = 0x1B02

[<Literal>]
let GL_DEPTH_TEST = 0x0B71

[<Literal>]
let GL_ALPHA_TEST = 0x0BC0

[<Literal>]
let GL_GREATER = 0x0204

[<Literal>]
let GL_LESS = 0x0201

[<Literal>]
let GL_GEQUAL = 0x0206

// BlendingFactorDest
[<Literal>]
let GL_ZERO = 0

[<Literal>]
let GL_ONE = 1

[<Literal>]
let GL_SRC_COLOR = 0x0300

[<Literal>]
let GL_ONE_MINUS_SRC_COLOR = 0x0301

[<Literal>]
let GL_SRC_ALPHA = 0x0302

[<Literal>]
let GL_ONE_MINUS_SRC_ALPHA = 0x0303

[<Literal>]
let GL_DST_ALPHA = 0x0304

[<Literal>]
let GL_ONE_MINUS_DST_ALPHA = 0x0305

[<Literal>]
let GL_DST_COLOR = 0x0306

[<Literal>]
let GL_ONE_MINUS_DST_COLOR = 0x0307

[<Literal>]
let GL_SRC_ALPHA_SATURATE = 0x0308

//

[<Literal>]
let GL_BLEND = 0x0BE2

//

[<Literal>]
let GL_DEPTH_BUFFER_BIT = 0x00000100

[<Literal>]
let GL_ACCUM_BUFFER_BIT = 0x00000200

[<Literal>]
let GL_STENCIL_BUFFER_BIT = 0x00000400

[<Literal>]
let GL_VIEWPORT_BIT = 0x00000800

[<Literal>]
let GL_TRANSFORM_BIT = 0x00001000

[<Literal>]
let GL_EnABLE_BIT = 0x00002000

[<Literal>]
let GL_COLOR_BUFFER_BIT = 0x00004000

[<Literal>]
let GL_HINT_BIT = 0x00008000

//

type GLenum = uint32
type GLint = int
type GLsizei = int
type GLfloat = single
type GLboolean = byte
type GLclampf = single
type GLbitfield = uint32

let inline GLenum a = uint32 a
let inline GLboolean a = byte a
let inline GLcampf a = single a
let inline GLbitfield a = uint32 a

//

[<SuppressUnmanagedCodeSecurity>]
[<DllImport (LibOpenGL, CallingConvention = OpenGLCallingConvention)>]
extern void glMatrixMode (GLenum mode)

[<SuppressUnmanagedCodeSecurity>]
[<DllImport (LibOpenGL, CallingConvention = OpenGLCallingConvention)>]
extern void glLoadMatrixf (GLfloat *m)

[<SuppressUnmanagedCodeSecurity>]
[<DllImport (LibOpenGL, CallingConvention = OpenGLCallingConvention)>]
extern void glViewport (GLint x, GLint y, GLsizei width, GLsizei height)

[<SuppressUnmanagedCodeSecurity>]
[<DllImport (LibOpenGL, CallingConvention = OpenGLCallingConvention)>]
extern void glScissor (GLint x, GLint y, GLsizei width, GLsizei height)

[<SuppressUnmanagedCodeSecurity>]
[<DllImport (LibOpenGL, CallingConvention = OpenGLCallingConvention)>]
extern void glFinish ()

[<SuppressUnmanagedCodeSecurity>]
[<DllImport (LibOpenGL, CallingConvention = OpenGLCallingConvention)>]
extern void glDepthFunc (GLenum func)

[<SuppressUnmanagedCodeSecurity>]
[<DllImport (LibOpenGL, CallingConvention = OpenGLCallingConvention)>]
extern void glEnable (GLenum cap)

[<SuppressUnmanagedCodeSecurity>]
[<DllImport (LibOpenGL, CallingConvention = OpenGLCallingConvention)>]
extern void glDisable (GLenum cap)

[<SuppressUnmanagedCodeSecurity>]
[<DllImport (LibOpenGL, CallingConvention = OpenGLCallingConvention)>]
extern void glBlendFunc (GLenum sfactor, GLenum dfactor)

[<SuppressUnmanagedCodeSecurity>]
[<DllImport (LibOpenGL, CallingConvention = OpenGLCallingConvention)>]
extern void glDepthMask (GLboolean flag)

[<SuppressUnmanagedCodeSecurity>]
[<DllImport (LibOpenGL, CallingConvention = OpenGLCallingConvention)>]
extern void glPolygonMode (GLenum face, GLenum mode)

[<SuppressUnmanagedCodeSecurity>]
[<DllImport (LibOpenGL, CallingConvention = OpenGLCallingConvention)>]
extern void glAlphaFunc (GLenum func, GLclampf ref)

[<SuppressUnmanagedCodeSecurity>]
[<DllImport (LibOpenGL, CallingConvention = OpenGLCallingConvention)>]
extern void glClearColor (GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha)

[<SuppressUnmanagedCodeSecurity>]
[<DllImport (LibOpenGL, CallingConvention = OpenGLCallingConvention)>]
extern void glClear (GLbitfield mask)
