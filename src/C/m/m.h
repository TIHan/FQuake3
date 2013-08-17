/*
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
*/

#ifndef __M_H__
#define __M_H__

#include <glib.h>

#if defined(_WIN32)
#	define M_IMPORT __declspec(dllimport)
#	define M_EXPORT	__declspec(dllexport)
#	define M_DECL __cdecl
#elif defined(__GNUC__)
#	define M_EXPORT __attribute__((visibility("default")))
#	define M_IMPORT
#	define M_DECL __attribute__((cdecl))
#else
#	error Compiler not supported.
#endif

typedef struct _MDomain MDomain;

typedef struct {
	gpointer __priv;
} MObject;

typedef struct {
	gpointer __priv;
} MArray;

typedef struct {
	gpointer __priv;
} MString;

typedef enum {
	M_RUNTIME_4_0,
	M_RUNTIME_4_5
} MRuntime;

void
m_setup_debugger (MDomain* domain);

MDomain*
m_domain_new (const gchar *assembly_dir, const gchar *config_dir, const char *root_domain_name, const MRuntime runtime);

void
m_domain_free (MDomain *const domain);

void
m_domain_exec (MDomain *const domain, const gchar *name, gint argc, gchar *argv[]);

void
m_load_assembly (const gchar *name);

MObject
m_object (const gchar *assembly_name, const gchar *name_space, const gchar *struct_name, gint argc, gpointer *args);

MObject
m_object_get_property (const MObject object, const gchar *property_name);

MArray
m_object_get_property_array (const MObject object, const gchar *property_name);

void
m_object_set_property (const MObject object, const gchar *property_name, gpointer value);

void
m_object_set_field (const MObject object, const gchar *field_name, gpointer value);

MObject
m_object_invoke (const MObject object, const gchar *method_name, gint argc, gpointer *args);

gpointer
m_object_unbox_struct (const MObject object);

gboolean
m_object_is_struct (MObject obj);

MObject
m_invoke_method (const gchar *assembly_name, const gchar *name_space, const gchar *static_class_name, const gchar *method_name, void **params);

MArray
m_array (const gchar *assembly_name, const gchar *name_space, const gchar *name, const gint size);

MArray
m_array_int32 (const gint size);

gchar*
m_array_addr_with_size (const MArray object, const gint size, const gint index);

gint
m_array_length (const MArray object);

MString
m_string (const gchar* text);

gpointer
m_object_as_arg (MObject obj);

gpointer
m_array_as_arg (MArray arr);

gpointer
m_string_as_arg (MString str);

#define m_array_addr(array,type,index) ((type*)(void*) m_array_addr_with_size (array, sizeof (type), index))
#define m_array_get(array,type,index) ( *(type*)m_array_addr ((array), type, (index)) ) 
#define m_array_set(array,type,index,value)	\
	do {	\
		type *__p = (type *) m_array_addr ((array), type, (index));	\
		*__p = (value);	\
	} while (0)

#define m_array_map(arr,argc,type,native_arr) \
{ \
	gint __i; \
\
	for (__i = 0; __i < argc; ++__i) \
	{ \
		m_array_set (arr, type, __i, native_arr [__i]); \
	} \
} \

#define m_array_map_objects(arr,argc,type,native_arr,obj_func) \
{ \
	gint __i; \
\
	for (__i = 0; __i < argc; ++__i) \
	{ \
		m_array_set (arr, type, __i, m_object_as_arg (obj_func (&native_arr [__i]))); \
	} \
} \

#define m_map_array(native_arr,argc,type,arr) \
{ \
	gint __i; \
\
	for (__i = 0; __i < argc; ++__i) \
	{ \
		native_arr [__i] = m_array_get (arr, type, __i); \
	} \
} \

#define m_invoke_method_easy(assembly_name,name_space,static_class_name,method_name,argc,arg_assignment,o) \
{ \
		gpointer __args [argc]; \
\
		arg_assignment \
\
		*(MObject *)&o = m_invoke_method (assembly_name, name_space, static_class_name, method_name, __args); \
} \

#endif /* __M_H__ */