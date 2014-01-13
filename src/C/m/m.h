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
#	define M_IMPORT		__declspec(dllimport)
#	define M_EXPORT		__declspec(dllexport)
#	define M_DECL		__cdecl
#elif defined(__GNUC__)
#	define M_EXPORT		__attribute__((visibility("default")))
#	define M_IMPORT
#	define M_DECL		__attribute__((cdecl))
#else
#	error Compiler not supported.
#endif

typedef struct _MDomain MDomain;
typedef struct _MObject MObject;
typedef struct _MString MString;
typedef struct _MMethod MMethod;

MDomain*
m_domain_new (const gchar *assembly_dir, const gchar *config_dir, const gchar *filename);

void
m_domain_exec (MDomain *domain, const gchar *assembly_name, const gint argc, gchar *argv[]);

void
m_domain_free (MDomain *domain);

void
m_load_assembly (const gchar *name);

MMethod *
m_method (const gchar *assembly_name, const gchar *name_space, const gchar *static_class_name, const gchar *method_name);

MObject *
m_method_invoke (MMethod *method, gpointer *params);

MObject *
m_object (const gchar *assembly_name, const gchar *name_space, const gchar *struct_name, gint argc, gpointer *args);

MObject *
m_object_get_property (MObject *obj, const gchar *property_name);

MObject *
m_object_invoke (MObject *obj, const gchar *method_name, gint argc, gpointer *args);

gpointer
m_object_unbox (MObject *obj);

MString *
m_string (const gchar* text);

gpointer
m_object_as_arg (MObject *obj);

gpointer
m_string_as_arg (MString *str);

guint32
m_gchandle_new (MObject *obj, gboolean is_pinned);

MObject *
m_gchandle_get_target (guint32 handle);

void
m_gchandle_free(guint32 handle);

#define m_ub(obj,type) *(type*)m_object_unbox (obj)

#define m_method_cache(assembly_name,name_space,static_class_name,method_name,o) \
{ \
	static MMethod *method; \
	if (!method) \
		method = m_method (assembly_name, name_space, static_class_name, method_name); \
	o = method; \
} \

#define m_invoke(assembly_name,name_space,static_class_name,method_name,argc,arg_assignment,o) \
{ \
	gpointer __args[argc]; \
	MMethod *cache; \
	\
	m_method_cache (assembly_name, name_space, static_class_name, method_name, cache); \
	\
	arg_assignment \
	\
	o = m_method_invoke (cache, __args); \
} \

#define m_invoke_new(assembly_name,name_space,static_class_name,method_name,o,...) \
{ \
	gpointer __args[] = { __VA_ARGS__ }; \
\
	MMethod *cache; \
\
	m_method_cache (#assembly_name, #name_space, #static_class_name, #method_name, cache); \
	o = m_method_invoke (cache, __args); \
} \

#endif /* __M_H__ */