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

#include "m.h"

#include <glib/gprintf.h>
#include <mono/jit/jit.h>
#include <mono/metadata/threads.h>
#include <mono/metadata/mono-debug.h>
#include <mono/metadata/assembly.h>
#include <mono/metadata/debug-helpers.h>

typedef MonoDomain _MDomain;
typedef MonoObject _MObject;
typedef MonoString _MString;
typedef MonoMethod _MMethod;

typedef struct {
	MonoAssembly *assembly;
	const gchar *name;
} MAssemblyQuery;


static gchar *
assembly_get_name (MonoAssembly *assembly)
{
	return (gchar *)mono_image_get_name (mono_assembly_get_image (assembly));
}

static void
foreach_assembly (MonoAssembly *assembly, MAssemblyQuery *query)
{
	if (g_strcmp0 (assembly_get_name (assembly), query->name) == 0)
		query->assembly = assembly;
}

static MonoAssembly *
find_assembly (const gchar *name)
{
	MAssemblyQuery query;

	query.name = name;
	mono_assembly_foreach ((GFunc)foreach_assembly, &query);
	return query.assembly;
}


static void
get_method_desc (const gchar *name_space, const gchar *class_name, const gchar *method_name, gchar *name)
{
	g_sprintf (name, "%s.%s:%s", name_space, class_name, method_name);
}


MDomain*
m_domain_new (const gchar *assembly_dir, const gchar *config_dir, const gchar *filename)
{
#if NDEBUG
	const gchar *options[] = { "--llvm", "-O=all" };
#endif

	MDomain *domain;

	mono_set_dirs (assembly_dir, config_dir);
	domain = (MDomain*)mono_jit_init (filename);

#if NDEBUG
	mono_jit_parse_options(2, (gchar**)options);
#endif
	return domain;
}


void
m_domain_exec (MDomain *domain, const gchar *assembly_name, const gint argc, gchar *argv[])
{
	MonoAssembly *assembly = mono_domain_assembly_open ((MonoDomain*)domain, assembly_name);

	if (!assembly)
		g_error ("M: Unable to load %s assembly.\n", assembly_name);

	mono_jit_exec ((MonoDomain*)domain, assembly, argc, argv);
}


void
m_domain_free (MDomain *domain)
{
	mono_jit_cleanup ((MonoDomain*)domain);
	g_free (domain);
}


void
m_load_assembly (const gchar *name)
{
	MonoAssembly *const assembly = mono_domain_assembly_open (mono_domain_get (), name);
	if (!assembly)
		g_error ("m: Unable to load %s assembly.\n", name);
}


void
m_thread_attach ()
{
	mono_thread_attach (mono_domain_get ());
}


MMethod *
m_method (const gchar *assembly_name, const gchar *name_space, const gchar *static_class_name, const gchar *method_name)
{
	gchar name[256];

	MonoAssembly *assembly;
	MonoImage *image;
	MonoMethodDesc *method_desc;
	MonoMethod *method;

	get_method_desc (name_space, static_class_name, method_name, name);

	assembly = find_assembly (assembly_name);

	if (!assembly)
		g_error ("m: Unable to find assembly %s.\n", assembly_name);

	image = mono_assembly_get_image (assembly);
	method_desc = mono_method_desc_new (name, FALSE);
	method = mono_method_desc_search_in_image (method_desc, image);

	mono_method_desc_free (method_desc);

	if (!method)
		g_error("m: Unable to find %s.\n", name);

	return (MMethod*)method;
}


MObject *
m_method_invoke (MMethod *method, void **params)
{
	return (MObject*)mono_runtime_invoke((MonoMethod*)method, NULL, params, NULL);
}


gpointer
m_method_get_thunk (MMethod *method)
{
	return mono_method_get_unmanaged_thunk ((MonoMethod*)method);
}


MObject *
m_object_get_property (MObject *obj, const gchar *property_name)
{
	MonoClass *klass = mono_object_get_class ((MonoObject*)obj);
	MonoProperty *prop = mono_class_get_property_from_name (klass, property_name);
	MonoType *type = mono_class_get_type (klass);
	
	if (mono_type_is_struct (type))
	{
		return (MObject*)mono_property_get_value (prop, mono_object_unbox ((MonoObject *)obj), NULL, NULL);
	}

	return (MObject*)mono_property_get_value (prop, (MonoObject *)obj, NULL, NULL);
}


MObject *
m_object_invoke (MObject *obj, const gchar *method_name, gint argc, gpointer *args)
{
	MonoClass *klass = mono_object_get_class ((MonoObject*)obj);
	MonoMethod *method = mono_class_get_method_from_name (klass, method_name, argc);
	MonoType *type = mono_class_get_type (klass);

	if (mono_type_is_struct (type))
	{
		return (MObject*)mono_runtime_invoke (method, mono_object_unbox ((MonoObject*)obj), args, NULL);
	}
	
	return (MObject*)mono_runtime_invoke (method, (MonoObject*)obj, args, NULL);
}


gpointer
m_object_unbox (MObject *obj)
{
	return mono_object_unbox ((MonoObject*)obj);
}


MString *
m_string (const gchar* text)
{
	if (text)
	{
		return (MString*)mono_string_new (mono_domain_get (), text);
	}
	
	return (MString*)mono_string_new (mono_domain_get (), "");
}

//****************************
// Object / String Arg Specific
//****************************

gpointer
m_object_as_arg (MObject *obj)
{
	MonoClass *klass;
	MonoType *type;

	if (!obj)
		return NULL;

	klass = mono_object_get_class((MonoObject*)obj);
	type = mono_class_get_type(klass);

	if (mono_type_is_struct (type)) 
	{
		return m_object_unbox (obj);
	}
	return obj;
}

gpointer
m_string_as_arg (MString *str)
{
	return str;
}

//****************************
// GCHandle
//****************************

guint32
m_gchandle_new (MObject *obj, gboolean is_pinned)
{
	return mono_gchandle_new ((MonoObject*)obj, is_pinned);
}

MObject *
m_gchandle_get_target (guint32 handle)
{
	return (MObject*)mono_gchandle_get_target (handle);
}

void
m_gchandle_free (guint32 handle)
{
	mono_gchandle_free (handle);
}