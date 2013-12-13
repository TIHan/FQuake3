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
#include <mono/metadata/mono-debug.h>
#include <mono/metadata/assembly.h>
#include <mono/metadata/debug-helpers.h>

typedef MonoDomain _MDomain;
typedef MonoObject _MObject;

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
	const gchar *options[] = { "--llvm", "-O=all" };

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
		g_error ("M: Unable to load %s assembly.\n", name);
}


MMethod
m_method (const gchar *assembly_name, const gchar *name_space, const gchar *static_class_name, const gchar *method_name)
{
	gchar name[256];

	MonoAssembly *assembly;
	MonoImage *image;
	MonoMethodDesc *method_desc;
	MonoMethod *method;

	MMethod result;

	get_method_desc (name_space, static_class_name, method_name, name);

	assembly = find_assembly (assembly_name);

	if (!assembly)
		g_error ("M: Unable to find assembly %s.\n", assembly_name);

	image = mono_assembly_get_image (assembly);
	method_desc = mono_method_desc_new (name, FALSE);
	method = mono_method_desc_search_in_image (method_desc, image);

	mono_method_desc_free (method_desc);

	if (method)
	{
		result.__priv = method;	
		return result;
	}

	g_error ("M: Unable to find %s.\n", name);
}


MObject *
m_method_invoke (MMethod method, void **params)
{
	if (method.__priv)
	{
		return (MObject*)mono_runtime_invoke((MonoMethod*)method.__priv, NULL, params, NULL);
	}

	g_error ("m: Method doesn't exist.\n");
}


MObject *
m_object (const gchar *assembly_name, const gchar *name_space, const gchar *name, gint argc, gpointer *args)
{
	MonoAssembly *assembly = find_assembly (assembly_name);
	MonoImage *image = image = mono_assembly_get_image (assembly);
	MonoClass *klass = mono_class_from_name (image, name_space, name);
	MonoObject *object = mono_object_new (mono_domain_get (), klass);
	MonoMethod *ctor = mono_class_get_method_from_name (klass, ".ctor", argc);
	MonoType *type = mono_class_get_type (klass);

	if (!ctor)
		g_error ("M: Unable to find constructor for type %s.\n", name);

	if (mono_type_is_struct (type))
	{
		mono_runtime_invoke (ctor, mono_object_unbox (object), args, NULL);
		return (MObject*)object;
	}

	mono_runtime_invoke (ctor, object, args, NULL);
	return (MObject*)object;
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


void
m_object_set_property (MObject *obj, const gchar *property_name, gpointer value)
{
	MonoClass *klass = mono_object_get_class ((MonoObject*)obj);
	MonoProperty *prop = mono_class_get_property_from_name (klass, property_name);
	MonoType *type = mono_class_get_type (klass);

	gpointer args[1];

	args[0] = value;
	if (mono_type_is_struct (type))
	{
		mono_property_set_value (prop, mono_object_unbox ((MonoObject*)obj), args, NULL); 
	}
	else
	{
		mono_property_set_value (prop, (MonoObject*)obj, args, NULL);
	}
}


void
m_object_set_field (MObject *obj, const gchar *field_name, gpointer value)
{
	MonoClass *klass = mono_object_get_class ((MonoObject*)obj);
	MonoClassField *field = mono_class_get_field_from_name (klass, field_name);
	MonoType *type = mono_class_get_type (klass);

	mono_field_set_value ((MonoObject*)obj, field, value);
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
m_object_unbox_struct (MObject *obj)
{
	return mono_object_unbox ((MonoObject*)obj);
}


MObject *
m_invoke_method (const gchar *assembly_name, const gchar *name_space, const gchar *static_class_name, const gchar *method_name, void **params)
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
		g_error("M: Unable to invoke %s.\n", name);

	return (MObject*)mono_runtime_invoke (method, NULL, params, NULL);
}


MString
m_string (const gchar* text)
{
	MString result;


	if (text)
	{
		result.__priv = mono_string_new (mono_domain_get (), text);
	}
	else
	{
		result.__priv = mono_string_new (mono_domain_get (), "");
	}
	return result;
}

//****************************
// Object / Array / String Arg Specific
//****************************

gpointer
m_object_as_arg (MObject *obj)
{
	MonoClass *klass = mono_object_get_class((MonoObject*)obj);
	MonoType *type = mono_class_get_type (klass);

	if (mono_type_is_struct (type)) 
	{
		return m_object_unbox_struct (obj);
	}
	return obj;
}

gpointer
m_string_as_arg (MString str)
{
	return str.__priv;
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