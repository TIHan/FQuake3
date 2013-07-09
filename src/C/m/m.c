#include "m.h"

#include <glib/gprintf.h>
#include <mono/jit/jit.h>
#include <mono/metadata/assembly.h>
#include <mono/metadata/debug-helpers.h>

struct _MInstance {
	gint assemblies_size;
	GArray *assemblies;
	MonoDomain *domain;
	MonoAssembly *main_assembly;
};

struct _MRecord {
	MonoObject *object;
	gchar *name_space;
	gchar *class_name;
};


MInstance*
m_instance_new (void)
{
	return (MInstance *)g_malloc0 (sizeof (MInstance));
}


void
m_init (MInstance *const instance, const gchar *assembly_dir, const gchar *config_dir, const char *root_domain_name, const MRuntime runtime)
{
	gchar *version;

	g_return_if_fail (instance->domain == NULL);

	switch (runtime)
	{
	case M_RUNTIME_4_0:
		version = "v4.0.30319";
		break;
	case M_RUNTIME_4_5:
		version = "v4.0.30319";
		break;
	default:
		g_error ("M: Invalid runtime version.");
	}

	instance->assemblies_size = 0;
	instance->assemblies = g_array_new (FALSE, TRUE, 256);
	mono_set_dirs (assembly_dir, config_dir);
	instance->domain = mono_jit_init_version (root_domain_name, version);
}


void
m_instance_free (MInstance *const instance)
{
	mono_jit_cleanup (instance->domain);
	g_free (instance);
}


void
m_record_free (MRecord *const record)
{
	g_free (record);
}


void
m_exec (MInstance *const instance, const gchar *name, int argc, char *argv[])
{
	instance->main_assembly = mono_domain_assembly_open (instance->domain, name);
	if (!instance->main_assembly)
		g_error ("M: Unable to load %s main assembly.\n", name);

	mono_jit_exec (instance->domain, instance->main_assembly, argc - 1, argv + 1);
}


void
m_load_assembly (MInstance *const instance, const gchar *name)
{
	const MonoAssembly *const assembly = mono_domain_assembly_open (instance->domain, name);
	if (!assembly)
		g_error ("M: Unable to load %s assembly.\n", name);

	g_array_append_val (instance->assemblies, assembly);
	++instance->assemblies_size;
}


void*
m_invoke_function_from_module (MInstance *const instance, const gchar *name_space, const gchar *module_name, const gchar *method_name, void **params)
{
	gchar name[256];
	gint i;

	g_sprintf (name, "%s.%s:%s", name_space, module_name, method_name);

	for (i = 0; i < instance->assemblies_size; ++i)
	{
		MonoAssembly *const assembly = g_array_index (instance->assemblies, MonoAssembly*, i);
		MonoImage *const image = mono_assembly_get_image (assembly);
		MonoMethodDesc *const method_desc = mono_method_desc_new (name, FALSE);
		MonoMethod *const method = mono_method_desc_search_in_image (method_desc, image);
		
		mono_method_desc_free (method_desc);
		if (method)
		{
			MonoObject *const retval = mono_runtime_invoke (method, NULL, params, NULL);
			
			return mono_object_unbox (retval);
		}
	}

	g_error ("M: Unable to invoke %s.\n", name);
}