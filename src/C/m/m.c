#include "m.h"

#include <glib/gprintf.h>
#include <mono/jit/jit.h>
#include <mono/metadata/assembly.h>
#include <mono/metadata/debug-helpers.h>

struct _MDomain {
	MonoDomain *domain;
};

typedef struct _MAssemblyQuery {
	MonoAssembly *assembly;
	const gchar *name;
} MAssemblyQuery;


static gchar*
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

static MonoAssembly*
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
m_domain_new (const gchar *assembly_dir, const gchar *config_dir, const char *root_domain_name, const MRuntime runtime)
{
	MDomain *const domain = (MDomain *)g_malloc0 (sizeof (MDomain));
	gchar *version;

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

	mono_set_dirs (assembly_dir, config_dir);
	domain->domain = mono_jit_init_version (root_domain_name, version);
	return domain;
}


void
m_domain_free (MDomain *const domain)
{
	mono_jit_cleanup (domain->domain);
	g_free (domain);
}


void
m_domain_exec (MDomain *const domain, const gchar *name, int argc, char *argv[])
{
	MonoAssembly *const assembly = mono_domain_assembly_open (domain->domain, name);
	if (!assembly)
		g_error ("M: Unable to load %s assembly.\n", name);

	mono_jit_exec (domain->domain, assembly, argc - 1, argv + 1);
}


void
m_load_assembly (const gchar *name)
{
	const MonoAssembly *const assembly = mono_domain_assembly_open (mono_domain_get (), name);
	if (!assembly)
		g_error ("M: Unable to load %s assembly.\n", name);
}


void*
m_invoke_function_from_module (const gchar *assembly_name, const gchar *name_space, const gchar *module_name, const gchar *method_name, void **params)
{
	gchar name[256];

	MonoAssembly *assembly;
	MonoImage *image;
	MonoMethodDesc *method_desc;
	MonoMethod *method;

	get_method_desc (name_space, module_name, method_name, name);

	assembly = find_assembly (assembly_name);
	image = mono_assembly_get_image (assembly);
	method_desc = mono_method_desc_new (name, FALSE);
	method = mono_method_desc_search_in_image (method_desc, image);

	mono_method_desc_free (method_desc);

	if (method)
	{
			MonoObject *const retval = mono_runtime_invoke (method, NULL, params, NULL);
			
			return mono_object_unbox (retval);
	}

	g_error ("M: Unable to invoke %s.\n", name);
}