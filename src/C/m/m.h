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
typedef struct _MObject {
	gpointer priv;
} MObject;

typedef enum {
	M_RUNTIME_4_0,
	M_RUNTIME_4_5
} MRuntime;

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
m_object_get_property (MObject object, const gchar *property_name);

MObject
m_object_invoke (MObject object, const gchar *method_name, gint argc, gpointer *args);

void*
m_invoke_module_function (const gchar *assembly_name, const gchar *name_space, const gchar *module_name, const gchar *method_name, void **params);

#endif /* __M_H__ */