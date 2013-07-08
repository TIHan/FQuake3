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

typedef struct _MInstance MInstance;
typedef struct _MRecord MRecord;

typedef enum {
	M_RUNTIME_4_0,
	M_RUNTIME_4_5
} MRuntime;

MInstance
*m_instance_new (void);

void
m_init (MInstance *const instance, const gchar *assembly_dir, const gchar *config_dir, const char *root_domain_name, const MRuntime runtime);

void
m_instance_free (MInstance *const instance);

void
m_record_free (MRecord *const record);

void
m_exec (MInstance *const instance, const gchar *name, int argc, char *argv[]);

void
m_load_assembly (MInstance *const instance, const gchar *name);

MRecord
*m_record_new (MInstance *const instance, const gchar *name_space, const gchar *class_name, void **params);

void
*m_invoke_function_from_module (MInstance *const instance, const gchar *name_space, const gchar *module_name, const gchar *method_name, void **params);

#endif /* __M_H__ */