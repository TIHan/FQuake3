#ifndef __M_H__
#define __M_H__

#include <glib.h>

typedef struct _MInstance MInstance;

typedef enum {
	M_RUNTIME_4_0,
	M_RUNTIME_4_5
} MRuntime;

MInstance
*m_instance_new (void);

void
m_init (MInstance *const instance, const gchar *assembly_dir, const gchar *config_dir, const char *root_domain_name, const MRuntime runtime);

void
m_cleanup (MInstance *const instance);

void
m_exec (MInstance *const instance, const gchar *name, int argc, char *argv[]);

void
m_load_assembly (MInstance *const instance, const gchar *name);

void
*m_invoke_function_from_module (MInstance *const instance, const gchar *name_space, const gchar *module_name, const gchar *method_name, void **params);

#endif /* __M_H__ */