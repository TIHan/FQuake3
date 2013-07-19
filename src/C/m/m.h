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

void
m_object_set_property (MObject object, const gchar *property_name, gpointer value);

void
m_object_set_field (MObject object, const gchar *field_name, gpointer value);

MObject
m_object_invoke (MObject object, const gchar *method_name, gint argc, gpointer *args);

gpointer
m_object_unbox (MObject object);

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

gpointer
m_array_unbox (const MArray object);

MObject
m_value_box (const gchar *assembly_name, const gchar *name_space, const gchar *name, gpointer value);

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