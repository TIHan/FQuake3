/*
===========================================================================
Copyright (C) 2013 William F. Smith
Copyright (C) 1999-2005 Id Software, Inc.

This file is part of Quake III Arena source code.

Quake III Arena source code is free software; you can redistribute it
and/or modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the License,
or (at your option) any later version.

Quake III Arena source code is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Foobar; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
===========================================================================
*/

// IMPORTANT: This file is only temporary and will not exist once most of the existing codebase
//	gets converted to F#. It is only intended as interop helpers.

#ifndef __QM_H__
#define __QM_H__

#include "game/q_shared.h"

#define define_mapping_assembly(name) \
static const gchar* Assembly = name \

#define define_mapping_namespace(name) \
static const gchar* Namespace = name \

#define of_invoke(static_class_name,method_name,arg) \
{ \
	MObject m_result; \
	m_invoke_method_cache (Assembly, Namespace, static_class_name, method_name, &arg, m_result); \
	return m_result; \
} \

#define to_invoke_easy(static_class_name,method_name,argc,args) \
{ \
	MObject m_void; \
	m_invoke_method_cache_easy (Assembly, Namespace, static_class_name, method_name, argc, args, m_void); \
	return m_void; \
} \

#define define_of_prototype(name,type) \
MObject \
qm_of_##name## (type ptr); \

#define define_of_ptr_prototype(name,type) \
MObject \
qm_of_ptr_##name## (type ptr); \

#define define_to_prototype(name,type) \
void \
qm_to_##name## (MObject obj, type ptr); \

#define define_to_ptr_prototype(name,type) \
void \
qm_to_ptr_##name## (MObject obj, type ptr); \

#define define_to_of_struct_prototype(name,type) \
void \
qm_to_of_struct_##name## (MObject obj, type ptr); \

#define define_of(name,type,managed_name) \
MObject \
qm_of_##name## (type ptr) \
{ \
	of_invoke (managed_name, "ofNative", ptr); \
} \

#define define_of_ptr(name,type,managed_name) \
	MObject \
	qm_of_ptr_##name## (type ptr) \
{ \
	to_invoke_easy(managed_name, "ofNativePtr", 1, { \
		__args[0] = ptr; \
	}); \
} \

#define define_to(name,type,managed_name) \
void \
qm_to_##name## (MObject obj, type ptr) \
{ \
	to_invoke_easy (managed_name, "toNativeByPtr", 2, { \
		__args [0] = ptr; \
		__args [1] = m_object_as_arg (obj); \
	}); \
} \

#define define_to_ptr(name,type,managed_name) \
void \
qm_to_ptr_##name## (MObject obj, type ptr) \
{ \
	to_invoke_easy (managed_name, "toNativeByPtr", 2, { \
		__args [0] = &ptr; \
		__args [1] = m_object_as_arg (obj); \
	}); \
} \

#define define_to_of_struct(name,type,managed_name) \
void \
qm_to_of_struct_##name## (MObject obj, type ptr) \
{ \
	to_invoke_easy (managed_name, "toNativeByPtr", 2, { \
		__args [0] = ptr; \
		__args [1] = m_object_unbox_struct (obj); \
	}); \
} \

#define define_mapping_prototype(name,type) \
	define_of_prototype(name,type) \
	define_of_ptr_prototype(name,type) \
	define_to_prototype(name,type) \
	define_to_ptr_prototype(name,type) \
	define_to_of_struct_prototype(name,type,managed_name) \

#define define_mapping(name,type,managed_name) \
	define_of(name,type,managed_name) \
	define_of_ptr(name,type,managed_name) \
	define_to(name,type,managed_name) \
	define_to_ptr(name,type,managed_name) \
	define_to_of_struct(name,type,managed_name) \

define_mapping_prototype (qboolean, qboolean*);
define_mapping_prototype (vec3, vec3_t);
define_mapping_prototype (vec4, vec4_t);
define_mapping_prototype (mat4x4, gfloat*);
define_mapping_prototype (cvar, cvar_t*);
define_mapping_prototype (bounds, vec3_t*);
define_mapping_prototype (axis, vec3_t*);

#endif /* __QM_H__ */