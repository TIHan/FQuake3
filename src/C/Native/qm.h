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

#define map_obj_invoke(static_class_name,method_name,arg) \
{ \
	MObject m_result; \
	m_invoke_method_cache ("Engine", "Engine.Native", static_class_name, method_name, arg, m_result); \
	return m_result; \
} \

#define obj_map_invoke_easy(static_class_name,method_name,argc,args) \
{ \
	MObject m_void; \
	m_invoke_method_cache_easy ("Engine", "Engine.Native", static_class_name, method_name, argc, args, m_void); \
	return m_void; \
} \

#define define_function_map_obj(name,type,managed_name) \
MObject \
qm_map_##name## (type ptr) \
{ \
	map_obj_invoke (managed_name, "ofNative", ptr); \
} \

#define define_function_obj_map(name,type,managed_name) \
void \
	qm_##name##_map (MObject obj, type ptr) \
{ \
	obj_map_invoke_easy (managed_name, "toNativeByPtr", 2, { \
		__args [0] = m_object_as_arg (obj); \
		__args [1] = ptr; \
	}); \
} \

#define define_mapping_functions(name,type,managed_name) \
	define_function_map_obj(name,type,managed_name) \
	define_function_obj_map(name,type,managed_name) \

/*
=================
vec3
=================
*/

// TODO:

MObject
qm_map_cvar (cvar_t* cvar);

#endif /* __QM_H__ */