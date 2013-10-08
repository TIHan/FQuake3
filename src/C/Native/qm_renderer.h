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

#ifndef __QM_RENDERER_H__
#define __QM_RENDERER_H__

#include "qm.h"
#include "renderer\tr_local.h"

define_mapping_prototype (orientationr, orientationr_t*);
define_mapping_prototype (frustum, cplane_t*);
define_mapping_prototype (bounds, vec3_t*);
define_mapping_prototype (view_parms, viewParms_t*);
define_mapping_prototype (ref_entity, refEntity_t*);

#endif /* __QM_RENDERER_H__ */