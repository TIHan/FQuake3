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

#include "renderer\tr_local.h"

MObject
qm_map_orientation (orientation_t* orientation);

MObject
qm_map_orientationr (orientationr_t* orientation);

MObject
qm_map_plane (cplane_t* plane);

MObject
qm_map_view_parms (viewParms_t* view_parms);

MObject
qm_map_ref_entity (const refEntity_t* ref_entity);

MObject
qm_map_tr_ref_entity (trRefEntity_t* tr_ref_entity);

MObject
qm_map_surface (surfaceType_t* surfaceType);

MObject
qm_map_frustum (cplane_t* frustum);

MObject
qm_map_draw_surf (drawSurf_t* draw_surf);

MObject
qm_map_tr_globals (trGlobals_t* tr_globals);

/*
=================
=================
*/

void
qm_qboolean_map (MObject obj, qboolean *b);

void
qm_matrix16_map (MObject obj, matrix16_t* m);

void
qm_bounds_map (MObject obj, bounds_t* bounds);

void
qm_frustum_map (MObject obj, cplane_t* frustum);

void
qm_view_parms_map (MObject obj, viewParms_t* view_parms);

void
qm_tr_globals_map (MObject obj, trGlobals_t* tr_globals);

#endif /* __QM_RENDERER_H__ */