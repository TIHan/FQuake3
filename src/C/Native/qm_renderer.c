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

#include "qm_renderer.h"

MObject
qm_map_orientation (orientation_t* orientation)
{
	return m_invoke_method ("Engine.Renderer", "Engine.Renderer.Native", "Orientation", "ofNative", &orientation);
}

MObject
qm_map_orientationr (orientationr_t* orientation)
{
	return m_invoke_method ("Engine.Renderer", "Engine.Renderer.Native", "OrientationR", "ofNative", &orientation);
}

MObject
qm_map_plane (cplane_t* plane)
{
	return m_invoke_method ("Engine.Renderer", "Engine.Renderer.Native", "Plane", "ofNative", &plane);
}

MObject
qm_map_view_parms (viewParms_t* view_parms)
{
	static MMethod m_of_native;

	m_method_cache ("Engine.Renderer", "Engine.Renderer.Native", "ViewParms", "ofNative", m_of_native);

	return m_method_invoke (m_of_native, &view_parms);
}

MObject
qm_map_ref_entity (const refEntity_t* ref_entity)
{
	static MMethod m_of_native;

	m_method_cache ("Engine.Renderer", "Engine.Renderer.Native", "RefEntity", "ofNative", m_of_native);

	return m_method_invoke (m_of_native, &ref_entity);
}

MObject
qm_map_tr_ref_entity (trRefEntity_t* tr_ref_entity)
{
	return m_invoke_method ("Engine.Renderer", "Engine.Renderer.Native", "TrRefEntity", "ofNative", &tr_ref_entity);
}

MObject
qm_map_surface (surfaceType_t* surfaceType)
{
	return m_invoke_method ("Engine.Renderer", "Engine.Renderer.Native", "Surface", "ofNativePtr", &surfaceType);
}

MObject
qm_map_frustum (cplane_t* frustum)
{
	return m_invoke_method ("Engine.Renderer", "Engine.Renderer.Native", "Frustum", "ofNativePtr", &frustum);
}

void
qm_frustum_map (MObject obj, cplane_t* frustum)
{
	gpointer args [2];
	
	args [0] = m_object_as_arg (obj);
	args [1] = frustum;
	m_invoke_method ("Engine.Renderer", "Engine.Renderer.Native", "Frustum", "toNativePtr", args);
}

MObject
qm_map_draw_surf (drawSurf_t* draw_surf)
{
	return m_invoke_method ("Engine.Renderer", "Engine.Renderer.Native", "DrawSurface", "ofNative", &draw_surf);
}

MObject
qm_map_tr_globals (trGlobals_t* tr_globals)
{

}
