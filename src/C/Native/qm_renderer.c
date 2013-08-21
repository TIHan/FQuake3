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
qm_create_vector3 (gfloat x, gfloat y, gfloat z)
{
	gpointer *args;

	args [0] = &x;
	args [1] = &y;
	args [2] = &z;

	return m_object ("Engine", "Engine.QMath", "Vector3", 3, args);
}

MArray
qm_create_vector3_array (const gint size)
{
	return m_array ("Engine", "Engine.QMath", "Vector3", size);
}

MArray
qm_create_draw_vertex_array (const gint size)
{
	return m_array ("Engine", "Engine", "DrawVertex", size);
}

MArray
qm_create_poly_vertex_array (const gint size)
{
	return m_array ("Engine", "Engine", "PolyVertex", size);
}

MArray
qm_create_tr_ref_entity_array (const gint size)
{
	return m_array ("Engine", "Engine", "TrRefEntity", size);
}

MObject
qm_map_orientation (orientation_t* orientation)
{
	gpointer args[1];

	args [0] = orientation;
	return m_invoke_method ("Engine", "Engine", "Orientation", "ofNative", args);
}

MObject
qm_map_orientationr (orientationr_t* orientation)
{
	gpointer args[1];

	args [0] = orientation;
	return m_invoke_method ("Engine", "Engine", "OrientationR", "ofNative", args);
}

MObject
qm_map_plane (cplane_t* plane)
{
	gpointer args[1];

	args [0] = plane;
	return m_invoke_method ("Engine", "Engine", "Plane", "ofNative", args);
}

MArray
qm_create_plane_array (const gint size)
{
	return m_array ("Engine", "Engine", "Plane", size);
}

MObject
qm_map_view_parms (viewParms_t* view_parms)
{
	gpointer args[1];

	args [0] = view_parms;
	return m_invoke_method ("Engine", "Engine", "ViewParms", "ofNative", args);
}

MObject
qm_map_ref_entity (refEntity_t* ref_entity)
{
	gpointer args[1];

	args [0] = ref_entity;
	return m_invoke_method ("Engine", "Engine", "RefEntity", "ofNative", args);
}

MObject
qm_map_tr_ref_entity (trRefEntity_t* tr_ref_entity)
{
	gpointer args[1];

	args [0] = tr_ref_entity;
	return m_invoke_method ("Engine", "Engine", "TrRefEntity", "ofNative", args);
}

MObject
qm_map_surface (const surfaceType_t const* surfaceType)
{
	MObject m_surface;

	switch (*surfaceType)
	{
	case SF_FACE:
		{
		gpointer args[1];

		args [0] = (srfSurfaceFace_t*)surfaceType;
		m_surface = m_invoke_method ("Engine", "Engine", "Surface", "ofNativeFace", args);
		}
		break;
	case SF_TRIANGLES:
		{
		gpointer args[1];

		args [0] =  (srfTriangles_t*)surfaceType;
		m_surface = m_invoke_method ("Engine", "Engine", "Surface", "ofNativeTriangles", args);
		}
		break;
	case SF_POLY:
		{
		gpointer args[1];

		args [0] = (srfPoly_t*)surfaceType;
		m_surface = m_invoke_method ("Engine", "Engine", "Surface", "ofNativePoly", args);
		}
		break;
	case SF_DISPLAY_LIST:
		{
		gpointer args[1];

		args [0] = (srfDisplayList_t*)surfaceType;
		m_surface = m_invoke_method ("Engine", "Engine", "Surface", "ofNativeDisplayList", args);
		}
		break;
	case SF_BAD:
		{
		m_surface = m_invoke_method ("Engine", "Engine", "Surface", "NewBad", NULL);
		}
		break;
	case SF_SKIP:
		{
		m_surface = m_invoke_method ("Engine", "Engine", "Surface", "NewSkip", NULL);
		}
		break;
	case SF_GRID:
		{
		gpointer args[1];

		args [0] = (srfGridMesh_t*)surfaceType;
		m_surface = m_invoke_method ("Engine", "Engine", "Surface", "ofNativeGridMesh", args);
		}
		break;
	case SF_MD3:
		{
		m_surface = m_invoke_method ("Engine", "Engine", "Surface", "NewMd3", NULL);
		}
		break;
	case SF_MD4:
		{
		m_surface = m_invoke_method ("Engine", "Engine", "Surface", "NewMd4", NULL);
		}
		break;
	case SF_FLARE:
		{
		gpointer args[1];

		args [0] = (srfFlare_t*)surfaceType;
		m_surface = m_invoke_method ("Engine", "Engine", "Surface", "ofNativeFlare", args);
		}
		break;
	case SF_ENTITY:
		{
		m_surface = m_invoke_method ("Engine", "Engine", "Surface", "NewEntity", NULL);
		}
		break;
	default:
		g_error ("Invalid surface type.");
	}

	return m_surface;
}

MObject
qm_map_frustum (const frustum_t* frustum)
{
	MObject m_frustum;
	
	gpointer args[4];

	args [0] = m_object_as_arg (qm_map_plane (&frustum->left));
	args [1] = m_object_as_arg (qm_map_plane (&frustum->right));
	args [2] = m_object_as_arg (qm_map_plane (&frustum->bottom));
	args [3] = m_object_as_arg (qm_map_plane (&frustum->top));
	
	m_frustum = m_object ("Engine", "Engine", "Frustum", 4, args);

	return m_frustum;
}

void
qm_frustum_map (MObject obj, frustum_t* frustum)
{
	frustum->left = *(cplane_t *)m_object_unbox_struct (m_object_get_property (obj, "Left"));
	frustum->right = *(cplane_t *)m_object_unbox_struct (m_object_get_property (obj, "Right"));
	frustum->bottom = *(cplane_t *)m_object_unbox_struct (m_object_get_property (obj, "Bottom"));
	frustum->top = *(cplane_t *)m_object_unbox_struct (m_object_get_property (obj, "Top"));
}

MObject
qm_map_draw_surf (drawSurf_t* draw_surf)
{
	gpointer args[2];

	args [0] = &draw_surf->sort;
	args [1] = m_object_as_arg (qm_map_surface (draw_surf->surface));

	return m_object ("Engine", "Engine", "DrawSurface", 2, args);
}

MObject
qm_map_tr_globals (trGlobals_t* tr_globals)
{

}
