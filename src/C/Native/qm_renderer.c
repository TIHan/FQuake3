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

MObject
qm_map_orientation (const orientation_t *const orientation)
{
	gpointer args[2];

	args [0] = (vector3_t *)orientation->origin;
	args [1] = (vector3_t *)orientation->axis;

	return m_object ("Engine", "Engine", "Orientation", 2, args);
}

MObject
qm_map_orientationr (const orientationr_t *const orientation)
{
	gpointer args[4];

	args [0] = (vector3_t *)orientation->origin;
	args [1] = (vector3_t *)orientation->axis;
	args [2] = (vector3_t *)orientation->viewOrigin;
	args [3] = (matrix16_t *)orientation->modelMatrix;

	return m_object ("Engine", "Engine", "OrientationR", 4, args);
}

MObject
qm_map_plane (const cplane_t *const plane)
{
	gpointer args[4];

	args [0] = (vector3_t *)plane->normal;
	args [1] = &plane->dist;
	args [2] = &plane->type;
	args [3] = &plane->signbits;

	return m_object ("Engine", "Engine", "Plane", 4, args);
}

MArray
qm_create_plane_array (const gint size)
{
	return m_array ("Engine", "Engine", "Plane", size);
}

MObject
qm_map_view_parms (const viewParms_t *const view_parms)
{
	MArray m_frustum = qm_create_plane_array (4);
	MArray m_visibility_bounds = qm_create_vector3_array (2);

	gpointer args[18];

	m_array_map (m_frustum, 4, cplane_t, view_parms->frustum);
	m_array_map (m_visibility_bounds, 2, vector3_t, ((vector3_t *)view_parms->visBounds));

	args [0] = m_object_as_arg (qm_map_orientationr (&view_parms->or));
	args [1] = m_object_as_arg (qm_map_orientationr (&view_parms->world));
	args [2] = (vector3_t *)view_parms->pvsOrigin;
	args [3] = &view_parms->isPortal;
	args [4] = &view_parms->isMirror;
	args [5] = &view_parms->frameSceneNum;
	args [6] = &view_parms->frameCount;
	args [7] = m_object_as_arg (qm_map_plane (&view_parms->portalPlane));
	args [8] = &view_parms->viewportX;
	args [9] = &view_parms->viewportY;
	args [10] = &view_parms->viewportWidth;
	args [11] = &view_parms->viewportHeight;
	args [12] = &view_parms->fovX;
	args [13] = &view_parms->fovY;
	args [14] = (matrix16_t *)view_parms->projectionMatrix;
	args [15] = m_array_as_arg (m_frustum);
	args [16] = m_array_as_arg (m_visibility_bounds);
	args [17] = &view_parms->zFar;

	return m_object ("Engine", "Engine", "ViewParms", 18, args);
}

MObject
qm_map_ref_entity (const refEntity_t const* ref_entity)
{
	gpointer args[20];

	args [0] = &ref_entity->reType;
	args [1] = &ref_entity->renderfx;
	args [2] = &ref_entity->hModel;
	args [3] = (vector3_t *)ref_entity->lightingOrigin;
	args [4] = &ref_entity->shadowPlane;
	args [5] = (vector3_t *)ref_entity->axis;
	args [6] = &ref_entity->nonNormalizedAxes;
	args [7] = (vector3_t *)ref_entity->origin;
	args [8] = &ref_entity->frame;
	args [9] = (vector3_t *)ref_entity->oldorigin;
	args [10] = &ref_entity->oldframe;
	args [11] = &ref_entity->backlerp;
	args [12] = &ref_entity->skinNum;
	args [13] = &ref_entity->customSkin;
	args [14] = &ref_entity->customShader;
	args [15] = ref_entity->shaderRGBA;
	args [16] = (vector2_t *)ref_entity->shaderTexCoord;
	args [17] = &ref_entity->shaderTime;
	args [18] = &ref_entity->radius;
	args [19] = &ref_entity->rotation;

	return m_object ("Engine", "Engine", "RefEntity", 20, args);
}

MObject
qm_map_tr_ref_entity (const trRefEntity_t const* tr_ref_entity)
{
	gpointer args[8];

	args [0] = m_object_as_arg (qm_map_ref_entity (&tr_ref_entity->e));
	args [1] = &tr_ref_entity->axisLength;
	args [2] = &tr_ref_entity->needDlights;
	args [3] = &tr_ref_entity->lightingCalculated;
	args [4] = (vector3_t *)tr_ref_entity->lightDir;
	args [5] = (vector3_t *)tr_ref_entity->ambientLight;
	args [6] = &tr_ref_entity->ambientLightInt;
	args [7] = (vector3_t *)tr_ref_entity->directedLight;

	return m_object ("Engine", "Engine", "TrRefEntity", 8, args);
}

MObject
qm_map_surface (const surfaceType_t const* surfaceType)
{
	MObject m_surface;

	switch (*surfaceType)
	{
	case SF_FACE:
		{
		srfSurfaceFace_t* surface = (srfSurfaceFace_t*)surfaceType;

		MObject m_type;

		gpointer args[7];

		args [0] = m_object_as_arg (qm_map_plane (&surface->plane));
		args [1] = &surface->dlightBits [0];
		args [2] = &surface->dlightBits [1];
		args [3] = &surface->numPoints;
		args [4] = &surface->numIndices;
		args [5] = &surface->ofsIndices;
		args [6] = &surface->points [0];
		
		m_type = m_object ("Engine", "Engine", "SurfaceFace", 7, args);

		args [0] = m_object_as_arg (m_type);

		m_surface = m_invoke_method ("Engine", "Engine", "Surface", "NewFace", args);
		}
		break;
	case SF_TRIANGLES:
		{
		srfTriangles_t* surface = (srfTriangles_t*)surfaceType;

		MArray m_indices = m_array_int32 (surface->numIndexes);
		MArray m_vertices = qm_create_draw_vertex_array (surface->numVerts);
		MObject m_type;

		gpointer args[7];

		m_array_map (m_indices, surface->numIndexes, gint, surface->indexes);
		m_array_map (m_vertices, surface->numVerts, drawVert_t, surface->verts);

		args [0] = &surface->dlightBits [0];
		args [1] = &surface->dlightBits [1];
		args [2] = (vector3_t*)surface->bounds;
		args [3] = (vector3_t*)surface->localOrigin;
		args [4] = &surface->radius;
		args [5] = m_array_as_arg (m_indices);
		args [6] = m_array_as_arg (m_vertices);

		m_type = m_object ("Engine", "Engine", "SurfaceTriangles", 7, args);

		args [0] = m_object_as_arg (m_type);

		m_surface = m_invoke_method ("Engine", "Engine", "Surface", "NewTriangles", args);
		}
		break;
	case SF_POLY:
		{
		srfPoly_t* surface = (srfPoly_t*)surfaceType;

		MArray m_vertices = qm_create_poly_vertex_array (surface->numVerts);
		MObject m_type;

		gpointer args[4];

		m_array_map (m_vertices, surface->numVerts, polyVert_t, surface->verts);

		args [0] = &surface->hShader;
		args [1] = &surface->fogIndex;
		args [2] = m_array_as_arg (m_vertices);

		m_type = m_object ("Engine", "Engine", "SurfacePoly", 3, args);

		args [0] = m_object_as_arg (m_type);

		m_surface = m_invoke_method ("Engine", "Engine", "Surface", "NewPoly", args);
		}
		break;
	case SF_DISPLAY_LIST:
		{
		srfDisplayList_t* surface = (srfDisplayList_t*)surfaceType;

		MObject m_type;

		gpointer args[1];

		args [0] = &surface->listNum;

		m_type = m_object ("Engine", "Engine", "SurfaceDisplayList", 1, args);

		args [0] = m_object_as_arg (m_type);

		m_surface = m_invoke_method ("Engine", "Engine", "Surface", "NewDisplayList", args);
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
		srfGridMesh_t* surface = (srfGridMesh_t*)surfaceType;

		MArray m_width_lod_error = m_array_int32 (surface->width);
		MArray m_height_lod_error = m_array_int32 (surface->height);
		MObject m_type;

		gpointer args[14];

		m_array_map (m_width_lod_error, surface->width, gfloat, surface->widthLodError);
		m_array_map (m_height_lod_error, surface->width, gfloat, surface->heightLodError);

		args [0] = &surface->dlightBits [0];
		args [1] = &surface->dlightBits [1];
		args [2] = (vector3_t*)surface->meshBounds;
		args [3] = (vector3_t*)surface->localOrigin;
		args [4] = &surface->meshRadius;
		args [5] = (vector3_t*)surface->lodOrigin;
		args [6] = &surface->lodRadius;
		args [7] = &surface->lodFixed;
		args [8] = &surface->lodStitched;
		args [9] = &surface->width;
		args [10] = &surface->height;
		args [11] = m_array_as_arg (m_width_lod_error);
		args [12] = m_array_as_arg (m_height_lod_error);
		args [13] = &surface->verts [0];

		m_type = m_object ("Engine", "Engine", "SurfaceGridMesh", 14, args);

		args [0] = m_object_as_arg (m_type);

		m_surface = m_invoke_method ("Engine", "Engine", "Surface", "NewGrid", args);
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
		srfFlare_t* surface = (srfFlare_t*)surfaceType;

		MObject m_type;

		gpointer args[3];

		args [0] = (vector3_t*)surface->origin;
		args [1] = (vector3_t*)surface->normal;
		args [2] = (vector3_t*)surface->color;

		m_type = m_object ("Engine", "Engine", "SurfaceFlare", 3, args);

		args [0] = m_object_as_arg (m_type);

		m_surface = m_invoke_method ("Engine", "Engine", "Surface", "NewFlare", args);
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
