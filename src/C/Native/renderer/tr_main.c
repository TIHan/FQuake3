/*
===========================================================================
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
// tr_main.c -- main control flow for each frame

#include "tr_local.h"
#include "../qm_renderer.h" // IMPORTANT: Temporary
#include "../qm.h" // IMPORTANT: Temporary

trGlobals_t		tr;

static float	s_flipMatrix[16] = {
	// convert from our coordinate system (looking down X)
	// to OpenGL's coordinate system (looking down -Z)
	0, 0, -1, 0,
	-1, 0, 0, 0,
	0, 1, 0, 0,
	0, 0, 0, 1
};


refimport_t	ri;

// entities that will have procedurally generated surfaces will just
// point at this for their sorting surface
surfaceType_t	entitySurface = SF_ENTITY;


/*
=================
R_CullLocalBox

Returns CULL_IN, CULL_CLIP, or CULL_OUT
=================
*/
gint
R_CullLocalBox (vec3_t bounds[2])
{
	MObject m_clip_type;

	m_invoke_method_easy ("Engine", "Engine", "MainRenderer", "CullLocalBox", 4, {
		__args [0] = (vector3_t *)bounds;
		__args [1] = m_object_as_arg (qm_map_orientationr (&tr.or));
		__args [2] = m_object_as_arg (qm_map_frustum (&tr.viewParms.frustum));
		__args [3] = m_object_as_arg (qm_map_cvar (r_nocull));
	}, m_clip_type);

	return *(gint *)m_object_unbox_struct (m_clip_type);
}

/*
** R_CullLocalPointAndRadius
*/
gint
R_CullLocalPointAndRadius( vec3_t pt, float radius )
{
	MObject m_clip_type;

	m_invoke_method_easy ("Engine", "Engine", "MainRenderer", "CullLocalPointAndRadius", 5, {
		__args [0] = pt;
		__args [1] = &radius;
		__args [2] = m_object_as_arg (qm_map_orientationr (&tr.or));
		__args [3] = m_object_as_arg (qm_map_frustum (&tr.viewParms.frustum));
		__args [4] = m_object_as_arg (qm_map_cvar (r_nocull));
	}, m_clip_type);

	return *(gint *)m_object_unbox_struct (m_clip_type);
}

/*
** R_CullPointAndRadius
*/
gint
R_CullPointAndRadius( vec3_t pt, float radius )
{
	MObject m_clip_type;

	m_invoke_method_easy ("Engine", "Engine", "MainRenderer", "CullPointAndRadius", 4, {
		__args [0] = pt;
		__args [1] = &radius;
		__args [2] = m_object_as_arg (qm_map_frustum (&tr.viewParms.frustum));
		__args [3] = m_object_as_arg (qm_map_cvar (r_nocull));
	}, m_clip_type);

	return *(gint *)m_object_unbox_struct (m_clip_type);
}


/*
=================
R_LocalNormalToWorld

=================
*/
void
R_LocalNormalToWorld (vec3_t local, vec3_t world) {
	MObject m_world;

	m_invoke_method_easy ("Engine", "Engine", "MainRenderer", "LocalNormalToWorld", 2, {
		__args [0] = local;
		__args [1] = m_object_as_arg (qm_map_orientationr (&tr.or));
	}, m_world);

	*(vector3_t *)world = *(vector3_t *)m_object_unbox_struct (m_world);
}

/*
=================
R_LocalPointToWorld

=================
*/
void
R_LocalPointToWorld (vec3_t local, vec3_t world)
{
	MObject m_world;

	m_invoke_method_easy ("Engine", "Engine", "MainRenderer", "LocalPointToWorld", 2, {
		__args [0] = local;
		__args [1] = m_object_as_arg (qm_map_orientationr (&tr.or));
	}, m_world);

	*(vector3_t *)world = *(vector3_t *)m_object_unbox_struct (m_world);
}

/*
=================
R_WorldToLocal

=================
*/
void
R_WorldToLocal (vec3_t world, vec3_t local)
{
	MObject m_local;

	m_invoke_method_easy ("Engine", "Engine", "MainRenderer", "WorldToLocal", 2, {
		__args [0] = local;
		__args [1] = m_object_as_arg (qm_map_orientationr (&tr.or));
	}, m_local);

	*(vector3_t *)local = *(vector3_t *)m_object_unbox_struct (m_local);
}

/*
==========================
R_TransformModelToClip

==========================
*/
void
R_TransformModelToClip( const vec3_t src, const float *modelMatrix, const float *projectionMatrix,
							vec4_t eye, vec4_t dst ) {
	MObject m_tuple_source_and_destination;

	m_invoke_method_easy ("Engine", "Engine", "MainRenderer", "TransformModelToClip", 3, {
		__args [0] = (vector3_t*)src;
		__args [1] = (matrix16_t*)modelMatrix;
		__args [2] = (matrix16_t*)projectionMatrix;
	}, m_tuple_source_and_destination);

	*(vector4_t *)eye = *(vector4_t *)m_object_unbox_struct (m_object_get_property (m_tuple_source_and_destination, "Item1"));
	*(vector4_t *)dst = *(vector4_t *)m_object_unbox_struct (m_object_get_property (m_tuple_source_and_destination, "Item2"));
}

/*
==========================
R_TransformClipToWindow

==========================
*/
void
R_TransformClipToWindow (const vec4_t clip, const viewParms_t *view, vec4_t normalized, vec4_t window)
{
	MObject m_tuple_normalized_and_window;

	m_invoke_method_easy ("Engine", "Engine", "MainRenderer", "TransformClipToWindow", 2, {
		__args [0] = (vector3_t *)clip;
		__args [1] = m_object_as_arg (qm_map_view_parms (view));
	}, m_tuple_normalized_and_window);

	*(vector4_t *)normalized = *(vector4_t *)m_object_unbox_struct (m_object_get_property (m_tuple_normalized_and_window, "Item1"));
	*(vector4_t *)window = *(vector4_t *)m_object_unbox_struct (m_object_get_property (m_tuple_normalized_and_window, "Item2"));
}


/*
==========================
myGlMultMatrix

==========================
*/
void
myGlMultMatrix (const gfloat *a, const gfloat *b, gfloat *out)
{
	MObject m_out;

	m_invoke_method_easy ("Engine", "Engine", "MainRenderer", "MyGLMultMatrix", 2, {
		__args [0] = (matrix16_t *)a;
		__args [1] = (matrix16_t *)b;
	}, m_out);

	*(matrix16_t *)out = *(matrix16_t *)m_object_unbox_struct (m_out);
}

/*
=================
R_RotateForEntity

Generates an orientation for an entity and viewParms
Does NOT produce any GL calls
Called by both the front end and the back end
=================
*/
void R_RotateForEntity( const trRefEntity_t *ent, const viewParms_t *viewParms,
					   orientationr_t *or ) {
	MObject m_or;

	m_invoke_method_easy ("Engine", "Engine", "MainRenderer", "RotateForEntity", 3, {
		__args [0] = m_object_as_arg (qm_map_tr_ref_entity (ent));
		__args [1] = m_object_as_arg (qm_map_view_parms (viewParms));
		__args [2] = m_object_as_arg (qm_map_orientationr (or));
	}, m_or);

	*or = *(orientationr_t *)m_object_unbox_struct (m_or);
}

/*
=================
R_RotateForViewer

Sets up the modelview matrix for a given viewParm
=================
*/
void
R_RotateForViewer (void) 
{
	MObject m_or;

	m_invoke_method_easy ("Engine", "Engine", "MainRenderer", "RotateForViewer", 1, {
		__args [0] = m_object_as_arg (qm_map_view_parms (&tr.viewParms));
	}, m_or);

	tr.viewParms.world = tr.or = *(orientationr_t *)m_object_unbox_struct (m_or);
}


/*
===============
R_SetupProjection
===============
*/
void
R_SetupProjection (void)
{
	MObject m_tuple_projection_matrix_and_zFar;

	m_invoke_method_easy ("Engine", "Engine", "MainRenderer", "SetupProjection", 5, {
		__args [0] = &r_znear->value;
		__args [1] = &tr.refdef.rdflags;
		__args [2] = m_object_as_arg (qm_map_view_parms (&tr.viewParms));
		__args [3] = &tr.refdef.fov_x;
		__args [4] = &tr.refdef.fov_y;
	}, m_tuple_projection_matrix_and_zFar);

	*(matrix16_t *)tr.viewParms.projectionMatrix = *(matrix16_t *)m_object_unbox_struct (m_object_get_property (m_tuple_projection_matrix_and_zFar, "Item1"));
	tr.viewParms.zFar = *(gfloat *)m_object_unbox_struct (m_object_get_property (m_tuple_projection_matrix_and_zFar, "Item2"));
}

/*
=================
R_SetupFrustum

Setup that culling frustum planes for the current view
=================
*/
void
R_SetupFrustum (void)
{
	MObject m_frustum;

	m_invoke_method_easy ("Engine", "Engine", "MainRenderer", "SetupFrustum", 1, {
		__args [0] = m_object_as_arg (qm_map_view_parms (&tr.viewParms));
	}, m_frustum);

	qm_frustum_map (m_frustum, &tr.viewParms.frustum);
}


/*
=================
R_MirrorPoint
=================
*/
void
R_MirrorPoint (vec3_t in, orientation_t *surface, orientation_t *camera, vec3_t out)
{
	MObject m_out;

	m_invoke_method_easy ("Engine", "Engine", "MainRenderer", "MirrorPoint", 3, {
		__args [0] = (vector3_t *)in;
		__args [1] = m_object_as_arg (qm_map_orientation (surface));
		__args [2] = m_object_as_arg (qm_map_orientation (camera));
	}, m_out);

	*(vector3_t *)out = *(vector3_t *)m_object_unbox_struct (m_out);
}

void
R_MirrorVector (vec3_t in, orientation_t *surface, orientation_t *camera, vec3_t out)
{
	MObject m_out;

	m_invoke_method_easy ("Engine", "Engine", "MainRenderer", "MirrorVector", 3, {
		__args [0] = (vector3_t *)in;
		__args [1] = m_object_as_arg (qm_map_orientation (surface));
		__args [2] = m_object_as_arg (qm_map_orientation (camera));
	}, m_out);

	*(vector3_t *)out = *(vector3_t *)m_object_unbox_struct (m_out);
}


/*
=============
R_PlaneForSurface
=============
*/
void
R_PlaneForSurface (surfaceType_t *surfType, cplane_t *plane)
{
	MObject m_out;

	if (!surfType) {
		g_error ("Bad surface type.");
		return;
	}

	m_invoke_method_easy ("Engine", "Engine", "MainRenderer", "PlaneForSurface", 2, {
		__args [0] = m_object_as_arg (qm_map_surface (surfType));
		__args [1] = m_object_as_arg (qm_map_plane (plane));
	}, m_out);

	*plane = *(cplane_t*)m_object_unbox_struct (m_out);
}

/*
=================
R_GetPortalOrientation

entityNum is the entity that the portal surface is a part of, which may
be moving and rotating.

Returns qtrue if it should be mirrored
=================
*/
qboolean R_GetPortalOrientations( drawSurf_t *drawSurf, int entityNum, 
							 orientation_t *surface, orientation_t *camera,
							 vec3_t pvsOrigin, qboolean *mirror ) {
	int			i;
	cplane_t	originalPlane, plane;
	trRefEntity_t	*e;
	float		d;
	vec3_t		transformed;

	// create plane axis for the portal we are seeing
	{
		MObject m_original_plane;

		m_invoke_method_easy ("Engine", "Engine", "MainRenderer", "CreatePlaneAxis", 1, {
			__args [0] = m_object_as_arg (qm_map_draw_surf (drawSurf));
		}, m_original_plane)

		originalPlane = *(cplane_t*)m_object_unbox_struct (m_original_plane);
	}

	// rotate the plane if necessary
	if ( entityNum != ENTITYNUM_WORLD ) {
		tr.currentEntityNum = entityNum;
		tr.currentEntity = &tr.refdef.entities[entityNum];

		// get the orientation of the entity
		R_RotateForEntity( tr.currentEntity, &tr.viewParms, &tr.or );

		// rotate the plane, but keep the non-rotated version for matching
		// against the portalSurface entities
		R_LocalNormalToWorld( originalPlane.normal, plane.normal );
		plane.dist = originalPlane.dist + DotProduct( plane.normal, tr.or.origin );

		// translate the original plane
		originalPlane.dist = originalPlane.dist + DotProduct( originalPlane.normal, tr.or.origin );
	} else {
		plane = originalPlane;
	}

	VectorCopy( plane.normal, surface->axis[0] );
	PerpendicularVector( surface->axis[1], surface->axis[0] );
	CrossProduct( surface->axis[0], surface->axis[1], surface->axis[2] );

	// locate the portal entity closest to this plane.
	// origin will be the origin of the portal, origin2 will be
	// the origin of the camera
	for ( i = 0 ; i < tr.refdef.num_entities ; i++ ) {
		e = &tr.refdef.entities[i];
		if ( e->e.reType != RT_PORTALSURFACE ) {
			continue;
		}

		d = DotProduct( e->e.origin, originalPlane.normal ) - originalPlane.dist;
		if ( d > 64 || d < -64) {
			continue;
		}

		// get the pvsOrigin from the entity
		VectorCopy( e->e.oldorigin, pvsOrigin );

		// if the entity is just a mirror, don't use as a camera point
		if ( e->e.oldorigin[0] == e->e.origin[0] && 
			e->e.oldorigin[1] == e->e.origin[1] && 
			e->e.oldorigin[2] == e->e.origin[2] ) {
			VectorScale( plane.normal, plane.dist, surface->origin );
			VectorCopy( surface->origin, camera->origin );
			VectorSubtract( vec3_origin, surface->axis[0], camera->axis[0] );
			VectorCopy( surface->axis[1], camera->axis[1] );
			VectorCopy( surface->axis[2], camera->axis[2] );

			*mirror = qtrue;
			return qtrue;
		}

		// project the origin onto the surface plane to get
		// an origin point we can rotate around
		d = DotProduct( e->e.origin, plane.normal ) - plane.dist;
		VectorMA( e->e.origin, -d, surface->axis[0], surface->origin );
			
		// now get the camera origin and orientation
		VectorCopy( e->e.oldorigin, camera->origin );
		AxisCopy( e->e.axis, camera->axis );
		VectorSubtract( vec3_origin, camera->axis[0], camera->axis[0] );
		VectorSubtract( vec3_origin, camera->axis[1], camera->axis[1] );

		// optionally rotate
		if ( e->e.oldframe ) {
			// if a speed is specified
			if ( e->e.frame ) {
				// continuous rotate
				d = (tr.refdef.time/1000.0f) * e->e.frame;
				VectorCopy( camera->axis[1], transformed );
				RotatePointAroundVector( camera->axis[1], camera->axis[0], transformed, d );
				CrossProduct( camera->axis[0], camera->axis[1], camera->axis[2] );
			} else {
				// bobbing rotate, with skinNum being the rotation offset
				d = sin( tr.refdef.time * 0.003f );
				d = e->e.skinNum + d * 4;
				VectorCopy( camera->axis[1], transformed );
				RotatePointAroundVector( camera->axis[1], camera->axis[0], transformed, d );
				CrossProduct( camera->axis[0], camera->axis[1], camera->axis[2] );
			}
		}
		else if ( e->e.skinNum ) {
			d = e->e.skinNum;
			VectorCopy( camera->axis[1], transformed );
			RotatePointAroundVector( camera->axis[1], camera->axis[0], transformed, d );
			CrossProduct( camera->axis[0], camera->axis[1], camera->axis[2] );
		}
		*mirror = qfalse;
		return qtrue;
	}

	// if we didn't locate a portal entity, don't render anything.
	// We don't want to just treat it as a mirror, because without a
	// portal entity the server won't have communicated a proper entity set
	// in the snapshot

	// unfortunately, with local movement prediction it is easily possible
	// to see a surface before the server has communicated the matching
	// portal surface entity, so we don't want to print anything here...

	//ri.Printf( PRINT_ALL, "Portal surface without a portal entity\n" );

	return qfalse;
}

static qboolean IsMirror( const drawSurf_t *drawSurf, int entityNum )
{
	int			i;
	cplane_t	originalPlane, plane;
	trRefEntity_t	*e;
	float		d;

	// create plane axis for the portal we are seeing
	{
		MObject m_original_plane;

		m_invoke_method_easy ("Engine", "Engine", "MainRenderer", "CreatePlaneAxis", 1, {
			__args [0] = m_object_as_arg (qm_map_draw_surf (drawSurf));
		}, m_original_plane)

		originalPlane = *(cplane_t*)m_object_unbox_struct (m_original_plane);
	}

	// rotate the plane if necessary
	if ( entityNum != ENTITYNUM_WORLD ) 
	{
		tr.currentEntityNum = entityNum;
		tr.currentEntity = &tr.refdef.entities[entityNum];

		// get the orientation of the entity
		R_RotateForEntity( tr.currentEntity, &tr.viewParms, &tr.or );

		// rotate the plane, but keep the non-rotated version for matching
		// against the portalSurface entities
		R_LocalNormalToWorld( originalPlane.normal, plane.normal );
		plane.dist = originalPlane.dist + DotProduct( plane.normal, tr.or.origin );

		// translate the original plane
		originalPlane.dist = originalPlane.dist + DotProduct( originalPlane.normal, tr.or.origin );
	} 
	else 
	{
		plane = originalPlane;
	}

	// locate the portal entity closest to this plane.
	// origin will be the origin of the portal, origin2 will be
	// the origin of the camera
	for ( i = 0 ; i < tr.refdef.num_entities ; i++ ) 
	{
		e = &tr.refdef.entities[i];
		if ( e->e.reType != RT_PORTALSURFACE ) {
			continue;
		}

		d = DotProduct( e->e.origin, originalPlane.normal ) - originalPlane.dist;
		if ( d > 64 || d < -64) {
			continue;
		}

		// if the entity is just a mirror, don't use as a camera point
		if ( e->e.oldorigin[0] == e->e.origin[0] && 
			e->e.oldorigin[1] == e->e.origin[1] && 
			e->e.oldorigin[2] == e->e.origin[2] ) 
		{
			return qtrue;
		}

		return qfalse;
	}
	return qfalse;
}

/*
** SurfIsOffscreen
**
** Determines if a surface is completely offscreen.
*/
static qboolean SurfIsOffscreen( const drawSurf_t *drawSurf, vec4_t clipDest[128] ) {
	float shortest = 100000000;
	int entityNum;
	int numTriangles;
	shader_t *shader;
	int		fogNum;
	int dlighted;
	vec4_t clip, eye;
	int i;
	unsigned int pointOr = 0;
	unsigned int pointAnd = (unsigned int)~0;

	if ( glConfig.smpActive ) {		// FIXME!  we can't do RB_BeginSurface/RB_EndSurface stuff with smp!
		return qfalse;
	}

	R_RotateForViewer();

	R_DecomposeSort( drawSurf->sort, &entityNum, &shader, &fogNum, &dlighted );
	RB_BeginSurface( shader, fogNum );
	rb_surfaceTable[ *drawSurf->surface ]( drawSurf->surface );

	assert( tess.numVertexes < 128 );

	for ( i = 0; i < tess.numVertexes; i++ )
	{
		int j;
		unsigned int pointFlags = 0;

		R_TransformModelToClip( tess.xyz[i], tr.or.modelMatrix, tr.viewParms.projectionMatrix, eye, clip );

		for ( j = 0; j < 3; j++ )
		{
			if ( clip[j] >= clip[3] )
			{
				pointFlags |= (1 << (j*2));
			}
			else if ( clip[j] <= -clip[3] )
			{
				pointFlags |= ( 1 << (j*2+1));
			}
		}
		pointAnd &= pointFlags;
		pointOr |= pointFlags;
	}

	// trivially reject
	if ( pointAnd )
	{
		return qtrue;
	}

	// determine if this surface is backfaced and also determine the distance
	// to the nearest vertex so we can cull based on portal range.  Culling
	// based on vertex distance isn't 100% correct (we should be checking for
	// range to the surface), but it's good enough for the types of portals
	// we have in the game right now.
	numTriangles = tess.numIndexes / 3;

	for ( i = 0; i < tess.numIndexes; i += 3 )
	{
		vec3_t normal;
		float dot;
		float len;

		VectorSubtract( tess.xyz[tess.indexes[i]], tr.viewParms.or.origin, normal );

		len = VectorLengthSquared( normal );			// lose the sqrt
		if ( len < shortest )
		{
			shortest = len;
		}

		if ( ( dot = DotProduct( normal, tess.normal[tess.indexes[i]] ) ) >= 0 )
		{
			numTriangles--;
		}
	}
	if ( !numTriangles )
	{
		return qtrue;
	}

	// mirrors can early out at this point, since we don't do a fade over distance
	// with them (although we could)
	if ( IsMirror( drawSurf, entityNum ) )
	{
		return qfalse;
	}

	if ( shortest > (tess.shader->portalRange*tess.shader->portalRange) )
	{
		return qtrue;
	}

	return qfalse;
}

/*
========================
R_MirrorViewBySurface

Returns qtrue if another view has been rendered
========================
*/
qboolean R_MirrorViewBySurface (drawSurf_t *drawSurf, int entityNum) {
	vec4_t			clipDest[128];
	viewParms_t		newParms;
	viewParms_t		oldParms;
	orientation_t	surface, camera;

	// don't recursively mirror
	if (tr.viewParms.isPortal) {
		ri.Printf( PRINT_DEVELOPER, "WARNING: recursive mirror/portal found\n" );
		return qfalse;
	}

	if ( r_noportals->integer || (r_fastsky->integer == 1) ) {
		return qfalse;
	}

	// trivially reject portal/mirror
	if ( SurfIsOffscreen( drawSurf, clipDest ) ) {
		return qfalse;
	}

	// save old viewParms so we can return to it after the mirror view
	oldParms = tr.viewParms;

	newParms = tr.viewParms;
	newParms.isPortal = qtrue;
	if ( !R_GetPortalOrientations( drawSurf, entityNum, &surface, &camera, 
		newParms.pvsOrigin, &newParms.isMirror ) ) {
		return qfalse;		// bad portal, no portalentity
	}

	R_MirrorPoint (oldParms.or.origin, &surface, &camera, newParms.or.origin );

	VectorSubtract( vec3_origin, camera.axis[0], newParms.portalPlane.normal );
	newParms.portalPlane.dist = DotProduct( camera.origin, newParms.portalPlane.normal );
	
	R_MirrorVector (oldParms.or.axis[0], &surface, &camera, newParms.or.axis[0]);
	R_MirrorVector (oldParms.or.axis[1], &surface, &camera, newParms.or.axis[1]);
	R_MirrorVector (oldParms.or.axis[2], &surface, &camera, newParms.or.axis[2]);

	// OPTIMIZE: restrict the viewport on the mirrored view

	// render the mirror view
	R_RenderView (&newParms);

	tr.viewParms = oldParms;

	return qtrue;
}

/*
=================
R_SpriteFogNum

See if a sprite is inside a fog volume
=================
*/
int R_SpriteFogNum( trRefEntity_t *ent ) {
	int				i, j;
	fog_t			*fog;

	if ( tr.refdef.rdflags & RDF_NOWORLDMODEL ) {
		return 0;
	}

	for ( i = 1 ; i < tr.world->numfogs ; i++ ) {
		fog = &tr.world->fogs[i];
		for ( j = 0 ; j < 3 ; j++ ) {
			if ( ent->e.origin[j] - ent->e.radius >= fog->bounds[1][j] ) {
				break;
			}
			if ( ent->e.origin[j] + ent->e.radius <= fog->bounds[0][j] ) {
				break;
			}
		}
		if ( j == 3 ) {
			return i;
		}
	}

	return 0;
}

/*
==========================================================================================

DRAWSURF SORTING

==========================================================================================
*/

/*
=================
qsort replacement

=================
*/
#define	SWAP_DRAW_SURF(a,b) temp=((int *)a)[0];((int *)a)[0]=((int *)b)[0];((int *)b)[0]=temp; temp=((int *)a)[1];((int *)a)[1]=((int *)b)[1];((int *)b)[1]=temp;

/* this parameter defines the cutoff between using quick sort and
   insertion sort for arrays; arrays with lengths shorter or equal to the
   below value use insertion sort */

#define CUTOFF 8            /* testing shows that this is good value */

static void shortsort( drawSurf_t *lo, drawSurf_t *hi ) {
    drawSurf_t	*p, *max;
	int			temp;

    while (hi > lo) {
        max = lo;
        for (p = lo + 1; p <= hi; p++ ) {
            if ( p->sort > max->sort ) {
                max = p;
            }
        }
        SWAP_DRAW_SURF(max, hi);
        hi--;
    }
}


/* sort the array between lo and hi (inclusive)
FIXME: this was lifted and modified from the microsoft lib source...
 */

void qsortFast (
    void *base,
    unsigned num,
    unsigned width
    )
{
    char *lo, *hi;              /* ends of sub-array currently sorting */
    char *mid;                  /* points to middle of subarray */
    char *loguy, *higuy;        /* traveling pointers for partition step */
    unsigned size;              /* size of the sub-array */
    char *lostk[30], *histk[30];
    int stkptr;                 /* stack for saving sub-array to be processed */
	int	temp;

	if ( sizeof(drawSurf_t) != 8 ) {
		ri.Error( ERR_DROP, "change SWAP_DRAW_SURF macro" );
	}

    /* Note: the number of stack entries required is no more than
       1 + log2(size), so 30 is sufficient for any array */

    if (num < 2 || width == 0)
        return;                 /* nothing to do */

    stkptr = 0;                 /* initialize stack */

    lo = base;
    hi = (char *)base + width * (num-1);        /* initialize limits */

    /* this entry point is for pseudo-recursion calling: setting
       lo and hi and jumping to here is like recursion, but stkptr is
       prserved, locals aren't, so we preserve stuff on the stack */
recurse:

    size = (hi - lo) / width + 1;        /* number of el's to sort */

    /* below a certain size, it is faster to use a O(n^2) sorting method */
    if (size <= CUTOFF) {
         shortsort((drawSurf_t *)lo, (drawSurf_t *)hi);
    }
    else {
        /* First we pick a partititioning element.  The efficiency of the
           algorithm demands that we find one that is approximately the
           median of the values, but also that we select one fast.  Using
           the first one produces bad performace if the array is already
           sorted, so we use the middle one, which would require a very
           wierdly arranged array for worst case performance.  Testing shows
           that a median-of-three algorithm does not, in general, increase
           performance. */

        mid = lo + (size / 2) * width;      /* find middle element */
        SWAP_DRAW_SURF(mid, lo);               /* swap it to beginning of array */

        /* We now wish to partition the array into three pieces, one
           consisiting of elements <= partition element, one of elements
           equal to the parition element, and one of element >= to it.  This
           is done below; comments indicate conditions established at every
           step. */

        loguy = lo;
        higuy = hi + width;

        /* Note that higuy decreases and loguy increases on every iteration,
           so loop must terminate. */
        for (;;) {
            /* lo <= loguy < hi, lo < higuy <= hi + 1,
               A[i] <= A[lo] for lo <= i <= loguy,
               A[i] >= A[lo] for higuy <= i <= hi */

            do  {
                loguy += width;
            } while (loguy <= hi &&  
				( ((drawSurf_t *)loguy)->sort <= ((drawSurf_t *)lo)->sort ) );

            /* lo < loguy <= hi+1, A[i] <= A[lo] for lo <= i < loguy,
               either loguy > hi or A[loguy] > A[lo] */

            do  {
                higuy -= width;
            } while (higuy > lo && 
				( ((drawSurf_t *)higuy)->sort >= ((drawSurf_t *)lo)->sort ) );

            /* lo-1 <= higuy <= hi, A[i] >= A[lo] for higuy < i <= hi,
               either higuy <= lo or A[higuy] < A[lo] */

            if (higuy < loguy)
                break;

            /* if loguy > hi or higuy <= lo, then we would have exited, so
               A[loguy] > A[lo], A[higuy] < A[lo],
               loguy < hi, highy > lo */

            SWAP_DRAW_SURF(loguy, higuy);

            /* A[loguy] < A[lo], A[higuy] > A[lo]; so condition at top
               of loop is re-established */
        }

        /*     A[i] >= A[lo] for higuy < i <= hi,
               A[i] <= A[lo] for lo <= i < loguy,
               higuy < loguy, lo <= higuy <= hi
           implying:
               A[i] >= A[lo] for loguy <= i <= hi,
               A[i] <= A[lo] for lo <= i <= higuy,
               A[i] = A[lo] for higuy < i < loguy */

        SWAP_DRAW_SURF(lo, higuy);     /* put partition element in place */

        /* OK, now we have the following:
              A[i] >= A[higuy] for loguy <= i <= hi,
              A[i] <= A[higuy] for lo <= i < higuy
              A[i] = A[lo] for higuy <= i < loguy    */

        /* We've finished the partition, now we want to sort the subarrays
           [lo, higuy-1] and [loguy, hi].
           We do the smaller one first to minimize stack usage.
           We only sort arrays of length 2 or more.*/

        if ( higuy - 1 - lo >= hi - loguy ) {
            if (lo + width < higuy) {
                lostk[stkptr] = lo;
                histk[stkptr] = higuy - width;
                ++stkptr;
            }                           /* save big recursion for later */

            if (loguy < hi) {
                lo = loguy;
                goto recurse;           /* do small recursion */
            }
        }
        else {
            if (loguy < hi) {
                lostk[stkptr] = loguy;
                histk[stkptr] = hi;
                ++stkptr;               /* save big recursion for later */
            }

            if (lo + width < higuy) {
                hi = higuy - width;
                goto recurse;           /* do small recursion */
            }
        }
    }

    /* We have sorted the array, except for any pending sorts on the stack.
       Check if there are any, and do them. */

    --stkptr;
    if (stkptr >= 0) {
        lo = lostk[stkptr];
        hi = histk[stkptr];
        goto recurse;           /* pop subarray from stack */
    }
    else
        return;                 /* all subarrays done */
}


//==========================================================================================

/*
=================
R_AddDrawSurf
=================
*/
void R_AddDrawSurf( surfaceType_t *surface, shader_t *shader, 
				   int fogIndex, int dlightMap ) {
	int			index;

	// instead of checking for overflow, we just mask the index
	// so it wraps around
	index = tr.refdef.numDrawSurfs & DRAWSURF_MASK;
	// the sort data is packed into a single 32 bit value so it can be
	// compared quickly during the qsorting process
	tr.refdef.drawSurfs[index].sort = (shader->sortedIndex << QSORT_SHADERNUM_SHIFT) 
		| tr.shiftedEntityNum | ( fogIndex << QSORT_FOGNUM_SHIFT ) | (int)dlightMap;
	tr.refdef.drawSurfs[index].surface = surface;
	tr.refdef.numDrawSurfs++;
}

/*
=================
R_DecomposeSort
=================
*/
void R_DecomposeSort( unsigned sort, int *entityNum, shader_t **shader, 
					 int *fogNum, int *dlightMap ) {
	*fogNum = ( sort >> QSORT_FOGNUM_SHIFT ) & 31;
	*shader = tr.sortedShaders[ ( sort >> QSORT_SHADERNUM_SHIFT ) & (MAX_SHADERS-1) ];
	*entityNum = ( sort >> QSORT_ENTITYNUM_SHIFT ) & 1023;
	*dlightMap = sort & 3;
}

/*
=================
R_SortDrawSurfs
=================
*/
void R_SortDrawSurfs( drawSurf_t *drawSurfs, int numDrawSurfs ) {
	shader_t		*shader;
	int				fogNum;
	int				entityNum;
	int				dlighted;
	int				i;

	// it is possible for some views to not have any surfaces
	if ( numDrawSurfs < 1 ) {
		// we still need to add it for hyperspace cases
		R_AddDrawSurfCmd( drawSurfs, numDrawSurfs );
		return;
	}

	// if we overflowed MAX_DRAWSURFS, the drawsurfs
	// wrapped around in the buffer and we will be missing
	// the first surfaces, not the last ones
	if ( numDrawSurfs > MAX_DRAWSURFS ) {
		numDrawSurfs = MAX_DRAWSURFS;
	}

	// sort the drawsurfs by sort type, then orientation, then shader
	qsortFast (drawSurfs, numDrawSurfs, sizeof(drawSurf_t) );

	// check for any pass through drawing, which
	// may cause another view to be rendered first
	for ( i = 0 ; i < numDrawSurfs ; i++ ) {
		R_DecomposeSort( (drawSurfs+i)->sort, &entityNum, &shader, &fogNum, &dlighted );

		if ( shader->sort > SS_PORTAL ) {
			break;
		}

		// no shader should ever have this sort type
		if ( shader->sort == SS_BAD ) {
			ri.Error (ERR_DROP, "Shader '%s'with sort == SS_BAD", shader->name );
		}

		// if the mirror was completely clipped away, we may need to check another surface
		if ( R_MirrorViewBySurface( (drawSurfs+i), entityNum) ) {
			// this is a debug option to see exactly what is being mirrored
			if ( r_portalOnly->integer ) {
				return;
			}
			break;		// only one mirror view at a time
		}
	}

	R_AddDrawSurfCmd( drawSurfs, numDrawSurfs );
}

/*
=============
R_AddEntitySurfaces
=============
*/
void R_AddEntitySurfaces (void) {
	trRefEntity_t	*ent;
	shader_t		*shader;

	if ( !r_drawentities->integer ) {
		return;
	}

	for ( tr.currentEntityNum = 0; 
	      tr.currentEntityNum < tr.refdef.num_entities; 
		  tr.currentEntityNum++ ) {
		ent = tr.currentEntity = &tr.refdef.entities[tr.currentEntityNum];

		ent->needDlights = qfalse;

		// preshift the value we are going to OR into the drawsurf sort
		tr.shiftedEntityNum = tr.currentEntityNum << QSORT_ENTITYNUM_SHIFT;

		//
		// the weapon model must be handled special --
		// we don't want the hacked weapon position showing in 
		// mirrors, because the true body position will already be drawn
		//
		if ( (ent->e.renderfx & RF_FIRST_PERSON) && tr.viewParms.isPortal) {
			continue;
		}

		// simple generated models, like sprites and beams, are not culled
		switch ( ent->e.reType ) {
		case RT_PORTALSURFACE:
			break;		// don't draw anything
		case RT_SPRITE:
		case RT_BEAM:
		case RT_LIGHTNING:
		case RT_RAIL_CORE:
		case RT_RAIL_RINGS:
			// self blood sprites, talk balloons, etc should not be drawn in the primary
			// view.  We can't just do this check for all entities, because md3
			// entities may still want to cast shadows from them
			if ( (ent->e.renderfx & RF_THIRD_PERSON) && !tr.viewParms.isPortal) {
				continue;
			}
			shader = R_GetShaderByHandle( ent->e.customShader );
			R_AddDrawSurf( &entitySurface, shader, R_SpriteFogNum( ent ), 0 );
			break;

		case RT_MODEL:
			// we must set up parts of tr.or for model culling
			R_RotateForEntity( ent, &tr.viewParms, &tr.or );

			tr.currentModel = R_GetModelByHandle( ent->e.hModel );
			if (!tr.currentModel) {
				R_AddDrawSurf( &entitySurface, tr.defaultShader, 0, 0 );
			} else {
				switch ( tr.currentModel->type ) {
				case MOD_MESH:
					R_AddMD3Surfaces( ent );
					break;
				case MOD_MD4:
					R_AddAnimSurfaces( ent );
					break;
				case MOD_BRUSH:
					R_AddBrushModelSurfaces( ent );
					break;
				case MOD_BAD:		// null model axis
					if ( (ent->e.renderfx & RF_THIRD_PERSON) && !tr.viewParms.isPortal) {
						break;
					}
					shader = R_GetShaderByHandle( ent->e.customShader );
					R_AddDrawSurf( &entitySurface, tr.defaultShader, 0, 0 );
					break;
				default:
					ri.Error( ERR_DROP, "R_AddEntitySurfaces: Bad modeltype" );
					break;
				}
			}
			break;
		default:
			ri.Error( ERR_DROP, "R_AddEntitySurfaces: Bad reType" );
		}
	}

}


/*
====================
R_GenerateDrawSurfs
====================
*/
void R_GenerateDrawSurfs( void ) {
	R_AddWorldSurfaces ();

	R_AddPolygonSurfaces();

	// set the projection matrix with the minimum zfar
	// now that we have the world bounded
	// this needs to be done before entities are
	// added, because they use the projection
	// matrix for lod calculation
	R_SetupProjection ();

	R_AddEntitySurfaces ();
}

/*
================
R_DebugPolygon
================
*/
void R_DebugPolygon( int color, int numPoints, float *points ) {
	int		i;

	GL_State( GLS_DEPTHMASK_TRUE | GLS_SRCBLEND_ONE | GLS_DSTBLEND_ONE );

	// draw solid shade

	qglColor3f( color&1, (color>>1)&1, (color>>2)&1 );
	qglBegin( GL_POLYGON );
	for ( i = 0 ; i < numPoints ; i++ ) {
		qglVertex3fv( points + i * 3 );
	}
	qglEnd();

	// draw wireframe outline
	GL_State( GLS_POLYMODE_LINE | GLS_DEPTHMASK_TRUE | GLS_SRCBLEND_ONE | GLS_DSTBLEND_ONE );
	qglDepthRange( 0, 0 );
	qglColor3f( 1, 1, 1 );
	qglBegin( GL_POLYGON );
	for ( i = 0 ; i < numPoints ; i++ ) {
		qglVertex3fv( points + i * 3 );
	}
	qglEnd();
	qglDepthRange( 0, 1 );
}

/*
====================
R_DebugGraphics

Visualization aid for movement clipping debugging
====================
*/
void R_DebugGraphics( void ) {
	if ( !r_debugSurface->integer ) {
		return;
	}

	// the render thread can't make callbacks to the main thread
	R_SyncRenderThread();

	GL_Bind( tr.whiteImage);
	GL_Cull( CT_FRONT_SIDED );
	ri.CM_DrawDebugSurface( R_DebugPolygon );
}


/*
================
R_RenderView

A view may be either the actual camera view,
or a mirror / remote location
================
*/
void R_RenderView (viewParms_t *parms) {
	int		firstDrawSurf;

	if ( parms->viewportWidth <= 0 || parms->viewportHeight <= 0 ) {
		return;
	}

	tr.viewCount++;

	tr.viewParms = *parms;
	tr.viewParms.frameSceneNum = tr.frameSceneNum;
	tr.viewParms.frameCount = tr.frameCount;

	firstDrawSurf = tr.refdef.numDrawSurfs;

	tr.viewCount++;

	// set viewParms.world
	R_RotateForViewer ();

	R_SetupFrustum ();

	R_GenerateDrawSurfs();

	R_SortDrawSurfs( tr.refdef.drawSurfs + firstDrawSurf, tr.refdef.numDrawSurfs - firstDrawSurf );

	// draw main system development information (surface outlines, etc)
	R_DebugGraphics();
}



