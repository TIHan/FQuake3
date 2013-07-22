namespace Engine

#nowarn "9"

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading
open System.Diagnostics
open Microsoft.FSharp.NativeInterop

module private NativeRenderer =
    
    [<Literal>]
    let libQuake3 = "quake3.dll"

    [<Literal>]
    let callingConvention = CallingConvention.Cdecl

    [<DllImport (libQuake3, CallingConvention = callingConvention)>]
    extern bool Cvar_GetNoCull ()

type CullType =
    | In = 0
    | Clip = 1
    | Out = 2

type PlaneType =
    | X = 0
    | Y = 1
    | Z = 2
    | NonAxial = 3

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Orientation =
    val Origin : Vector3        // in world coordinates

    [<MarshalAs (UnmanagedType.ByValArray, SizeConst = 3)>]
    val Axis : Vector3[]        // orientation in world
    val ViewOrigin : Vector3    // viewParams->or.origin in local coordinates

    [<MarshalAs (UnmanagedType.ByValArray, SizeConst = 16)>]
    val ModelMatrix : float32[]

    new (origin, axis, viewOrigin, modelMatrix) = { Origin = origin; Axis = axis; ViewOrigin = viewOrigin; ModelMatrix = modelMatrix }


[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = 20)>]
type Plane =
    
    [<FieldOffset (0)>]
    val Normal : Vector3

    [<FieldOffset (12)>]
    val Distance : single

    [<FieldOffset (16)>]
    [<MarshalAs (UnmanagedType.I8)>]
    val Type : PlaneType        // signx + (signy<<1) + (signz<<2), used as lookup during collision

    [<FieldOffset (17)>]
    val SignBits : byte

    new (normal, distance, typ, signBits) = { Normal = normal; Distance = distance; Type = typ; SignBits = signBits; }

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type ViewParms =
    val Orientation : Orientation
    val World : Orientation
    val PvsOrigin : Vector3
    val IsPortal : bool
    val IsMirror : bool
    val FrameSceneId : int
    val FrameCount : int
    val PortalPlane : Plane
    val ViewportX : int
    val ViewportY : int
    val ViewportWidth : int
    val ViewportHeight : int
    val FovX : float32
    val FovY : float32

    [<MarshalAs (UnmanagedType.ByValArray, SizeConst = 16)>]
    val ProjectionMatrix : float32[]

    [<MarshalAs (UnmanagedType.ByValArray, SizeConst = 4)>]
    val Frustum : Plane[]

    [<MarshalAs (UnmanagedType.ByValArray, SizeConst = 2)>]
    val VisibilityBounds : Vector3[]

    val ZFar : float32

    new (orientation, world, pvsOrigin, isPortal, isMirror, frameSceneId, frameCount, portalPlane, viewPortX, viewPortY, viewPortWidth, viewPortHeight, fovX, fovY, projectionMatrix, frustum, visibilityBounds, zFar) =
        {
            Orientation = orientation;
            World = world;
            PvsOrigin = pvsOrigin;
            IsPortal = isPortal;
            IsMirror = isMirror;
            FrameSceneId = frameSceneId;
            FrameCount = frameCount;
            PortalPlane = portalPlane;
            ViewportX = viewPortX;
            ViewportY = viewPortY;
            ViewportWidth = viewPortWidth;
            ViewportHeight = viewPortHeight;
            FovX = fovX;
            FovY = fovY;
            ProjectionMatrix = projectionMatrix;
            Frustum = frustum;
            VisibilityBounds = visibilityBounds;
            ZFar = zFar;
        }
        

module CvarModule =
    
    let GetNoCull () =
        NativeRenderer.Cvar_GetNoCull ()

module MainRenderer =
    module private LocalBox =
        /// <summary>
        /// Transform into world space.
        /// </summary>
        let TransformWorldSpace (bounds: Vector3[]) (orientation: Orientation) =
            let transformed : Vector3[] = Array.zeroCreate 8
        
            transformed |> Array.mapi (fun i x ->
                let v = Vector3 (bounds.[i &&& 1].X, bounds.[(i >>> 1) &&& 1].Y, bounds.[(i >>> 2) &&& 1].Z)

                orientation.Origin
                |> Vector3.MA v.X orientation.Axis.[0]
                |> Vector3.MA v.Y orientation.Axis.[1]
                |> Vector3.MA v.Z orientation.Axis.[2]
            )

        /// <summary>
        /// Check against frustum planes.
        /// </summary>
        let CheckFrustumPlanes (transformed: Vector3[]) (frustum: Plane[]) =
            let rec checkFrustumPlane (frust: Plane) front back isFront acc =
                match acc = Array.length transformed || isFront with
                | true -> (front, back)
                | _ ->
                    let distance = Vector3.DotProduct transformed.[acc] frust.Normal

                    match distance > frust.Distance with
                    | true -> checkFrustumPlane frust 1 back (back = 1) (acc + 1)
                    | _ -> checkFrustumPlane frust front 1 false (acc + 1)



            let rec checkFrustumPlanes anyBack isFront acc =
                match acc = Array.length frustum || isFront = false with
                | true -> (anyBack, isFront)
                | _ ->
                    let frust = frustum.[acc]

                    match checkFrustumPlane frust 0 0 false 0 with
                    | (front, back) ->
                        checkFrustumPlanes (anyBack ||| back) (front = 1) (acc + 1)

            match checkFrustumPlanes 0 true 0 with
            | (_, false) -> CullType.Out
            | (0, _) -> CullType.In
            | _ -> CullType.Clip

    /// <summary>
    ///
    /// </summary>
    module private PointAndRadius =
        let CheckFrustumPlanes (point: Vector3) (radius: single) (frustum: Plane[]) =
            let rec checkFrustumPlanes mightBeClipped canCullOut acc =
                match acc = Array.length frustum || canCullOut with
                | true -> (mightBeClipped, canCullOut)
                | _ ->
                    let frust = frustum.[acc]
                    let distance = (Vector3.DotProduct point frust.Normal) - frust.Distance

                    match distance < -radius with
                    | true -> checkFrustumPlanes mightBeClipped true (acc + 1)
                    | _ when distance <= radius -> checkFrustumPlanes true false (acc + 1)
                    | _ -> checkFrustumPlanes mightBeClipped false (acc + 1)

            match checkFrustumPlanes false false 0 with
            | (_, true) -> CullType.Out
            | (true, _) -> CullType.Clip
            | _ -> CullType.In


(*
int R_CullLocalBox (vec3_t bounds[2]) {
	int		i, j;
	vec3_t	transformed[8];
	float	dists[8];
	vec3_t	v;
	cplane_t	*frust;
	int			anyBack;
	int			front, back;

	if ( r_nocull->integer ) {
		return CULL_CLIP;
	}

	// transform into world space
	for (i = 0 ; i < 8 ; i++) {
		v[0] = bounds[i&1][0];
		v[1] = bounds[(i>>1)&1][1];
		v[2] = bounds[(i>>2)&1][2];

		VectorCopy( tr.or.origin, transformed[i] );
		VectorMA( transformed[i], v[0], tr.or.axis[0], transformed[i] );
		VectorMA( transformed[i], v[1], tr.or.axis[1], transformed[i] );
		VectorMA( transformed[i], v[2], tr.or.axis[2], transformed[i] );
	}

	// check against frustum planes
	anyBack = 0;
	for (i = 0 ; i < 4 ; i++) {
		frust = &tr.viewParms.frustum[i];

		front = back = 0;
		for (j = 0 ; j < 8 ; j++) {
			dists[j] = DotProduct(transformed[j], frust->normal);
			if ( dists[j] > frust->dist ) {
				front = 1;
				if ( back ) {
					break;		// a point is in front
				}
			} else {
				back = 1;
			}
		}
		if ( !front ) {
			// all points were behind one of the planes
			return CULL_OUT;
		}
		anyBack |= back;
	}

	if ( !anyBack ) {
		return CULL_IN;		// completely inside frustum
	}

	return CULL_CLIP;		// partially clipped
}
*)

    /// <summary>
    // R_CullLocalBox (vec3_t bounds[2])
    // </summary>
    let CullLocalBox (bounds: Vector3[]) (orientation: Orientation) (viewParms: ViewParms) =
        match CvarModule.GetNoCull () with
        | true -> CullType.Clip
        | _ ->

        // transform into world space
        let transformed = LocalBox.TransformWorldSpace bounds orientation

        // check against frustum planes
        LocalBox.CheckFrustumPlanes transformed viewParms.Frustum

(*
int R_CullPointAndRadius( vec3_t pt, float radius )
{
	int		i;
	float	dist;
	cplane_t	*frust;
	qboolean mightBeClipped = qfalse;

	if ( r_nocull->integer ) {
		return CULL_CLIP;
	}

	// check against frustum planes
	for (i = 0 ; i < 4 ; i++) 
	{
		frust = &tr.viewParms.frustum[i];

		dist = DotProduct( pt, frust->normal) - frust->dist;
		if ( dist < -radius )
		{
			return CULL_OUT;
		}
		else if ( dist <= radius ) 
		{
			mightBeClipped = qtrue;
		}
	}

	if ( mightBeClipped )
	{
		return CULL_CLIP;
	}

	return CULL_IN;		// completely inside frustum
}
*)

    /// <summary>
    /// R_CullPointAndRadius( vec3_t pt, float radius )
    /// </summary>
    let CullPointAndRadius (point: Vector3) (radius: single) (viewParms: ViewParms) =
        match CvarModule.GetNoCull () with
        | true -> CullType.Clip
        | _ ->

        PointAndRadius.CheckFrustumPlanes point radius viewParms.Frustum

(*
void R_LocalPointToWorld (vec3_t local, vec3_t world) {
	world[0] = local[0] * tr.or.axis[0][0] + local[1] * tr.or.axis[1][0] + local[2] * tr.or.axis[2][0] + tr.or.origin[0];
	world[1] = local[0] * tr.or.axis[0][1] + local[1] * tr.or.axis[1][1] + local[2] * tr.or.axis[2][1] + tr.or.origin[1];
	world[2] = local[0] * tr.or.axis[0][2] + local[1] * tr.or.axis[1][2] + local[2] * tr.or.axis[2][2] + tr.or.origin[2];
}
*)

    /// <summary>
    /// R_LocalPointToWorld (vec3_t local, vec3_t world)
    /// </summary>
    let LocalPointToWorld (local: Vector3) (orientation: Orientation) =
        Vector3 (
            (local.X * orientation.Axis.[0].X) + (local.Y * orientation.Axis.[1].X) + (local.Z * orientation.Axis.[2].X) + orientation.Origin.X,
            (local.X * orientation.Axis.[0].Y) + (local.Y * orientation.Axis.[1].Y) + (local.Z * orientation.Axis.[2].Y) + orientation.Origin.Y,
            (local.X * orientation.Axis.[0].Z) + (local.Y * orientation.Axis.[1].Z) + (local.Z * orientation.Axis.[2].Z) + orientation.Origin.Z
        )

(*
void R_LocalNormalToWorld (vec3_t local, vec3_t world) {
	world[0] = local[0] * tr.or.axis[0][0] + local[1] * tr.or.axis[1][0] + local[2] * tr.or.axis[2][0];
	world[1] = local[0] * tr.or.axis[0][1] + local[1] * tr.or.axis[1][1] + local[2] * tr.or.axis[2][1];
	world[2] = local[0] * tr.or.axis[0][2] + local[1] * tr.or.axis[1][2] + local[2] * tr.or.axis[2][2];
}
*)

    /// <summary>
    /// R_LocalNormalToWorld (vec3_t local, vec3_t world)
    /// </summary>
    let LocalNormalToWorld (local: Vector3) (orientation: Orientation) =
        Vector3 (
            (local.X * orientation.Axis.[0].X) + (local.Y * orientation.Axis.[1].X) + (local.Z * orientation.Axis.[2].X),
            (local.X * orientation.Axis.[0].Y) + (local.Y * orientation.Axis.[1].Y) + (local.Z * orientation.Axis.[2].Y),
            (local.X * orientation.Axis.[0].Z) + (local.Y * orientation.Axis.[1].Z) + (local.Z * orientation.Axis.[2].Z)
        )

(*
void R_WorldToLocal (vec3_t world, vec3_t local) {
	local[0] = DotProduct(world, tr.or.axis[0]);
	local[1] = DotProduct(world, tr.or.axis[1]);
	local[2] = DotProduct(world, tr.or.axis[2]);
}
*)

    /// <summary>
    /// R_WorldToLocal (vec3_t world, vec3_t local)
    /// </summary>
    let WorldToLocal (world: Vector3) (orientation: Orientation) =
        Vector3 (
            Vector3.DotProduct world orientation.Axis.[0],
            Vector3.DotProduct world orientation.Axis.[1],
            Vector3.DotProduct world orientation.Axis.[2]
        )


(*
int R_CullLocalPointAndRadius( vec3_t pt, float radius )
{
	vec3_t transformed;

	R_LocalPointToWorld( pt, transformed );

	return R_CullPointAndRadius( transformed, radius );
}
*)

    /// <summary>
    /// R_CullLocalPointAndRadius( vec3_t pt, float radius )
    /// </summary>
    let CullLocalPointAndRadius (point: Vector3) (radius: single) (orientation: Orientation) (viewParms: ViewParms) =
        let transformed = LocalPointToWorld point orientation
        CullPointAndRadius transformed radius viewParms

(*
void R_TransformModelToClip( const vec3_t src, const float *modelMatrix, const float *projectionMatrix,
							vec4_t eye, vec4_t dst ) {
	int i;

	for ( i = 0 ; i < 4 ; i++ ) {
		eye[i] = 
			src[0] * modelMatrix[ i + 0 * 4 ] +
			src[1] * modelMatrix[ i + 1 * 4 ] +
			src[2] * modelMatrix[ i + 2 * 4 ] +
			1 * modelMatrix[ i + 3 * 4 ];
	}

	for ( i = 0 ; i < 4 ; i++ ) {
		dst[i] = 
			eye[0] * projectionMatrix[ i + 0 * 4 ] +
			eye[1] * projectionMatrix[ i + 1 * 4 ] +
			eye[2] * projectionMatrix[ i + 2 * 4 ] +
			eye[3] * projectionMatrix[ i + 3 * 4 ];
	}
}
*)

    /// <summary>
    /// R_CullLocalPointAndRadius( vec3_t pt, float radius )
    /// </summary>
    let TransformModelToClip (source: Vector3) (modelMatrix: single[]) (projectionMatrix: single[]) =
        let calculateEye i =
            (source.X * modelMatrix.[i + 0 * 4]) +
            (source.Y * modelMatrix.[i + 1 * 4]) +
            (source.Z * modelMatrix.[i + 2 * 4]) +
            (1.f * modelMatrix.[i + 3 * 4])
          
        let eye =
            Vector4 (
                calculateEye 0,
                calculateEye 1,
                calculateEye 2,
                calculateEye 3
            )

        let calculateDestination i =
            (eye.X * projectionMatrix.[i + 0 * 4]) +
            (eye.Y * projectionMatrix.[i + 1 * 4]) +
            (eye.Z * projectionMatrix.[i + 2 * 4]) +
            (eye.W * projectionMatrix.[i + 3 * 4])

        let destination =
            Vector4 (
                calculateDestination 0,
                calculateDestination 1,
                calculateDestination 2,
                calculateDestination 3
            )

        (eye, destination)

(*
void R_TransformClipToWindow( const vec4_t clip, const viewParms_t *view, vec4_t normalized, vec4_t window ) {
	normalized[0] = clip[0] / clip[3];
	normalized[1] = clip[1] / clip[3];
	normalized[2] = ( clip[2] + clip[3] ) / ( 2 * clip[3] );

	window[0] = 0.5f * ( 1.0f + normalized[0] ) * view->viewportWidth;
	window[1] = 0.5f * ( 1.0f + normalized[1] ) * view->viewportHeight;
	window[2] = normalized[2];

	window[0] = (int) ( window[0] + 0.5 );
	window[1] = (int) ( window[1] + 0.5 );
}
*)
    
    /// <summary>
    /// R_TransformClipToWindow( const vec4_t clip, const viewParms_t *view, vec4_t normalized, vec4_t window )
    /// </summary>
    let TransformClipToWindow (clip: Vector4) (view: ViewParms) =
        let normalized =
            Vector4 (
                clip.X / clip.W,
                clip.Y / clip.W,
                (clip.Z + clip.W) / (2.f * clip.W),
                0.f
            )

        let window =
            Vector4 (
                truncate ((0.5f * (1.0f + normalized.X) * (single view.ViewportWidth)) + 0.5f),
                truncate ((0.5f * (1.0f + normalized.Y) * (single view.ViewportHeight)) + 0.5f),
                normalized.Z,
                0.f
            )

        (normalized, window)

(*
void myGlMultMatrix( const float *a, const float *b, float *out ) {
	int		i, j;

	for ( i = 0 ; i < 4 ; i++ ) {
		for ( j = 0 ; j < 4 ; j++ ) {
			out[ i * 4 + j ] =
				a [ i * 4 + 0 ] * b [ 0 * 4 + j ]
				+ a [ i * 4 + 1 ] * b [ 1 * 4 + j ]
				+ a [ i * 4 + 2 ] * b [ 2 * 4 + j ]
				+ a [ i * 4 + 3 ] * b [ 3 * 4 + j ];
		}
	}
}
*)

    let MyGLMultMatrix (a: Matrix16) (b: Matrix16) =
        a * b