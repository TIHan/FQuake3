(*
Copyright (C) 2013 William F. Smith

This program is free software; you can redistribute it
and/or modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the License,
or (at your option) any later version.

This program is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

Derivative of Quake III Arena source:
Copyright (C) 1999-2005 Id Software, Inc.
*)

(*
    Rules of Thumb:
    1. Types that are 64 bytes or less can be structs.
    2. Types that don't singularly try to represent a value should be records.
*)

#nowarn "9"

namespace Engine

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading
open System.Diagnostics
open System.Diagnostics.Contracts
open Microsoft.FSharp.NativeInterop
open Engine.QMath

/// <summary>
/// Based on Q3: CULL_IN, CULL_CLIP, CULL_OUT
/// ClipType
/// </summary>
type ClipType =
    | In = 0    // completely unclipped
    | Clip = 1  // clipped by one or more planes
    | Out = 2   // completely outside the clipping planes

/// <summary>
/// Based on Q3: PLANE_X, PLANE_Y, PLANE_Z, PLANE_NON_AXIAL
/// PlaneType
///
/// plane types are used to speed some tests
/// 0-2 are axial planes
/// </summary>
type PlaneType =
    | X = 0
    | Y = 1
    | Z = 2
    | NonAxial = 3

/// <summary>
/// Axis
/// </summary>
[<Struct>]
type Axis =
    val X : Vector3
    val Y : Vector3
    val Z : Vector3

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.X
            | 1 -> this.Y
            | 2 -> this.Z
            | _ -> raise <| IndexOutOfRangeException ()

    new (x, y, z) =
        {
            X = x;
            Y = y;
            Z = z;
        }


/// <summary>
/// Rgba
/// </summary>
[<Struct>]
type Rgba =
    val R : byte
    val G : byte
    val B : byte
    val A : byte

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.R
            | 1 -> this.G
            | 2 -> this.B
            | 3 -> this.A
            | _ -> raise <| IndexOutOfRangeException ()

    new (r, g, b, a) =
        {
            R = r;
            G = g;
            B = b;
            A = a;
        }

/// <summary>
/// Based on Q3: orientation_t
/// Orientation
/// </summary>
[<Struct>]
type Orientation =
    val Origin : Vector3
    val Axis : Axis

    new (origin, axis) =
        {
            Origin = origin;
            Axis = axis;
        }

/// <summary>
/// Based on Q3: orientationr_t
/// OrientationR
///
/// Note: Should this be a record type? It is over 64 bytes, don't know for sure.
/// </summary>
[<Struct>]
type OrientationR =
    val Origin : Vector3        // in world coordinates
    val Axis : Axis             // orientation in world
    val ViewOrigin : Vector3    // viewParms->or.origin in local coordinates // FIXME: This directly points to viewParms orientation origin? Yuck.
    val ModelMatrix : Matrix16

    new (origin, axis, viewOrigin, modelMatrix) =
        {
            Origin = origin;
            Axis = axis;
            ViewOrigin = viewOrigin;
            ModelMatrix = modelMatrix;
        }

/// <summary>
/// Based on Q3: cplane_t
/// Plane
/// </summary>
[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = 20)>]
type Plane =
    [<FieldOffset (0)>]
    val Normal : Vector3

    [<FieldOffset (12)>]
    val Distance : single

    [<FieldOffset (16)>]
    [<MarshalAs (UnmanagedType.I8)>]
    val Type : PlaneType    // for fast side tests: 0,1,2 = axial, 3 = nonaxial

    [<FieldOffset (17)>]
    val SignBits : byte     // signx + (signy<<1) + (signz<<2), used as lookup during collision

(*
void SetPlaneSignbits (cplane_t *out) {
	int	bits, j;

	// for fast box on planeside test
	bits = 0;
	for (j=0 ; j<3 ; j++) {
		if (out->normal[j] < 0) {
			bits |= 1<<j;
		}
	}
	out->signbits = bits;
}
*)

    static member inline Zero = Plane (Vector3.Zero, 0.f, PlaneType.X, 0uy)

    /// <summary>
    /// Based on Q3: SetPlaneSignBits
    /// CalculateSignBits
    /// </summary>
    static member inline CalculateSignBits (normal: Vector3) =
        let rec calculatePlaneSignBits bits acc =
            match acc with
            | 3 -> bits
            | _ ->
                calculatePlaneSignBits (match normal.[acc] < 0.f with | true -> bits ||| (1uy <<< acc) | _ -> bits) (acc + 1)

        calculatePlaneSignBits 0uy 0

(*
qboolean PlaneFromPoints( vec4_t plane, const vec3_t a, const vec3_t b, const vec3_t c ) {
	vec3_t	d1, d2;

	VectorSubtract( b, a, d1 );
	VectorSubtract( c, a, d2 );
	CrossProduct( d2, d1, plane );
	if ( VectorNormalize( plane ) == 0 ) {
		return qfalse;
	}

	plane[3] = DotProduct( a, plane );
	return qtrue;
}
*)

    /// <summary>
    /// Based on Q3: PlaneFromPoints
    /// InitFromPoints
    ///
    /// The normal will point out of the clock for clockwise ordered points
    /// </summary>
    static member inline InitFromPoints (a: Vector3) (b: Vector3) (c: Vector3) =
        let d1 = b - a
        let d2 = c - a
        let cross = Vector3.CrossProduct d2 d1
        let normal = Vector3.Normalize cross
        
        match Vector3.Length cross with
        | 0.f -> Plane (normal, 0.f, PlaneType.X, 0uy)
        | _ -> Plane (normal, Vector3.DotProduct a normal, PlaneType.X, 0uy)

    new (normal, distance, typ, signBits) =
        {
            Normal = normal;
            Distance = distance;
            Type = typ;
            SignBits = signBits;
        }

    new (normal, distance, typ) =
        {
            Normal = normal;
            Distance = distance;
            Type = typ;
            SignBits = Plane.CalculateSignBits normal;
        }

/// <summary>
/// Bounds
/// </summary>
[<Struct>]
type Bounds =
    val Alpha : Vector3
    val Omega : Vector3     

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.Alpha
            | 1 -> this.Omega
            | _ -> raise <| IndexOutOfRangeException ()

    new (alpha, omega) =
        {
            Alpha = alpha;
            Omega = omega;
        }

/// <summary>
/// Frustum
///
/// TODO: Find out if this is truly left, right, bottom, and top in this order
/// </summary>
type Frustum =
    {
        Left: Plane;
        Right: Plane;
        Bottom: Plane;
        Top: Plane;
    }

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.Left
            | 1 -> this.Right
            | 2 -> this.Bottom
            | 3 -> this.Top
            | _ -> raise <| IndexOutOfRangeException ()

/// <summary>
/// Transform
///
/// Note: Too big to be a struct?
/// </summary>
[<Struct>]
type Transform =
    val T0 : Vector3
    val T1 : Vector3
    val T2 : Vector3
    val T3 : Vector3
    val T4 : Vector3
    val T5 : Vector3
    val T6 : Vector3
    val T7 : Vector3

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.T0
            | 1 -> this.T1
            | 2 -> this.T2
            | 3 -> this.T3
            | 4 -> this.T4
            | 5 -> this.T5
            | 6 -> this.T6
            | 7 -> this.T7
            | _ -> raise <| IndexOutOfRangeException ()

    new (t0, t1, t2, t3, t4, t5, t6, t7) =
        {
            T0 = t0;
            T1 = t1;
            T2 = t2;
            T3 = t3;
            T4 = t4;
            T5 = t5;
            T6 = t6;
            T7 = t7;
        }

    static member inline Init (f: int -> Vector3) =
        Transform (f 0, f 1, f 2, f 3, f 4, f 5, f 6, f 7)


/// <summary>
/// Based on Q3: viewParms_t
/// ViewParms
/// </summary>
type ViewParms =
    {
        Orientation: OrientationR;
        World: OrientationR;
        PvsOrigin: Vector3;         // may be different than or.origin for portals
        IsPortal: bool;             // true if this view is through a portal
        IsMirror: bool;             // the portal is a mirror, invert the face culling
        FrameSceneId: int;          // copied from tr.frameSceneNum
        FrameCount: int;            // copied from tr.frameCount
        PortalPlane: Plane;         // clip anything behind this if mirroring
        ViewportX: int;
        ViewportY: int;
        ViewportWidth: int;
        ViewportHeight: int;
        FovX: single;
        FovY: single;
        ProjectionMatrix: Matrix16;
        Frustum: Plane[];
        VisibilityBounds: Vector3[];
        ZFar: single;
    }

/// <summary>
/// Based on Q3: refEntityType_t
/// RefEntityType
/// </summary>
type RefEntityType =
    | Model = 0
    | Poly = 1
    | Sprite = 2
    | Beam = 3
    | RailCore = 4
    | RailRings = 5
    | Lightning = 6
    | PortalSurface = 7     // doesn't draw anything, just info for portals
    | MaxRefEntityType = 8

/// <summary>
/// Based on Q3: refEntity_t
/// RefEntity
/// </summary>
type RefEntity =
    {
        Type: RefEntityType;
        RenderFx: int;
        ModelHandle: int;                   // opaque type outside refresh
        LightningOrigin: Vector3;           // so multi-part models can be lit identically (RF_LIGHTING_ORIGIN)
        ShadowPlane: single;                // projection shadows go here, stencils go slightly lower
        Axis: Axis;                         // rotation vectors
        HasNonNormalizedAxes: bool;         // axis are not normalized, i.e. they have scale
        Origin: Vector3;                    // also used as MODEL_BEAM's "from"
        Frame: int;                         // also used as MODEL_BEAM's diameter
        OldOrigin: Vector3;                 // also used as MODEL_BEAM's "to"
        OldFrame: int;
        BackLerp: single;                   // 0.0 = current, 1.0 = old
        SkinId: int;                        // inline skin index
        CustomSkinHandle: int;              // NULL for default skin
        CustomShaderHandle: int;            // use one image for the entire thing
        ShaderRgba: Rgba;                   // colors used by rgbgen entity shaders
        ShaderTextureCoordinate: Vector2;   // texture coordinates used by tcMod entity modifiers
        ShaderTime: single;                 // subtracted from refdef time to control effect start times
        Radius: single;
        Rotation: single;
    }

/// <summary>
/// Based on Q3: trRefEntity_t
/// TrRefEntity
///
/// a trRefEntity_t has all the information passed in by
/// the client game, as well as some locally derived info
/// </summary>
type TrRefEntity =
    {
        Entity: RefEntity;
        AxisLength: single;         // compensate for non-normalized axis
        NeedDlights: bool;          // true for bmodels that touch a dlight
        IsLightingCalculated: bool;
        LightDirection: Vector3;    // normalized direction towards light
        AmbientLight: Vector3;      // color normalized to 0-255
        AmbientLightInt: int;       // 32 bit rgba packed
        DirectedLight: Vector3;
    }

/// <summary>
/// Based on Q3: dlight_t
/// Dlight
/// </summary>
[<Struct>]
type Dlight =
    val Origin : Vector3
    val Color : Vector3         // range from 0.0 to 1.0, should be color normalized
    val Radius : single
    val Transformed : Vector3   // origin in local coordinate system
    val Additive : int          // texture detail is lost tho when the lightmap is dark

    new (origin, color, radius, transformed, additive) =
        {
            Origin = origin;
            Color = color;
            Radius = radius;
            Transformed = transformed;
            Additive = additive;
        }

/// <summary>
/// Based on Q3: drawVert_t
/// DrawVertex
/// </summary>
[<Struct>]
type DrawVertex =
    val Vertex : Vector3
    val St1 : single
    val St2 : single
    val LightMap1 : single
    val LightMap2 : single
    val Normal : Vector3
    val Color : Rgba

    new (vertex, st1, st2, lightMap1, lightMap2, normal, color) =
        {
            Vertex = vertex;
            St1 = st1;
            St2 = st2;
            LightMap1 = lightMap1;
            LightMap2 = lightMap2;
            Normal = normal;
            Color = color;
        }

/// <summary>
/// Based on Q3: polyVert_t
/// PolyVertex
/// </summary>
[<Struct>]
type PolyVertex =
    val Vertex : Vector3
    val St1 : single;
    val St2 : single;
    val Modulate1 : single;
    val Modulate2 : single;
    val Modulate3 : single;
    val Modulate4 : single;

    new (vertex, st1, st2, modulate1, modulate2, modulate3, modulate4) =
        {
            Vertex = vertex;
            St1 = st1;
            St2 = st2;
            Modulate1 = modulate1;
            Modulate2 = modulate2;
            Modulate3 = modulate3;
            Modulate4 = modulate4;
        }

/// <summary>
/// Based on Q3: srfPoly_t
/// SurfacePoly
///
/// when cgame directly specifies a polygon, it becomes a srfPoly_t
/// as soon as it is called
/// </summary>
[<Struct>]
type SurfacePoly =
    val ShaderHandle : int
    val FogIndex : int
    val Vertices : PolyVertex[] // TODO: Change to list.

    new (shaderHandle, fogIndex, vertexCount, vertices) =
        {
            ShaderHandle = shaderHandle;
            FogIndex = fogIndex;
            Vertices = vertices;
        }

/// <summary>
/// Based on Q3: srfDisplayList_t
/// SurfaceDisplayList
/// </summary>
[<Struct>]
type SurfaceDisplayList =
    val ListId : int

    new (listId) =
        {
            ListId = listId;
        }

/// <summary>
/// Based on Q3: srfFlare_t
/// SurfaceFlare
/// </summary>
[<Struct>]
type SurfaceFlare =
    val Origin : Vector3
    val Normal : Vector3
    val Color : Vector3

    new (origin, normal, color) =
        {
            Origin = origin;
            Normal = normal;
            Color = color;
        }

/// <summary>
/// Based on Q3: srfGridMesh_t
/// SurfaceGridMesh
/// </summary>
[<Struct>]
type SurfaceGridMesh =
    val DlightBit1 : int
    val DlightBit2 : int

    // culling information
    val MeshBounds : Bounds
    val LocalOrigin : Vector3
    val MeshRadius : single

    // lod information, which may be different
    // than the culling information to allow for
    // groups of curves that LOD as a unit
    val LodOrigin : Vector3
    val LodRadius : single
    val LodFixed : int
    val LodStitched : int

    val Width : int
    val Height : int
    val WidthLodError : single[] // TODO: Change to list.
    val HeightLodError : single[] // TODO: Change to list.
    val Vertex : DrawVertex         // variable sized

    new (dlightBit1, dlightBit2, meshBounds, localOrigin, meshRadius, lodOrigin, lodRadius, lodFixed, lodStitched, width, height, widthLodError, heightLodError, vertex) =
        {
            DlightBit1 = dlightBit1;
            DlightBit2 = dlightBit2;
            MeshBounds = meshBounds;
            LocalOrigin = localOrigin;
            MeshRadius = meshRadius;
            LodOrigin = lodOrigin;
            LodRadius = lodRadius;
            LodFixed = lodFixed;
            LodStitched = lodStitched;
            Width = width;
            Height = height;
            WidthLodError = widthLodError;
            HeightLodError = heightLodError;
            Vertex = vertex;
        }

/// <summary>
/// FaceVertexPoints
///
/// Note: Should this be a struct?
/// </summary>
[<Struct>]
type FaceVertexPoints =
    val Vertex0 : single
    val Vertex1 : single
    val Vertex2 : single
    val Vertex3 : single
    val Vertex4 : single
    val Vertex5 : single
    val Vertex6 : single
    val Vertex7 : single

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.Vertex0
            | 1 -> this.Vertex1
            | 2 -> this.Vertex2
            | 3 -> this.Vertex3
            | 4 -> this.Vertex4
            | 5 -> this.Vertex5
            | 6 -> this.Vertex6
            | 7 -> this.Vertex7
            | _ -> raise <| IndexOutOfRangeException ()

    new (vertex0, vertex1, vertex2, vertex3, vertex4, vertex5, vertex6, vertex7) =
        {
            Vertex0 = vertex0;
            Vertex1 = vertex1;
            Vertex2 = vertex2;
            Vertex3 = vertex3;
            Vertex4 = vertex4;
            Vertex5 = vertex5;
            Vertex6 = vertex6;
            Vertex7 = vertex7;
        }

/// <summary>
/// Based on Q3: srfSurfaceFace_t
/// SurfaceFace
/// </summary>
[<Struct>]
type SurfaceFace =
    val Plane : Plane
    val DlightBit1 : int
    val DlightBit2 : int

    // triangle definitions (no normals at points)
    val PointCount : int
    val IndexCount : int
    val OfsIndices : int
    val Points : FaceVertexPoints   // variable sized; // there is a variable length list of indices here also

    new (plane, dlightBit1, dlightBit2, pointCount, indexCount, ofsIndices, points) =
        {
            Plane = plane;
            DlightBit1 = dlightBit1;
            DlightBit2 = dlightBit2;
            PointCount = pointCount;
            IndexCount = indexCount;
            OfsIndices = ofsIndices;
            Points = points;
        }

/// <summary>
/// Based on Q3: srfTriangles_t
/// SurfaceTriangles
///
/// // misc_models in maps are turned into direct geometry by q3map
/// </summary>
[<Struct>]
type SurfaceTriangles =
    val DlightBit1 : int
    val DlightBit2 : int

    // culling information (FIXME: use this!)
    val Bounds : Bounds
    val LocalOrigin : Vector3
    val Radius : single

    // triangle definitions
    val Indices : int[] // TODO: Change to list
    val Vertices : DrawVertex[] // TODO: Change to list

    new (dlightBit1, dlightBit2, bounds, localOrigin, radius, indices, vertices) =
        {
            DlightBit1 = dlightBit1;
            DlightBit2 = dlightBit2;
            Bounds = bounds;
            LocalOrigin = localOrigin;
            Radius = radius;
            Indices = indices;
            Vertices = vertices;
        }

/// <summary>
/// Based on Q3: surfaceType_t
/// Surface
///
/// any changes in surfaceType must be mirrored in rb_surfaceTable[] // FIXME: Oh crap..
/// </summary>
type Surface =
    | Bad
    | Skip                              // ignore
    | Face of SurfaceFace
    | Grid of SurfaceGridMesh
    | Triangles of SurfaceTriangles
    | Poly of SurfacePoly
    | Md3
    | Md4
    | Flare of SurfaceFlare
    | Entity                            // beams, rails, lightning, etc that can be determined by entity
    | DisplayList of SurfaceDisplayList

/// <summary>
/// Based on Q3: drawSurf_t
/// DrawSurface
/// </summary>
type DrawSurface =
    {
        Sort: uint32;       // bit combination for fast compares
        Surface : Surface
    }

/// <summary>
/// Based on Q3: RDF_NOWORLDMODEL, RDF_HYPERSPACE
/// RdFlags
/// </summary>
[<Flags>]
type RdFlags =
    | NoWorldModel = 0x1    // used for player configuration screen
    | Hyperspace = 0x4      // teleportation effect

/// <summary>
/// Based on Q3: trRefdef_t
/// TrRefDef
///
/// trRefdef_t holds everything that comes in refdef_t,
/// as well as the locally generated scene information
/// </summary>
type TrRefDef =
    {
        X: int;
        Y: int;
        Width: int;
        Height: int;
        ViewOrigin: Vector3;
        ViewAxis: Axis;             // transformation matrix
        Time: int;                  // time in milliseconds for shader effects and other time dependent rendering issues
        RdFlags: RdFlags;

        // 1 bits will prevent the associated area from rendering at all
        AreaMask: byte[]; // TODO: Remove array.
        HasAreaMaskModified: bool;  // qtrue if areamask changed since last scene

        FloatTime: single;          // tr.refdef.time / 1000.0
        Text: string[]; // TODO: Remove array.
        EntityCount: int;
        Entities: TrRefEntity[]; // // TODO: Remove array. Maybe a list?
        DlightCount: int;
        DLights: Dlight[]; // TODO: Remove array. Maybe a list?
        PolyCount: int;
        Polys:  SurfacePoly[]; // TODO: Remove array. Maybe a list?
        DrawSurfaceCount: int;
        DrawSurfaces: DrawSurface[]; // TODO: Remove array. Maybe a list?
    }


/// <summary>
/// Based on Q3: image_t
/// Image
///
/// TODO: Not finished. Will hold actual OpenGL values.
/// </summary>
type Image =
    {
        Path : string;          // game path, including extension
        Width : int;
        Height : int;

        // after power of two and picmip but not including clamp to MAX_TEXTURE_SIZE
        UploadWidth : int;
        UploadHeight : int

        TextureId : int;        // gl texture binding // TODO: Perhaps we should have GL specific types
        FrameUsed : int;
        InternalFormat : int;
        IsMipmap : bool;
        CanAllowPicmip : bool;
        WrapClampMode: int;     // GL_CLAMP or GL_REPEAT
        
        Next: Image; // Is this a good idea?
    }

/// <summary>
/// Based on Q3: dshader_t
/// DShader
/// </summary>
type DShader =
    {
        Shader: string;
        SurfaceFlags: int;
        ContentFlags: int;
    }

/// <summary>
/// Based on Q3: cullType_t
/// CullType
/// </summary>
type CullType =
    | FrontSided = 0
    | BackSided = 1
    | TwoSided = 2

/// <summary>
/// Based on Q3: fogPass_t
/// FogType
/// </summary>
type FogType =
    | None = 0  // surface is translucent and will just be adjusted properly
    | Equal = 1 // surface is opaque but possibly alpha tested
    | Le = 2    // surface is trnaslucent, but still needs a fog pass (fog surface)

/// <summary>
/// Based on Q3: skyParms_t
/// SkyParms
/// </summary>
type SkyParms =
    {
        CloudHeight: single;
        // TODO:
    }

/// <summary>
/// Based on Q3: fogParms_t
/// FogParms
/// </summary>
type FogParms =
    {
        Color: Vector3;
        DepthForOpaque: single;
    }

/// <summary>
/// Based on Q3: genFunc_t
/// WaveFormType
/// </summary>
type WaveFormType =
    | None = 0
    | Sin = 1
    | Square = 2
    | Triangle = 3
    | Sawtooth = 4
    | InverseSawtooth = 5
    | Noise = 6

/// <summary>
/// Based on Q3: waveForm_t
/// WaveForm
/// </summary>
type WaveForm =
    {
        Type: WaveFormType;
        Base: single;
        Amplitude: single;
        Phase: single;
        Frequency: single;
    }

/// <summary>
/// Based on Q3: deform_t
/// DeformType
/// </summary>
type DeformType =
    | None = 0
    | Wave = 1
    | Normals = 2
    | Bulge = 3
    | Move = 4
    | ProjectionShadow = 5
    | Autosprite = 6
    | Autosprite2 = 7
    | Text0 = 8
    | Text1 = 9
    | Text2 = 10
    | Text3 = 11
    | Text4 = 12
    | Text5 = 13
    | Text6 = 14
    | Text7 = 15

/// <summary>
/// Based on Q3: deformStage_t
/// DeformStage
/// </summary>
type DeformStage =
    {
        Type: DeformType;       // vertex coordinate modification type
        MoveVector: Vector3;
        Wave: WaveForm;
        Spread: single;
        BulgeWidth: single;
        BulgeHeight: single;
        BulgeSpeed: single;
    }

/// <summary>
/// Based on Q3: shader_t
/// Shader
/// </summary>
type Shader =
    {
        Name: string;               // game path, including extension
        LightmapIndex: int;         // for a shader to match, both name and lightmapIndex must match
        Index: int;                 // this shader == tr.shaders[index]
        SortedIndex: int;           // this shader == tr.sortedShaders[sortedIndex]
        Sort: single;               // lower numbered shaders draw before higher numbered

        // we want to return index 0 if the shader failed to
        // load for some reason, but R_FindShader should
        // still keep a name allocated for it, so if
        // something calls RE_RegisterShader again with
        // the same name, we don't try looking for it again
        IsDefaultShader: bool;

        IsExplicitlyDefined: bool;  // found in a .shader file
        SurfaceFlags: int;          // if explicitlyDefined, this will have SURF_* flags
        ContentFlags: int;
        IsEntityMergable: bool;     // merge across entites optimizable (smoke, blood)
        IsSky: bool;
        Sky: SkyParms;
        Fog: FogParms;
        PortalRange: single;        // distance to fog out at
        MultitextureEnv: int;       // 0, GL_MODULATE, GL_ADD (FIXME: put in stage)
        CullType: CullType;         // CT_FRONT_SIDED, CT_BACK_SIDED, or CT_TWO_SIDED
        HasPolygonOffset: bool;     // set for decals and other items that must be offset 
        HasNoMipMaps: bool;         // for console fonts, 2D elements, etc.
        HasNoPicMip: bool;          // for images that must always be full resolution
        FogType: FogType;           // draw a blended pass, possibly with depth test equals
        NeedsNormal: bool;          // not all shaders will need all data to be gathered
        NeedsSt1: bool;
        NeedsSt2: bool;
        NeedsColor: bool;
        Deforms: DeformStage seq;
        // TODO:
    }

/// <summary>
/// Based on Q3: msurface_t
/// DShader
/// </summary>
type MSurface =
    {
        ViewCount: int;
        // TODO:
    }

/// <summary>
/// Based on Q3: bmodel_t
/// BModel
/// </summary>
type BModel =
    {
        Bounds: Bounds;         // for culling
        Surfaces: MSurface seq;
    }

/// <summary>
/// Based on Q3: world_t
/// World
/// </summary>
type World =
    {
        Name: string;           // ie: maps/tim_dm2.bsp
        BaseName: string;       // ie: tim_dm2
        DataSize: int;
        Shaders: DShader seq;
        // TODO:
    }

/// <summary>
/// Based on Q3: trGlobals_t
/// TrGlobals
///
/// Most renderer globals are defined here.
/// backend functions should never modify any of these fields,
/// but may read fields that aren't dynamically modified
/// by the frontend.
/// TODO: Not finished.
/// </summary>
type TrGlobals =
    {
        IsRegistered : bool;            // cleared at shutdown, set at beginRegistration
        VisCount : int;                 // incremented every time a new vis cluster is entered
        FrameCount : int;               // incremented every frame
        SceneCount : int;               // incremented every scene
        ViewCount : int;                // incremented every view (twice a scene if portaled) and every R_MarkFragments call
        SmpFrame : int;                 // toggles from 0 to 1 every endFrame
        FrameSceneId : int;             // zeroed at RE_BeginFrame
        HasWorldMapLoaded : bool;
        //world_t *world;
        //const byte externalVisData - NOT SET/USED // from RE_SetWorldVisData, shared with CM_Load
        //TODO:
    }