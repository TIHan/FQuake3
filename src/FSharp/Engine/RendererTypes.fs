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
open Microsoft.FSharp.NativeInterop
open Engine.QMath

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

    new (x, y, z) = { X = x; Y = y; Z = z; }

[<Struct>]
type Rgba =
    val R : byte
    val G : byte
    val B : byte
    val A : byte

// Should this be a record type? It is over 64 bytes, don't know for sure.
[<Struct>]
type Orientation =
    val Origin : Vector3
    val Axis : Axis
    val ViewOrigin : Vector3
    val ModelMatrix : Matrix16

    new (origin, axis, viewOrigin, modelMatrix) =
        {
            Origin = origin;
            Axis = axis;
            ViewOrigin = viewOrigin;
            ModelMatrix = modelMatrix;
        }


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

    /// <summary>
    /// void SetPlaneSignbits (cplane_t *out)
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
    /// PlaneFromPoints( vec4_t plane, const vec3_t a, const vec3_t b, const vec3_t c ) {
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

[<Struct>]
type Bounds =
    val Begin : Vector3
    val End : Vector3     

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.Begin
            | 1 -> this.End
            | _ -> raise <| IndexOutOfRangeException ()

// TODO: Find out if this is truly left, right, bottom, and top in this order
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

// Too big to be a struct?
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


// This is way too big to be a struct, makes sense for it to be a record.
type ViewParms =
    {
        Orientation: Orientation;
        World: Orientation;
        PvsOrigin: Vector3;
        IsPortal: bool;
        IsMirror: bool;
        FrameSceneId: int;
        FrameCount: int;
        PortalPlane: Plane;
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

type RefEntityType =
    | Model = 0
    | Poly = 1
    | Sprite = 2
    | Beam = 3
    | RailCore = 4
    | RailRings = 5
    | Lightning = 6
    | PortalSurface = 7 // doesn't draw anything, just info for portals
    | MaxRefEntityType = 8

type RefEntity =
    {
        Type: RefEntityType;
        RenderFx: int;
        ModelHandle: int;
        LightningOrigin: Vector3;
        Axis: Axis;
        HasNonNormalizedAxes: bool;
        Origin: Vector3;
        Frame: int;
        OldOrigin: Vector3;
        OldFrame: int;
        BackLerp: single;
        SkinId: int;
        CustomSkinHandle: int;
        ShaderRgba: Rgba;
        ShaderTextureCoordinate: Vector2;
        ShaderTime: single;
        Radius: single;
        Rotation: single;
    }

type TrRefEntity =
    {
        Entity: RefEntity;
        AxisLength: single;
        NeedDlights: bool;
        IsLightingCalculated: bool;
        LightDirection: Vector3;
        AmbientLight: Vector3;
        AmbientLightInt: int;
        DirectedLight: Vector3;
    }

[<Struct>]
type Dlight =
    val Origin : Vector3
    val Color : Vector3
    val Radius : single
    val Transformed : Vector3
    val Additive : int

[<Struct>]
type DrawVertex =
    val Vertex : Vector3
    val St1 : single
    val St2 : single
    val LightMap1 : single
    val LightMap2 : single
    val Normal : Vector3
    val Color : Rgba

[<Struct>]
type PolyVertex =
    val Vertex : Vector3
    val St1 : single;
    val St2 : single;
    val Modulate1 : single;
    val Modulate2 : single;
    val Modulate3 : single;
    val Modulate4 : single;

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

[<Struct>]
type SurfaceDisplayList =
    val ListId : int

    new (listId) =
        {
            ListId = listId;
        }

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

[<Struct>]
type SurfaceGridMesh =
    val DlightBit1 : int
    val DlightBit2 : int
    val MeshBounds : Bounds
    val LocalOrigin : Vector3
    val MeshRadius : single
    val LodOrigin : Vector3
    val LodRadius : single
    val LodFixed : int
    val LodStitched : int
    val Width : int
    val Height : int
    val WidthLodError : single[] // TODO: Change to list.
    val HeightLodError : single[] // TODO: Change to list.
    val Vertex : DrawVertex

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

[<Struct>]
type SurfaceFace =
    val Plane : Plane
    val DlightBit1 : int
    val DlightBit2 : int
    val PointCount : int
    val IndexCount : int
    val OfsIndices : int
    val Points : FaceVertexPoints

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

[<Struct>]
type SurfaceTriangles =
    val DlightBit1 : int
    val DlightBit2 : int
    val Bounds : Bounds
    val LocalOrigin : Vector3
    val Radius : single
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

type Surface =
    | Bad
    | Skip
    | Face of SurfaceFace
    | Grid of SurfaceGridMesh
    | Triangles of SurfaceTriangles
    | Poly of SurfacePoly
    | Md3
    | Md4
    | Flare of SurfaceFlare
    | Entity
    | DisplayList of SurfaceDisplayList

// TODO:
[<Struct>]
type DrawSurface =
    val Sort : uint32
    val Surface : Surface

[<Flags>]
type RdFlags =
    | NoWorldModel = 0x1
    | Hyperspace = 0x4

// TODO:
type RefDef =
    {
        X: int;
        Y: int;
        Width: int;
        Height: int;
        ViewOrigin: Vector3;
        ViewAxis: Axis;
        Time: int;
        RdFlags: RdFlags;
        AreaMask: byte[];
        HasAreaMaskModified: bool;
        FloatTime: single;
        Text: string[];
        EntityCount: int;
        Entities: TrRefEntity[];
        DlightCount: int;
        DLights: Dlight[];
        PolyCount: int;
        Polys:  SurfacePoly[];
        DrawSurfaceCount: int;
        DrawSurfaces: DrawSurface[];
    }



type Image =
    {
        Path : string;
        Width : int;
        Height : int;
        UploadWidth : int;
        UploadHeight : int
        TextureId : int;
        FrameUsed : int;
        InternalFormat : int;
        IsMipmap : bool;
        CanAllowPicmip : bool;
        //TODO:
    }

// C equivalent - trGlobals_t
type TrState =
    {
        IsRegistered : bool;
        VisCount : int;
        FrameCount : int;
        SceneCount : int;
        ViewCount : int;
        SmpFrame : int;
        FrameSceneId : int;
        HasWorldMapLoaded : bool;
        //world_t *world;
        //const byte externalVisData - NOT SET/USED

    }