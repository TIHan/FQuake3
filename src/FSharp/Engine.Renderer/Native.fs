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

// Disable native interop warnings
#nowarn "9"
#nowarn "51"

namespace Engine.Renderer.Native

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading
open System.Diagnostics
open System.Diagnostics.Contracts
open Microsoft.FSharp.NativeInterop
open Engine.Core
open Engine.Math
open Engine.NativeInterop
open Engine.Native
open Engine.Renderer

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type cplane_t =
    val mutable normal : vec3_t
    val mutable dist : single
    val mutable type' : byte
    val mutable signbits : byte
    val mutable pad : byte
    val private pad1 : byte

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type orientation_t =
    val mutable origin : vec3_t
    val mutable axis : vec3_t
    val private axis1 : vec3_t
    val private axis2 : vec3_t

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type orientationr_t =
    val mutable origin : vec3_t
    val mutable axis : vec3_t
    val private axis1 : vec3_t
    val private axis2 : vec3_t
    val mutable viewOrigin : vec3_t
    val mutable modelMatrix : single
    val private modelMatrix1 : single
    val private modelMatrix2 : single
    val private modelMatrix3 : single
    val private modelMatrix4 : single
    val private modelMatrix5 : single
    val private modelMatrix6 : single
    val private modelMatrix7 : single
    val private modelMatrix8 : single
    val private modelMatrix9 : single
    val private modelMatrix10 : single
    val private modelMatrix11 : single
    val private modelMatrix12 : single
    val private modelMatrix13 : single
    val private modelMatrix14 : single
    val private modelMatrix15 : single

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type viewParms_t =
    val mutable or' : orientationr_t
    val mutable world : orientationr_t
    val mutable pvsOrigin : vec3_t
    val mutable isPortal : qboolean
    val mutable isMirror : qboolean
    val mutable frameSceneNum : int
    val mutable frameCount : int
    val mutable portalPlane : cplane_t
    val mutable viewportX : int
    val mutable viewportY : int
    val mutable viewportWidth : int
    val mutable viewportHeight : int
    val mutable fovX : single
    val mutable fovY : single
    val mutable projectionMatrix : single
    val private projectionMatrix1 : single
    val private projectionMatrix2 : single
    val private projectionMatrix3 : single
    val private projectionMatrix4 : single
    val private projectionMatrix5 : single
    val private projectionMatrix6 : single
    val private projectionMatrix7 : single
    val private projectionMatrix8 : single
    val private projectionMatrix9 : single
    val private projectionMatrix10 : single
    val private projectionMatrix11 : single
    val private projectionMatrix12 : single
    val private projectionMatrix13 : single
    val private projectionMatrix14 : single
    val private projectionMatrix15 : single
    val mutable frustum : cplane_t
    val private frustum1 : cplane_t
    val private frustum2 : cplane_t
    val private frustum3 : cplane_t
    val mutable visBounds : vec3_t
    val private visBounds1 : vec3_t
    val mutable zFar : single
    
type surfaceType_t =
    | SF_BAD = 0
    | SF_SKIP = 1
    | SF_FACE = 2
    | SF_GRID = 3
    | SF_TRIANGLES = 4
    | SF_POLY = 5
    | SF_MD3 = 6
    | SF_MD4 = 7
    | SF_FLARE = 8
    | SF_ENTITY = 9
    | SF_DISPLAY_LIST = 10
    | SF_NUM_SURFACE_TYPES = 11
    | SF_MAX = 0x7fffffff

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type drawVert_t =
    val mutable xyz : vec3_t
    val mutable st : single
    val private st1 : single
    val mutable lightmap : single
    val mutable lightmap1 : single
    val mutable normal : vec3_t
    val mutable color : byte
    val private color1 : byte
    val private color2 : byte
    val private color3 : byte

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type polyVert_t =
    val mutable xyz : vec3_t
    val mutable st : single
    val private st1 : single
    val mutable modulate : byte
    val private modulate1 : byte
    val private modulate2 : byte
    val private modulate3 : byte

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type poly_t =
    val mutable hShader : qhandle_t
    val mutable numVerts : int
    val mutable verts : nativeptr<polyVert_t>

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type srfPoly_t =
    val mutable surfaceType : surfaceType_t
    val mutable hShader : qhandle_t
    val mutable fogIndex : int
    val mutable numVerts : int
    val mutable verts : nativeptr<polyVert_t>

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type srfDisplayList_t =
    val mutable surfaceType : surfaceType_t
    val mutable listNum : int

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type srfGridMesh_t =
    val mutable surfaceType : surfaceType_t
    val mutable dlightBits : int
    val private dlightBits1 : int
    val mutable meshBounds : vec3_t
    val private meshBounds1 : vec3_t
    val mutable localOrigin : vec3_t
    val mutable meshRadius : single
    val mutable lodOrigin : vec3_t
    val mutable lodRadius : single
    val mutable lodFixed : int
    val mutable lodStitched : int
    val mutable width : int
    val mutable height : int
    val mutable widthLodError : nativeptr<single>
    val mutable heightLodError : nativeptr<single>
    val mutable verts : drawVert_t

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type srfFlare_t =
    val mutable surfaceType : surfaceType_t
    val mutable origin : vec3_t
    val mutable normal : vec3_t
    val mutable color : vec3_t

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type srfSurfaceFace_t =
    val mutable surfaceType : surfaceType_t
    val mutable plane : cplane_t
    val mutable dlightBits : int
    val private dlightBits1 : int
    val mutable numPoints : int
    val mutable numIndices : int
    val mutable ofsIndices : int
    val mutable points : single
    val private points1 : single
    val private points2 : single
    val private points3 : single
    val private points4 : single
    val private points5 : single
    val private points6 : single
    val private points7 : single
        
[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type srfTriangles_t =
    val mutable surfaceType : surfaceType_t
    val mutable dlightBits : int
    val private dlightBits1 : int
    val mutable bounds : vec3_t
    val private bounds1 : vec3_t
    val mutable localOrigin : vec3_t
    val mutable radius : single
    val mutable numIndexes : int
    val mutable indexes : nativeptr<int>
    val mutable numVerts : int
    val mutable verts : nativeptr<drawVert_t>

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type drawSurf_t =
    val mutable sort : uint32
    val mutable surface : nativeptr<surfaceType_t>

type refEntityType_t =
    | RT_MODEL = 0
    | RT_POLY = 1
    | RT_SPRITE = 2
    | RT_BEAM = 3
    | RT_RAIL_CORE = 4
    | RT_RAIL_RINGS = 5
    | RT_LIGHTNING = 6
    | RT_PORTALSURFACE = 7
    | RT_MAX_REF_ENTITY_TYPE = 8

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type refEntity_t =
    val mutable reType : refEntityType_t
    val mutable renderfx : int
    val mutable hModel : qhandle_t
    val mutable lightingOrigin : vec3_t
    val mutable shadowPlane : single
    val mutable axis : vec3_t
    val private axis1 : vec3_t
    val private axis2 : vec3_t
    val mutable nonNormalizedAxes : qboolean
    val mutable origin : single
    val private origin1 : single
    val private origin2 : single
    val mutable frame : int
    val mutable oldorigin : single
    val private oldorigin1 : single
    val private oldorigin2 : single
    val mutable oldframe : int
    val mutable backlerp : single
    val mutable skinNum : int
    val mutable customSkin : qhandle_t
    val mutable customShader : qhandle_t
    val mutable shaderRGBA : byte
    val private shaderRGBA1 : byte
    val private shaderRGBA2 : byte
    val private shaderRGBA3 : byte
    val mutable shaderTexCoord : single
    val private shaderTexCoord1 : single
    val mutable shaderTime : single
    val mutable radius : single
    val mutable rotation : single

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type trRefEntity_t =
    val mutable e : refEntity_t
    val mutable axisLength : single
    val mutable needDlights : qboolean
    val mutable lightingCalculated : qboolean
    val mutable lightDir : vec3_t
    val mutable ambientLight : vec3_t
    val mutable ambientLightInt : int
    val mutable directedLight : vec3_t

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type world_t =
    val mutable name : char
    // TODO:

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type image_t =
    val mutable imgName : char
    // TODO:

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type shader_t =
    val mutable shaderName : char
    // TODO:


[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type refdef_t =
    val mutable x : int
    val mutable y : int
    val mutable width : int
    val mutable height : int
    val mutable vieworg : vec3_t
    val mutable viewaxis : vec3_t
    val private viewaxis1 : vec3_t
    val private viewaxis2 : vec3_t
    val mutable time : int;
    val mutable rdflags : int;
    // TODO:


[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type trGlobals_t =
    val mutable registered : qboolean
    val mutable visCount : int
    val mutable frameCount : int
    val mutable sceneCount : int
    val mutable viewCount : int
    val mutable smpFrame : int
    val mutable frameSceneNum : int
    val mutable worldMapLoaded : qboolean
    val mutable world : nativeptr<world_t>
    val mutable externalVisData : nativeptr<byte>
    val mutable defaultImage : nativeptr<image_t>
    val mutable scratchImage : nativeptr<image_t>
    val private scratchImage1 : nativeptr<image_t>
    val private scratchImage2 : nativeptr<image_t>
    val private scratchImage3 : nativeptr<image_t>
    val private scratchImage4 : nativeptr<image_t>
    val private scratchImage5 : nativeptr<image_t>
    val private scratchImage6 : nativeptr<image_t>
    val private scratchImage7 : nativeptr<image_t>
    val private scratchImage8 : nativeptr<image_t>
    val private scratchImage9 : nativeptr<image_t>
    val private scratchImage10 : nativeptr<image_t>
    val private scratchImage11 : nativeptr<image_t>
    val private scratchImage12 : nativeptr<image_t>
    val private scratchImage13 : nativeptr<image_t>
    val private scratchImage14 : nativeptr<image_t>
    val private scratchImage15 : nativeptr<image_t>
    val private scratchImage16 : nativeptr<image_t>
    val private scratchImage17 : nativeptr<image_t>
    val private scratchImage18 : nativeptr<image_t>
    val private scratchImage19 : nativeptr<image_t>
    val private scratchImage20 : nativeptr<image_t>
    val private scratchImage21 : nativeptr<image_t>
    val private scratchImage22 : nativeptr<image_t>
    val private scratchImage23 : nativeptr<image_t>
    val private scratchImage24 : nativeptr<image_t>
    val private scratchImage25 : nativeptr<image_t>
    val private scratchImage26 : nativeptr<image_t>
    val private scratchImage27 : nativeptr<image_t>
    val private scratchImage28 : nativeptr<image_t>
    val private scratchImage29 : nativeptr<image_t>
    val private scratchImage30 : nativeptr<image_t>
    val private scratchImage31 : nativeptr<image_t>
    val mutable fogImage : nativeptr<image_t>
    val mutable dlightImage : nativeptr<image_t>
    val mutable flareImage : nativeptr<image_t>
    val mutable whiteImage : nativeptr<image_t>
    val mutable identityLightImage : nativeptr<image_t>
    // TODO:

(*
=======================================================================================================================
Mappings
=======================================================================================================================
*)

module Orientation =
    let inline ofNative (native: orientation_t) =
        Orientation (
            NativePtr.toStructure &&native.origin,
            NativePtr.toStructure &&native.axis
        )

module OrientationR =
    let inline ofNative (native: orientationr_t) =
        OrientationR (
            NativePtr.toStructure &&native.origin,
            NativePtr.toStructure &&native.axis,
            NativePtr.toStructure &&native.viewOrigin,
            NativePtr.toStructure &&native.modelMatrix
        )

module Plane =
    let inline ofNative (native: cplane_t) =
        Plane (
            NativePtr.toStructure &&native.normal,
            native.dist,
            NativePtr.toStructure &&native.type',
            native.signbits
        )

    let inline toNative (plane: Plane) =
        let mutable native = cplane_t ()
        NativePtr.ofStructure plane.Normal &&native.normal
        native.dist <- plane.Distance
        NativePtr.ofStructure plane.Type &&native.type'
        native.signbits <- plane.SignBits
        native

module Frustum =
    let inline ofNativePtr (nativePtr: nativeptr<cplane_t>) =
        {
            Left = Plane.ofNative <| NativePtr.get nativePtr 0;
            Right = Plane.ofNative <| NativePtr.get nativePtr 1;
            Bottom = Plane.ofNative <| NativePtr.get nativePtr 2;
            Top = Plane.ofNative <| NativePtr.get nativePtr 3;
        }

    let inline toNativePtr (frustum: Frustum) (nativePtr: nativeptr<cplane_t>) =
        NativePtr.set nativePtr 0 <| Plane.toNative frustum.Left
        NativePtr.set nativePtr 1 <| Plane.toNative frustum.Right
        NativePtr.set nativePtr 2 <| Plane.toNative frustum.Bottom
        NativePtr.set nativePtr 3 <| Plane.toNative frustum.Top
        nativePtr

module ViewParms =
    let inline ofNative (native: viewParms_t) =
        {
            Orientation = OrientationR.ofNative native.or';
            World = OrientationR.ofNative native.world;
            PvsOrigin = NativePtr.toStructure &&native.pvsOrigin;
            IsPortal = Convert.ToBoolean native.isPortal;
            IsMirror = Convert.ToBoolean native.isMirror;
            FrameSceneId = native.frameSceneNum;
            FrameCount = native.frameCount;
            PortalPlane = Plane.ofNative native.portalPlane;
            ViewportX = native.viewportX;
            ViewportY = native.viewportY;
            ViewportWidth = native.viewportWidth;
            ViewportHeight = native.viewportHeight;
            FovX = native.fovX;
            FovY = native.fovY;
            ProjectionMatrix = NativePtr.toStructure &&native.projectionMatrix;
            Frustum = Frustum.ofNativePtr &&native.frustum;
            VisibilityBounds = NativePtr.toStructure &&native.visBounds;
            ZFar = native.zFar;
        }

module DrawVertex =
    let inline ofNative (native: drawVert_t) =
        DrawVertex (
            NativePtr.toStructure &&native.xyz,
            NativePtr.get &&native.st 0,
            NativePtr.get &&native.st 1,
            NativePtr.get &&native.lightmap 0,
            NativePtr.get &&native.lightmap 1,
            NativePtr.toStructure &&native.normal,
            NativePtr.toStructure &&native.color
        )

module PolyVertex =
    let inline ofNative (native: polyVert_t) =
        PolyVertex (
            NativePtr.toStructure &&native.xyz,
            NativePtr.get &&native.st 0,
            NativePtr.get &&native.st 1,
            NativePtr.get &&native.modulate 0,
            NativePtr.get &&native.modulate 1,
            NativePtr.get &&native.modulate 2,
            NativePtr.get &&native.modulate 3
        )

module Surface =
    let inline ofNativeFace (native: srfSurfaceFace_t) =
        SurfaceFace (
            Plane.ofNative native.plane,
            NativePtr.get &&native.dlightBits 0,
            NativePtr.get &&native.dlightBits 1,
            native.numPoints,
            native.numIndices,
            native.ofsIndices,
            NativePtr.toStructure &&native.points
        )
        |> Face

    let inline ofNativeTriangles (native: srfTriangles_t) =
        SurfaceTriangles (
            NativePtr.get &&native.dlightBits 0,
            NativePtr.get &&native.dlightBits 1,
            NativePtr.toStructure &&native.bounds,
            NativePtr.toStructure &&native.localOrigin,
            native.radius,
            List.ofNativePtrArray native.numIndexes native.indexes,
            List.ofNativePtrArrayMap native.numVerts DrawVertex.ofNative native.verts
        )
        |> Triangles

    let inline ofNativePoly (native: srfPoly_t) =
        SurfacePoly (
            native.hShader,
            native.fogIndex,
            List.ofNativePtrArrayMap native.numVerts PolyVertex.ofNative native.verts
        )
        |> Poly

    let inline ofNativeDisplayList (native: srfDisplayList_t) =
        SurfaceDisplayList (
            native.listNum
        )
        |> DisplayList

    let inline ofNativeGridMesh (native: srfGridMesh_t) =
        SurfaceGridMesh (
            NativePtr.get &&native.dlightBits 0,
            NativePtr.get &&native.dlightBits 1,
            NativePtr.toStructure &&native.meshBounds,
            NativePtr.toStructure &&native.localOrigin,
            native.meshRadius,
            NativePtr.toStructure &&native.lodOrigin,
            native.lodRadius,
            native.lodFixed,
            native.lodStitched,
            native.width,
            native.height,
            List.ofNativePtrArray native.width native.widthLodError,
            List.ofNativePtrArray native.height native.heightLodError,
            NativePtr.toStructure &&native.verts
        )
        |> Grid

    let inline ofNativeFlare (native: srfFlare_t) =
        SurfaceFlare (
            NativePtr.toStructure &&native.origin,
            NativePtr.toStructure &&native.normal,
            NativePtr.toStructure &&native.color
        )
        |> Flare

    let inline ofNativePtr (nativePtr: nativeptr<surfaceType_t>) =
        let type' = NativePtr.read nativePtr
        match type' with
        | surfaceType_t.SF_BAD -> Bad
        | surfaceType_t.SF_SKIP -> Skip
        | surfaceType_t.SF_FACE ->
            let nativePtr = NativePtr.ofNativeInt<srfSurfaceFace_t> <| NativePtr.toNativeInt nativePtr
            let native = NativePtr.read nativePtr
            ofNativeFace native            
        | surfaceType_t.SF_GRID ->
            let nativePtr = NativePtr.ofNativeInt<srfGridMesh_t> <| NativePtr.toNativeInt nativePtr
            let native = NativePtr.read nativePtr
            ofNativeGridMesh native
        | surfaceType_t.SF_TRIANGLES ->
            let nativePtr = NativePtr.ofNativeInt<srfTriangles_t> <| NativePtr.toNativeInt nativePtr
            let native = NativePtr.read nativePtr
            ofNativeTriangles native
        | surfaceType_t.SF_POLY ->
            let nativePtr = NativePtr.ofNativeInt<srfPoly_t> <| NativePtr.toNativeInt nativePtr
            let native = NativePtr.read nativePtr
            ofNativePoly native
        | surfaceType_t.SF_MD3 -> Md3
        | surfaceType_t.SF_MD4 -> Md4
        | surfaceType_t.SF_FLARE ->
            let nativePtr = NativePtr.ofNativeInt<srfFlare_t> <| NativePtr.toNativeInt nativePtr
            let native = NativePtr.read nativePtr
            ofNativeFlare native 
        | surfaceType_t.SF_ENTITY -> Entity
        | surfaceType_t.SF_DISPLAY_LIST ->
            let nativePtr = NativePtr.ofNativeInt<srfDisplayList_t> <| NativePtr.toNativeInt nativePtr
            let native = NativePtr.read nativePtr
            ofNativeDisplayList native
        | _ -> raise <| Exception "Invalid Surface Type"          

module DrawSurface =
    let inline ofNative (native: drawSurf_t) =
        {
            Sort = native.sort;
            Surface = Surface.ofNativePtr native.surface;
        }

module RefEntity =
    let inline ofNative (native: refEntity_t) =
        {
            RefEntity.Type = enum<RefEntityType> (int native.reType);
            RenderFx = native.renderfx;
            ModelHandle = native.hModel;
            LightingOrigin = NativePtr.toStructure &&native.lightingOrigin;
            ShadowPlane = native.shadowPlane;
            Axis = NativePtr.toStructure &&native.axis;
            HasNonNormalizedAxes = Convert.ToBoolean native.nonNormalizedAxes;
            Origin = NativePtr.toStructure &&native.origin;
            Frame = native.frame;
            OldOrigin = NativePtr.toStructure &&native.oldorigin;
            OldFrame = native.oldframe;
            BackLerp = native.backlerp;
            SkinId = native.skinNum;
            CustomSkinHandle = native.customSkin;
            CustomShaderHandle = native.customShader;
            ShaderRgba = NativePtr.toStructure &&native.shaderRGBA;
            ShaderTextureCoordinate = NativePtr.toStructure &&native.shaderTexCoord;
            ShaderTime = native.shaderTime;
            Radius = native.radius;
            Rotation = native.rotation;
        }

module TrRefEntity =
    let inline ofNative (native: trRefEntity_t) =
        {
            Entity = RefEntity.ofNative native.e;
            AxisLength = native.axisLength;
            NeedDlights = Convert.ToBoolean native.needDlights;
            IsLightingCalculated = Convert.ToBoolean native.lightingCalculated;
            LightDirection = NativePtr.toStructure &&native.lightDir;
            AmbientLight = NativePtr.toStructure &&native.ambientLight;
            AmbientLightInt = native.ambientLightInt;
            DirectedLight = NativePtr.toStructure &&native.directedLight;
        }

module RefDef =
    let inline ofNative (native: refdef_t) =
        {
            X = native.x;
            Y = native.y;
            Width = native.width;
            Height = native.height;
            ViewOrigin = NativePtr.toStructure &&native.vieworg;
            ViewAxis = NativePtr.toStructure &&native.viewaxis;
            Time = native.time;
            RdFlags = enum<RdFlags> (native.rdflags)
        }