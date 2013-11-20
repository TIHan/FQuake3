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

namespace Engine.Renderer.Native

// Disable native interop warnings
#nowarn "9"
#nowarn "51"

open System.Runtime.InteropServices
open Engine.Native

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type glstate_t =
    val mutable currenttextures : int
    val private currenttextures1 : int
    val mutable currenttmu : int
    val mutable finishedCalled : qboolean
    val mutable texEnv : int
    val private texEnv1 : int
    val mutable faceCulling : int
    val mutable glStateBits : uint32

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
    val mutable origin : vec3_t
    val mutable frame : int
    val mutable oldorigin : vec3_t
    val mutable oldframe : int
    val mutable backlerp : single
    val mutable skinNum : int
    val mutable customSkin : qhandle_t
    val mutable customShader : qhandle_t
    val mutable shaderRGBA : byte
    val private shaderRGBA1 : byte
    val private shaderRGBA2 : byte
    val private shaderRGBA3 : byte
    val mutable shaderTexCoord : vec2_t
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
[<StructLayout (LayoutKind.Explicit, Size = 256)>]
type refdef_t_text =
    [<FieldOffset (0)>]
    val private value : sbyte

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type refdef_t =
    val mutable x : int
    val mutable y : int
    val mutable width : int
    val mutable height : int
    val mutable fov_x : single
    val mutable fov_y : single
    val mutable vieworg : vec3_t
    val mutable viewaxis : vec3_t
    val private viewaxis1 : vec3_t
    val private viewaxis2 : vec3_t
    val mutable time : int;
    val mutable rdflags : int;

    val mutable areamask : byte
    val private areamask1 : byte
    val private areamask2 : byte
    val private areamask3 : byte
    val private areamask4 : byte
    val private areamask5 : byte
    val private areamask6 : byte
    val private areamask7 : byte
    val private areamask8 : byte
    val private areamask9 : byte
    val private areamask10 : byte
    val private areamask11 : byte
    val private areamask12 : byte
    val private areamask13 : byte
    val private areamask14 : byte
    val private areamask15 : byte
    val private areamask16 : byte
    val private areamask17 : byte
    val private areamask18 : byte
    val private areamask19 : byte
    val private areamask20 : byte
    val private areamask21 : byte
    val private areamask22 : byte
    val private areamask23 : byte
    val private areamask24 : byte
    val private areamask25 : byte
    val private areamask26 : byte
    val private areamask27 : byte
    val private areamask28 : byte
    val private areamask29 : byte
    val private areamask30 : byte
    val private areamask31 : byte

    val mutable text : refdef_t_text

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type dlight_t =
    val mutable origin : vec3_t
    val mutable color : vec3_t
    val mutable radius : single
    val mutable transformed : vec3_t
    val mutable additive : int

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type model_t =
    val mutable name : sbyte
    // TODO:

[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = 256)>]
type trRefdef_t_text =
    [<FieldOffset (0)>]
    val mutable text : sbyte

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type trRefdef_t =
    val mutable x : int
    val mutable y : int
    val mutable width : int
    val mutable height : int
    val mutable fov_x : single
    val mutable fov_y : single
    val mutable vieworg : vec3_t
    val mutable viewaxis : vec3_t
    val private viewaxis1 : vec3_t
    val private viewaxis2 : vec3_t
    val mutable time : int
    val mutable rdflags : int

    val mutable areamask : byte
    val private areamask1 : byte
    val private areamask2 : byte
    val private areamask3 : byte
    val private areamask4 : byte
    val private areamask5 : byte
    val private areamask6 : byte
    val private areamask7 : byte
    val private areamask8 : byte
    val private areamask9 : byte
    val private areamask10 : byte
    val private areamask11 : byte
    val private areamask12 : byte
    val private areamask13 : byte
    val private areamask14 : byte
    val private areamask15 : byte
    val private areamask16 : byte
    val private areamask17 : byte
    val private areamask18 : byte
    val private areamask19 : byte
    val private areamask20 : byte
    val private areamask21 : byte
    val private areamask22 : byte
    val private areamask23 : byte
    val private areamask24 : byte
    val private areamask25 : byte
    val private areamask26 : byte
    val private areamask27 : byte
    val private areamask28 : byte
    val private areamask29 : byte
    val private areamask30 : byte
    val private areamask31 : byte

    val mutable areamaskModified : bool
    val mutable floatTime : single
    val mutable text : trRefdef_t_text
    val mutable num_entities : int
    val mutable enities : nativeptr<trRefEntity_t>
    val mutable num_delights : int
    val mutable dlights : nativeptr<dlight_t>
    val mutable numPolys : int
    val mutable polys : nativeptr<srfPoly_t>
    val mutable numDrawSurfs : int
    val mutable drawSurfs : nativeptr<drawSurf_t>

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type frontEndCounters_t =
    val mutable c_sphere_cull_patch_in : int
    val mutable c_sphere_cull_patch_clip : int
    val mutable c_sphere_cull_patch_out : int
    val mutable c_box_cull_patch_in : int
    val mutable c_box_cull_patch_clip : int
    val mutable c_box_cull_patch_out : int
    val mutable c_sphere_cull_md3_in : int
    val mutable c_sphere_cull_md3_clip : int
    val mutable c_sphere_cull_md3_out : int
    val mutable c_box_cull_md3_in : int
    val mutable c_box_cull_md3_clip : int
    val mutable c_box_cull_md3_out : int
    val mutable c_leafs : int
    val mutable c_dlightSurfaces : int
    val mutable c_dlightSurfacesCulled : int

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type backEndCounters_t =
    val mutable c_surfaces : int
    val mutable c_shaders : int
    val mutable c_vertexes : int
    val mutable c_indexes : int
    val mutable c_overDraw : single
    val mutable c_totalIndexes : int
    val mutable c_dlightVertexes : int
    val mutable c_dlightIndexes : int
    val mutable c_flareAdds : int
    val mutable c_flareTests : int
    val mutable c_flareRenders : int
    val mutable msec : int

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type backEndState_t =
    val mutable smpFrame : int
    val mutable refdef : trRefdef_t
    val mutable viewParms : viewParms_t
    val mutable or' : orientationr_t
    val mutable pc : backEndCounters_t
    val mutable isHyperspace : qboolean
    val mutable currentEntity : nativeptr<trRefEntity_t>
    val mutable skyRenderedThisView : qboolean
    val mutable projection2D : qboolean
    val mutable color2D : byte
    val private color2D1 : byte
    val private color2D2 : byte
    val private color2D3 : byte
    val mutable vertexes2D : qboolean
    val mutable entity2D : trRefEntity_t

[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = 64)>]
type skinSurface_t_name =
    [<FieldOffset (0)>]
    val mutable name : sbyte

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type skinSurface_t =
    val mutable name : skinSurface_t_name
    val mutable shader : nativeptr<shader_t>

[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = 64)>]
type skin_t_name =
    [<FieldOffset (0)>]
    val mutable name : sbyte

[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = 128)>]
type skin_t_surfaces =
    [<FieldOffset (0)>]
    val mutable surfaces : nativeptr<skinSurface_t>

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type skin_t = 
    val mutable name : skin_t_name
    val mutable numSurfaces : int
    val mutable surfaces : skin_t_surfaces

[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = 128)>]
type trGlobals_t_scratchImage =
    [<FieldOffset (0)>]
    val mutable scratchImage : nativeptr<image_t>

[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = 1024)>]
type trGlobals_t_lightmaps =
    [<FieldOffset (0)>]
    val mutable lightmaps : nativeptr<image_t>

[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = 4096)>]
type trGlobals_t_models =
    [<FieldOffset (0)>]
    val mutable models : nativeptr<model_t>

[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = 8192)>]
type trGlobals_t_images =
    [<FieldOffset (0)>]
    val mutable images : nativeptr<image_t>

[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = 65536)>]
type trGlobals_t_shaders =
    [<FieldOffset (0)>]
    val mutable shaders : nativeptr<shader_t>

[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = 4096)>]
type trGlobals_t_skins =
    [<FieldOffset (0)>]
    val mutable skins : nativeptr<skin_t>

[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = 4096)>]
type trGlobals_t_FUNCTABLE_SIZE =
    [<FieldOffset (0)>]
    val mutable table : single

[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = 1024)>]
type trGlobals_t_FOG_TABLE_SIZE =
    [<FieldOffset (0)>]
    val mutable table : single

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
    val mutable scratchImage : trGlobals_t_scratchImage
    val mutable fogImage : nativeptr<image_t>
    val mutable dlightImage : nativeptr<image_t>
    val mutable flareImage : nativeptr<image_t>
    val mutable whiteImage : nativeptr<image_t>
    val mutable identityLightImage : nativeptr<image_t>
    val mutable defaultShader : nativeptr<shader_t>
    val mutable shadowShader : nativeptr<shader_t>
    val mutable projectionShadowShader : nativeptr<shader_t>
    val mutable flareShader : nativeptr<shader_t>
    val mutable sunShader : nativeptr<shader_t>
    val mutable numLightmaps : int
    val mutable lightmaps : trGlobals_t_lightmaps
    val mutable currentEntity : nativeptr<trRefEntity_t>
    val mutable worldEntity : trRefEntity_t
    val mutable currentEntityNum : int
    val mutable shiftedEntityNum : int
    val mutable currentModel : nativeptr<model_t>
    val mutable viewParms : viewParms_t
    val mutable identityLight : single
    val mutable identityLightByte : int
    val mutable overbrightBits : int
    val mutable or' : orientationr_t
    val mutable refdef : trRefdef_t
    val mutable viewCluster : int
    val mutable sunLight : vec3_t
    val mutable sunDirection : vec3_t
    val mutable pc : frontEndCounters_t
    val mutable frontEndMsec : int
    val mutable models : trGlobals_t_models
    val mutable numModels : int
    val mutable numImages : int
    val mutable images : trGlobals_t_images
    val mutable numShaders : int
    val mutable shaders : trGlobals_t_shaders
    val mutable sortedShaders : trGlobals_t_shaders
    val mutable numSkins : int
    val mutable skins : trGlobals_t_skins
    val mutable sinTable : trGlobals_t_FUNCTABLE_SIZE
    val mutable squareTable : trGlobals_t_FUNCTABLE_SIZE
    val mutable triangleTable : trGlobals_t_FUNCTABLE_SIZE
    val mutable sawToothTable : trGlobals_t_FUNCTABLE_SIZE
    val mutable inverseSawToothTable : trGlobals_t_FUNCTABLE_SIZE
    val mutable fogTable : trGlobals_t_FOG_TABLE_SIZE

