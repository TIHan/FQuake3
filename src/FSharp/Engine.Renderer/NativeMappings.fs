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

open System
open Microsoft.FSharp.NativeInterop
open Engine.Core
open Engine.Math
open Engine.NativeInterop
open Engine.Native
open Engine.Renderer.Core

module Axis =
    let inline ofNativePtr (ptr: nativeptr<vec3_t>) =
        Axis (
            (Vec3.ofNativePtr <| NativePtr.add ptr 0),
            (Vec3.ofNativePtr <| NativePtr.add ptr 1),
            (Vec3.ofNativePtr <| NativePtr.add ptr 2))

    let inline toNativeByPtr (ptr: nativeptr<vec3_t>) (axis: Axis) =
        let mutable nativeX = NativePtr.get ptr 0
        let mutable nativeY = NativePtr.get ptr 1
        let mutable nativeZ = NativePtr.get ptr 2

        Vec3.toNativeByPtr &&nativeX axis.x
        Vec3.toNativeByPtr &&nativeY axis.y
        Vec3.toNativeByPtr &&nativeZ axis.z

        NativePtr.set ptr 0 nativeX
        NativePtr.set ptr 1 nativeY
        NativePtr.set ptr 2 nativeZ

module Orientation =
    let inline ofNativePtr (ptr: nativeptr<orientation_t>) =
        let mutable native = NativePtr.read ptr
        {
            Orientation.Origin = Vec3.ofNativePtr &&native.origin;
            Axis = Axis.ofNativePtr &&native.axis;
        }

    let inline toNativeByPtr (ptr: nativeptr<orientation_t>) (orientation: Orientation) =
        let mutable native = NativePtr.read ptr

        Vec3.toNativeByPtr &&native.origin orientation.Origin
        Axis.toNativeByPtr &&native.axis orientation.Axis

        NativePtr.write ptr native

module OrientationR =
    let inline ofNativePtr (ptr: nativeptr<orientationr_t>) =
        let mutable native = NativePtr.read ptr

        {
            Origin = Vec3.ofNativePtr &&native.origin;
            Axis = Axis.ofNativePtr &&native.axis;
            ViewOrigin = Vec3.ofNativePtr &&native.viewOrigin;
            ModelMatrix = Mat4.ofNativePtr &&native.modelMatrix;
        }

    let inline toNativeByPtr (ptr: nativeptr<orientationr_t>) (orientation: OrientationR) =
        let mutable native = NativePtr.read ptr

        Vec3.toNativeByPtr &&native.origin orientation.Origin
        Axis.toNativeByPtr &&native.axis orientation.Axis
        Vec3.toNativeByPtr &&native.viewOrigin orientation.ViewOrigin
        Mat4.toNativeByPtr &&native.modelMatrix orientation.ModelMatrix

        NativePtr.write ptr native

module GLState =
    let inline ofNativePtr (ptr: nativeptr<glstate_t>) =
        let mutable native = NativePtr.read ptr

        {
            CurrentTexture1 = NativePtr.get &&native.currenttextures 0;
            CurrentTexture2 = NativePtr.get &&native.currenttextures 1;
            CurrentTextureMappingUnit = native.currenttmu;
            HasFinishCalled = Boolean.ofNativePtr &&native.finishedCalled;
            TextureEnvironment1 = NativePtr.get &&native.texEnv 0;
            TextureEnvironment2 = NativePtr.get &&native.texEnv 1;
            FaceCulling = native.faceCulling;
            GLStateBits = native.glStateBits;
        }

    let inline toNativeByPtr (ptr: nativeptr<glstate_t>) (value: GLState) =
        let mutable native = NativePtr.read ptr

        NativePtr.set &&native.currenttextures 0 value.CurrentTexture1
        NativePtr.set &&native.currenttextures 1 value.CurrentTexture2
        native.currenttmu <- value.CurrentTextureMappingUnit
        NativePtr.set &&native.texEnv 0 value.TextureEnvironment1
        NativePtr.set &&native.texEnv 1 value.TextureEnvironment2
        native.faceCulling <- value.FaceCulling
        native.glStateBits <- value.GLStateBits

        NativePtr.write ptr native

module Plane =
    let inline ofNativePtr (ptr: nativeptr<cplane_t>) =
        let mutable native = NativePtr.read ptr

        {
            Normal = Vec3.ofNativePtr &&native.normal;
            Distance = native.dist;
            Type = NativePtr.toStructure &&native.type';
            SignBits = native.signbits;
        }

    let inline toNativeByPtr (ptr: nativeptr<cplane_t>) (plane: Plane) =
        let mutable native = NativePtr.read ptr

        Vec3.toNativeByPtr &&native.normal plane.Normal
        native.dist <- plane.Distance
        native.type' <- byte plane.Type
        native.signbits <- plane.SignBits
        
        NativePtr.write ptr native

module Frustum =
    let inline ofNativePtr (ptr: nativeptr<cplane_t>) =
        {
            Left = Plane.ofNativePtr <| NativePtr.add ptr 0;
            Right = Plane.ofNativePtr <| NativePtr.add ptr 1;
            Bottom = Plane.ofNativePtr <| NativePtr.add ptr 2;
            Top = Plane.ofNativePtr <| NativePtr.add ptr 3;
        }

    let inline toNativeByPtr (ptr: nativeptr<cplane_t>) (frustum: Frustum) =
        Plane.toNativeByPtr (NativePtr.add ptr 0) frustum.Left
        Plane.toNativeByPtr (NativePtr.add ptr 1) frustum.Right
        Plane.toNativeByPtr (NativePtr.add ptr 2) frustum.Bottom
        Plane.toNativeByPtr (NativePtr.add ptr 3) frustum.Top

module Model =
    let ofNativePtr (ptr: nativeptr<model_t>) =
        let mutable native = NativePtr.read ptr

        {
        Name = NativePtr.toStringAnsi &&native.name;
        Type = enum<ModelType> (int native.type');
        Index = native.index;
        DataSize = native.dataSize;
        Md3 = Md3.ofNativePtr native.md3
        Md3Lods = List.ofNativePtrArrayMap (native.numLods - 1) (fun x -> Md3.ofNativePtr <| NativePtr.read x) &&native.md3 }

module ViewParms =
    let inline ofNativePtr (ptr: nativeptr<viewParms_t>) =
        let mutable native = NativePtr.read ptr

        {
            Orientation = OrientationR.ofNativePtr &&native.or';
            World = OrientationR.ofNativePtr &&native.world;
            PvsOrigin = Vec3.ofNativePtr &&native.pvsOrigin;
            IsPortal = Boolean.ofNativePtr &&native.isPortal;
            IsMirror = Boolean.ofNativePtr &&native.isMirror;
            FrameSceneId = native.frameSceneNum;
            FrameCount = native.frameCount;
            PortalPlane = Plane.ofNativePtr &&native.portalPlane;
            ViewportX = native.viewportX;
            ViewportY = native.viewportY;
            ViewportWidth = native.viewportWidth;
            ViewportHeight = native.viewportHeight;
            FovX = native.fovX;
            FovY = native.fovY;
            ProjectionMatrix = Mat4.ofNativePtr &&native.projectionMatrix;
            Frustum = Frustum.ofNativePtr &&native.frustum;
            VisibilityBounds = Bounds.ofNativePtr &&native.visBounds;
            ZFar = native.zFar;
        }

    let inline toNativeByPtr (ptr: nativeptr<viewParms_t>) (view: ViewParms) =
        let mutable native = NativePtr.read ptr
        
        OrientationR.toNativeByPtr &&native.or' view.Orientation
        OrientationR.toNativeByPtr &&native.world view.World
        Vec3.toNativeByPtr &&native.pvsOrigin view.PvsOrigin
        native.isPortal <- Boolean.toNative view.IsPortal
        native.isMirror <- Boolean.toNative view.IsMirror
        native.frameSceneNum <- view.FrameSceneId
        native.frameCount <- view.FrameCount
        Plane.toNativeByPtr &&native.portalPlane view.PortalPlane
        native.viewportX <- view.ViewportX
        native.viewportY <- view.ViewportY
        native.viewportWidth <- view.ViewportWidth
        native.viewportHeight <- view.ViewportHeight
        native.fovX <- view.FovX
        native.fovY <- view.FovY
        Mat4.toNativeByPtr &&native.projectionMatrix view.ProjectionMatrix
        Frustum.toNativeByPtr &&native.frustum view.Frustum
        Bounds.toNativeByPtr &&native.visBounds view.VisibilityBounds
        native.zFar <- view.ZFar

        NativePtr.write ptr native

module Rgba =
    let inline ofNativePtr (ptr: nativeptr<byte>) =
        Rgba.create
            (NativePtr.get ptr 0)
            (NativePtr.get ptr 1)
            (NativePtr.get ptr 2)
            (NativePtr.get ptr 3)

    let inline toNativeByPtr (ptr: nativeptr<byte>) (rgba: Rgba) =
        NativePtr.set ptr 0 rgba.R
        NativePtr.set ptr 1 rgba.G
        NativePtr.set ptr 2 rgba.B
        NativePtr.set ptr 3 rgba.A

module DrawVertex =
    let inline ofNativePtr (ptr: nativeptr<drawVert_t>) =
        let mutable native = NativePtr.read ptr

        {
            DrawVertex.Vertex = Vec3.ofNativePtr &&native.xyz;
            St0 = NativePtr.get &&native.st 0;
            St1 = NativePtr.get &&native.st 1;
            Lightmap0 = NativePtr.get &&native.lightmap 0;
            Lightmap1 = NativePtr.get &&native.lightmap 1;
            Normal = Vec3.ofNativePtr &&native.normal;
            Color = Rgba.ofNativePtr &&native.color;
        }

module PolyVertex =
    let inline ofNativePtr (ptr: nativeptr<polyVert_t>) =
        let mutable native = NativePtr.read ptr

        {
            Vertex = Vec3.ofNativePtr &&native.xyz;
            St0 = NativePtr.get &&native.st 0;
            St1 = NativePtr.get &&native.st 1;
            Modulate0 = NativePtr.get &&native.modulate 0;
            Modulate1 = NativePtr.get &&native.modulate 1;
            Modulate2 = NativePtr.get &&native.modulate 2;
            Modulate3 = NativePtr.get &&native.modulate 3;
        }

module FaceVertexPoints =
    let inline ofNativePtr (ptr: nativeptr<single>) =
        {
            Vertex0 = NativePtr.get ptr 0;
            Vertex1 = NativePtr.get ptr 1;
            Vertex2 = NativePtr.get ptr 2;
            Vertex3 = NativePtr.get ptr 3;
            Vertex4 = NativePtr.get ptr 4;
            Vertex5 = NativePtr.get ptr 5;
            Vertex6 = NativePtr.get ptr 6;
            Vertex7 = NativePtr.get ptr 7;
        }

module Surface =
    let inline ofNativePtrFace (ptr: nativeptr<srfSurfaceFace_t>) =
        let mutable native = NativePtr.read ptr

        {
            Plane = Plane.ofNativePtr &&native.plane;
            DlightBit0 = NativePtr.get &&native.dlightBits 0;
            DlightBit1 = NativePtr.get &&native.dlightBits 1;
            PointCount = native.numPoints;
            IndexCount = native.numIndices;
            OffsetIndices = native.ofsIndices;
            Points = FaceVertexPoints.ofNativePtr &&native.points;
        }
        |> Face

    let inline ofNativePtrTriangles (ptr: nativeptr<srfTriangles_t>) =
        let mutable native = NativePtr.read ptr

        {
            DlightBit0 = NativePtr.get &&native.dlightBits 0;
            DlightBit1 = NativePtr.get &&native.dlightBits 1;
            Bounds = Bounds.ofNativePtr &&native.bounds;
            LocalOrigin = Vec3.ofNativePtr &&native.localOrigin;
            Radius = native.radius;
            Indices = List.ofNativePtrArray native.numIndexes native.indexes;
            Vertices = List.ofNativePtrArrayMap native.numVerts DrawVertex.ofNativePtr native.verts;
        }
        |> Triangles

    let inline ofNativePtrPoly (ptr: nativeptr<srfPoly_t>) =
        let mutable native = NativePtr.read ptr

        {
            ShaderHandle = native.hShader;
            FogIndex = native.fogIndex;
            Vertices = List.ofNativePtrArrayMap native.numVerts PolyVertex.ofNativePtr native.verts;
        }
        |> Poly

    let inline ofNativePtrDisplayList (ptr: nativeptr<srfDisplayList_t>) =
        let mutable native = NativePtr.read ptr

        {
            ListId = native.listNum;
        }
        |> DisplayList

    let inline ofNativePtrGridMesh (ptr: nativeptr<srfGridMesh_t>) =
        let mutable native = NativePtr.read ptr

        {
            DlightBit0 = NativePtr.get &&native.dlightBits 0;
            DlightBit1 = NativePtr.get &&native.dlightBits 1;
            MeshBounds = Bounds.ofNativePtr &&native.meshBounds;
            LocalOrigin = Vec3.ofNativePtr &&native.localOrigin;
            MeshRadius = native.meshRadius;
            LodOrigin = Vec3.ofNativePtr &&native.lodOrigin;
            LodRadius = native.lodRadius;
            LodFixed = native.lodFixed;
            LodStitched = native.lodStitched;
            Width = native.width;
            Height = native.height;
            WidthLodError = List.ofNativePtrArray native.width native.widthLodError;
            HeightLodError = List.ofNativePtrArray native.height native.heightLodError;
            Vertex = DrawVertex.ofNativePtr &&native.verts;
        }
        |> Grid

    let inline ofNativePtrFlare (ptr: nativeptr<srfFlare_t>) =
        let mutable native = NativePtr.read ptr

        {
            Origin = Vec3.ofNativePtr &&native.origin;
            Normal = Vec3.ofNativePtr &&native.normal;
            Color = Vec3.ofNativePtr &&native.color;
        }
        |> Flare

    let inline ofNativePtr (ptr: nativeptr<surfaceType_t>) =
        let mutable native = NativePtr.read ptr

        match native with
        | surfaceType_t.SF_BAD -> Bad
        | surfaceType_t.SF_SKIP -> Skip
        | surfaceType_t.SF_FACE ->
            let ptr = NativePtr.ofNativeInt<srfSurfaceFace_t> <| NativePtr.toNativeInt ptr
            ofNativePtrFace ptr
        | surfaceType_t.SF_GRID ->
            let ptr = NativePtr.ofNativeInt<srfGridMesh_t> <| NativePtr.toNativeInt ptr
            ofNativePtrGridMesh ptr
        | surfaceType_t.SF_TRIANGLES ->
            let ptr = NativePtr.ofNativeInt<srfTriangles_t> <| NativePtr.toNativeInt ptr
            ofNativePtrTriangles ptr
        | surfaceType_t.SF_POLY ->
            let ptr = NativePtr.ofNativeInt<srfPoly_t> <| NativePtr.toNativeInt ptr
            ofNativePtrPoly ptr
        | surfaceType_t.SF_MD3 -> Md3
        | surfaceType_t.SF_MD4 -> Md4
        | surfaceType_t.SF_FLARE ->
            let ptr = NativePtr.ofNativeInt<srfFlare_t> <| NativePtr.toNativeInt ptr
            ofNativePtrFlare ptr
        | surfaceType_t.SF_ENTITY -> Entity
        | surfaceType_t.SF_DISPLAY_LIST ->
            let ptr = NativePtr.ofNativeInt<srfDisplayList_t> <| NativePtr.toNativeInt ptr
            ofNativePtrDisplayList ptr
        | _ -> raise <| Exception "Invalid Surface Type" 
        
        
    let toNativeByPtr (ptr: nativeptr<surfaceType_t>) (value: Surface) =
        match value with
        | Md3 ->
            NativePtr.write ptr surfaceType_t.SF_MD3
        | _ -> failwith "not implemented"         

module DrawSurface =
    let ofNativePtr (ptr: nativeptr<drawSurf_t>) =
        let mutable native = NativePtr.read ptr

        {
        Surface = Surface.ofNativePtr native.surface
        ShaderId = native.shaderIndex
        EntityId = native.entityNum
        FogId = native.fogIndex
        DynamicLightMap = native.dlightMap }

    let toNativeByPtr (ptr: nativeptr<drawSurf_t>) (value: DrawSurface) =
        let mutable native = NativePtr.read ptr

        Surface.toNativeByPtr native.surface value.Surface
        native.shaderIndex <- value.ShaderId
        native.entityNum <- value.EntityId
        native.fogIndex <- value.FogId
        native.dlightMap <- value.DynamicLightMap

        NativePtr.write ptr native

module RefEntity =
    let inline ofNativePtr (ptr: nativeptr<refEntity_t>) =
        let mutable native = NativePtr.read ptr

        {
            RefEntity.Type = enum<RefEntityType> (int native.reType);
            RenderFx = enum<RenderFxFlags> (native.renderfx);
            ModelHandle = native.hModel;
            LightingOrigin = Vec3.ofNativePtr &&native.lightingOrigin;
            ShadowPlane = native.shadowPlane;
            Axis = Axis.ofNativePtr &&native.axis;
            HasNonNormalizedAxes = Boolean.ofNativePtr &&native.nonNormalizedAxes;
            Origin = Vec3.ofNativePtr &&native.origin;
            Frame = native.frame;
            OldOrigin = Vec3.ofNativePtr &&native.oldorigin;
            OldFrame = native.oldframe;
            BackLerp = native.backlerp;
            SkinId = native.skinNum;
            CustomSkinHandle = native.customSkin;
            CustomShaderHandle = native.customShader;
            ShaderRgba = Rgba.ofNativePtr &&native.shaderRGBA;
            ShaderTextureCoordinate = Vec2.ofNativePtr &&native.shaderTexCoord;
            ShaderTime = native.shaderTime;
            Radius = native.radius;
            Rotation = native.rotation;
        }

    let inline toNativeByPtr (ptr: nativeptr<refEntity_t>) (entity: RefEntity) =
        let mutable native = NativePtr.read ptr

        native.reType <- enum<refEntityType_t> (int entity.Type)
        native.renderfx <- int entity.RenderFx
        native.hModel <- entity.ModelHandle
        Vec3.toNativeByPtr &&native.lightingOrigin entity.LightingOrigin
        native.shadowPlane <- native.shadowPlane
        Axis.toNativeByPtr &&native.axis entity.Axis
        native.nonNormalizedAxes <- Boolean.toNative entity.HasNonNormalizedAxes
        Vec3.toNativeByPtr &&native.origin entity.Origin
        native.frame <- entity.Frame
        Vec3.toNativeByPtr &&native.oldorigin entity.OldOrigin
        native.oldframe <- entity.OldFrame
        native.backlerp <- entity.BackLerp
        native.skinNum <- entity.SkinId
        native.customSkin <- entity.CustomSkinHandle
        native.customShader <- entity.CustomShaderHandle
        Rgba.toNativeByPtr &&native.shaderRGBA entity.ShaderRgba
        Vec2.toNativeByPtr &&native.shaderTexCoord entity.ShaderTextureCoordinate
        native.shaderTime <- entity.ShaderTime
        native.radius <- entity.Radius
        native.rotation <- entity.Rotation

        NativePtr.write ptr native 

module TrRefEntity =
    let inline ofNativePtr (ptr: nativeptr<trRefEntity_t>) =
        let mutable native = NativePtr.read ptr

        {
            Entity = RefEntity.ofNativePtr &&native.e;
            AxisLength = native.axisLength;
            NeedDlights = Boolean.ofNativePtr &&native.needDlights;
            IsLightingCalculated = Boolean.ofNativePtr &&native.lightingCalculated;
            LightDirection = Vec3.ofNativePtr &&native.lightDir;
            AmbientLight = Vec3.ofNativePtr &&native.ambientLight;
            AmbientLightInt = native.ambientLightInt;
            DirectedLight = Vec3.ofNativePtr &&native.directedLight;
        }

    let inline toNativeByPtr (ptr: nativeptr<trRefEntity_t>) (refEntity: TrRefEntity) =
        let mutable native = NativePtr.read ptr
        
        RefEntity.toNativeByPtr &&native.e refEntity.Entity
        native.axisLength <- refEntity.AxisLength
        native.needDlights <- Boolean.toNative refEntity.NeedDlights
        native.lightingCalculated <- Boolean.toNative refEntity.IsLightingCalculated
        Vec3.toNativeByPtr &&native.lightDir refEntity.LightDirection
        Vec3.toNativeByPtr &&native.ambientLight refEntity.AmbientLight
        native.ambientLightInt <- refEntity.AmbientLightInt
        Vec3.toNativeByPtr &&native.directedLight refEntity.DirectedLight
        
        NativePtr.write ptr native

module Refdef =
    let inline ofNativePtr (ptr: nativeptr<refdef_t>) =
        let mutable native = NativePtr.read ptr

        {
            X = native.x;
            Y = native.y;
            Width = native.width;
            Height = native.height;
            ViewOrigin = Vec3.ofNativePtr &&native.vieworg;
            ViewAxis = Axis.ofNativePtr &&native.viewaxis;
            Time = native.time;
            RdFlags = enum<RdFlags> (native.rdflags)
        }

    let inline toNativeByPtr (ptr: nativeptr<refdef_t>) (refdef: Refdef) =
        let mutable native = NativePtr.read ptr

        native.x <- refdef.X
        native.y <- refdef.Y
        native.width <- refdef.Width
        native.height <- refdef.Height
        Vec3.toNativeByPtr &&native.vieworg refdef.ViewOrigin
        Axis.toNativeByPtr &&native.viewaxis refdef.ViewAxis
        native.time <- refdef.Time
        native.rdflags <- int refdef.RdFlags

        NativePtr.write ptr native

module Dlight =
    let inline ofNativePtr (ptr: nativeptr<dlight_t>) =
        let mutable native = NativePtr.read ptr

        {
            Origin = Vec3.ofNativePtr &&native.origin;
            Color = Vec3.ofNativePtr &&native.color;
            Radius = native.radius;
            Transformed = Vec3.ofNativePtr &&native.transformed;
            Additive = native.additive;
        }

module TrRefdef =
    let ofNativePtr (ptr: nativeptr<trRefdef_t>) =
        let mutable native = NativePtr.read ptr

        {
            X = native.x;
            Y = native.y;
            Width = native.width;
            Height = native.height;
            ViewOrigin = Vec3.ofNativePtr &&native.vieworg;
            ViewAxis = Axis.ofNativePtr &&native.viewaxis;
            Time = native.time;
            RdFlags = enum<RdFlags> (native.rdflags);

            AreaMask = ByteString.ofNativePtr 32 &&native.areamask;
            HasAreaMaskModified = Boolean.ofNativePtr &&native.areamaskModified;

            FloatTime = native.floatTime;
            Text = List.ofNativePtrArrayMap 8 (fun x -> "") &&native.text; // FIXME: This isn't right.
            Entities = List.ofNativePtrArrayMap native.num_entities (fun x -> TrRefEntity.ofNativePtr x) native.enities;
            Dlights = List.ofNativePtrArrayMap native.num_delights (fun x -> Dlight.ofNativePtr x) native.dlights;
            Polys = List.ofNativePtrArrayMap native.numPolys (fun x -> Surface.ofNativePtrPoly x) native.polys;
            DrawSurfaces = List.ofNativePtrArrayMap native.numDrawSurfs (fun x -> DrawSurface.ofNativePtr x) native.drawSurfs;
        }

    let toNativeByPtr (ptr: nativeptr<trRefdef_t>) (value: TrRefdef) =
        let mutable native = NativePtr.read ptr

        native.x <- value.X
        native.y <- value.Y
        native.width <- value.Width
        native.height <- value.Height
        Vec3.toNativeByPtr &&native.vieworg value.ViewOrigin
        Axis.toNativeByPtr &&native.viewaxis value.ViewAxis
        native.time <- value.Time
        native.rdflags <- int value.RdFlags
        // TODO: Map AreaMask
        native.areamaskModified <- Boolean.toNative value.HasAreaMaskModified
        native.floatTime <- value.FloatTime
        // TODO: Map Text
        // TODO: Map Entities
        // TODO: Map Dlights
        // TODO: Map Polys
        // TODO: Map DrawSurfaces

module FrontEndPerformanceCounters =
    let ofNativePtr (ptr: nativeptr<frontEndCounters_t>) =
        let mutable native = NativePtr.read ptr

        {
            SpherePatch = { CullIn = native.c_sphere_cull_patch_in; CullClip = native.c_sphere_cull_patch_clip; CullOut = native.c_sphere_cull_patch_out };
            BoxPatch = { CullIn = native.c_box_cull_patch_in; CullClip = native.c_box_cull_patch_clip; CullOut = native.c_box_cull_patch_out };
            SphereMd3 = { CullIn = native.c_sphere_cull_md3_in; CullClip = native.c_sphere_cull_md3_clip; CullOut = native.c_sphere_cull_md3_out };
            BoxMd3 = { CullIn = native.c_box_cull_md3_in; CullClip = native.c_box_cull_md3_clip; CullOut = native.c_box_cull_md3_out };
            Leafs = native.c_leafs;
            DynamicLightSurfaces = native.c_dlightSurfaces;
            DynamicLightSurfacesCulled = native.c_dlightSurfacesCulled
        }

    let toNativeByPtr (ptr: nativeptr<frontEndCounters_t>) (value: FrontEndPerformanceCounters) =
        let mutable native = NativePtr.read ptr

        native.c_sphere_cull_patch_in <- value.SpherePatch.CullIn
        native.c_sphere_cull_patch_clip <- value.SpherePatch.CullClip
        native.c_sphere_cull_patch_out <- value.SpherePatch.CullOut
        native.c_box_cull_patch_in <- value.BoxPatch.CullIn
        native.c_box_cull_patch_clip <- value.BoxPatch.CullClip
        native.c_box_cull_patch_out <- value.BoxPatch.CullOut
        native.c_sphere_cull_md3_in <- value.SphereMd3.CullIn
        native.c_sphere_cull_md3_clip <- value.SphereMd3.CullClip
        native.c_sphere_cull_md3_out <- value.SphereMd3.CullOut
        native.c_box_cull_md3_in <- value.BoxMd3.CullIn
        native.c_box_cull_md3_clip <- value.BoxMd3.CullClip
        native.c_box_cull_md3_out <- value.BoxMd3.CullOut
        native.c_leafs <- value.Leafs
        native.c_dlightSurfaces <- value.DynamicLightSurfaces
        native.c_dlightSurfacesCulled <- value.DynamicLightSurfacesCulled

        NativePtr.write ptr native

module Backend =
    let inline ofNativePtr (ptr: nativeptr<backEndState_t>) =
        let mutable native = NativePtr.read ptr

        {
            Refdef = TrRefdef.ofNativePtr &&native.refdef;
            View = ViewParms.ofNativePtr &&native.viewParms;
            Orientation = OrientationR.ofNativePtr &&native.or';
            IsHyperspace = Boolean.ofNativePtr &&native.isHyperspace;
            CurrentEntity = Option.ofNativePtr (fun x -> TrRefEntity.ofNativePtr x) native.currentEntity;
            HasSkyRenderedThisView = Boolean.ofNativePtr &&native.skyRenderedThisView;
            IsProjection2D = Boolean.ofNativePtr &&native.projection2D;
            Color2D = Rgba.ofNativePtr &&native.color2D;
            IsVertex2D = Boolean.ofNativePtr &&native.vertexes2D;
            Entity2D = TrRefEntity.ofNativePtr &&native.entity2D;
        }

    // TODO: NOT FINISHED!
    let inline toNativeByPtr (ptr: nativeptr<backEndState_t>) (value: Backend) =
        let mutable native = NativePtr.read ptr

        Boolean.toNativeByPtr &&native.isHyperspace value.IsHyperspace
        Boolean.toNativeByPtr &&native.skyRenderedThisView value.HasSkyRenderedThisView
        Boolean.toNativeByPtr &&native.projection2D value.IsProjection2D

        NativePtr.write ptr native

module Image =
    let ofNativePtr (ptr: nativeptr<image_t>) =
        let mutable native = NativePtr.read ptr

        {
            Path = NativePtr.toStringAnsi &&native.imgName;
            Width = native.width;
            Height = native.height;
            UploadWidth = native.uploadWidth;
            UploadHeight = native.uploadHeight;
            TextureId = native.texnum;
            FrameUsed = native.frameUsed;
            InternalFormat = native.internalFormat;
            IsMipmap = Boolean.ofNativePtr &&native.mipmap;
            CanAllowPicmip = Boolean.ofNativePtr &&native.allowPicmip;
            WrapClampMode = native.wrapClampMode;
        }

    let toNativeByPtr (ptr: nativeptr<image_t>) (value: Image) =
        let mutable native = NativePtr.read ptr

        native.imgName <- String.toStructure value.Path
        native.width <- value.Width
        native.height <- value.Height
        native.uploadWidth <- value.UploadWidth
        native.uploadHeight <- value.UploadHeight
        native.texnum <- value.TextureId
        native.frameUsed <- value.FrameUsed
        native.internalFormat <- value.InternalFormat
        native.mipmap <- Boolean.toNative value.IsMipmap
        native.allowPicmip <- Boolean.toNative value.CanAllowPicmip
        native.wrapClampMode <- value.WrapClampMode

        NativePtr.write ptr native

module Skybox =
    let inline ofNativePtr (ptr: nativeptr<image_t>) =
        {
        Image1 = Image.ofNativePtr (NativePtr.add ptr 0)
        Image2 = Image.ofNativePtr (NativePtr.add ptr 1)
        Image3 = Image.ofNativePtr (NativePtr.add ptr 2)
        Image4 = Image.ofNativePtr (NativePtr.add ptr 3)
        Image5 = Image.ofNativePtr (NativePtr.add ptr 4)
        Image6 = Image.ofNativePtr (NativePtr.add ptr 5) }

module SkyParms =
    let inline ofNativePtr (ptr: nativeptr<skyParms_t>) =
        let mutable native = NativePtr.read ptr

        {
        CloudHeight = native.cloudHeight }

module FogParms =
    let inline ofNativePtr (ptr: nativeptr<fogParms_t>) =
        let mutable native = NativePtr.read ptr

        {
        Color = Vec3.ofNativePtr &&native.color
        DepthForOpaque = native.depthForOpaque }

// TODO: NOT FINISHED!
module Shader =
    let rec ofNativePtr (ptr: nativeptr<shader_t>) =
        let mutable native = NativePtr.read ptr

        {
        Name = NativePtr.toStringAnsi &&native.name.value;
        LightmapIndex = native.lightmapIndex;
        Index = native.index;
        SortedIndex = native.sortedIndex;
        Sort = native.sort;
        IsDefaultShader = Boolean.ofNativePtr &&native.defaultShader;
        IsExplicitlyDefined = Boolean.ofNativePtr &&native.explicitlyDefined;
        SurfaceFlags = native.surfaceFlags;
        ContentFlags = native.contentFlags;
        IsEntityMergable = Boolean.ofNativePtr &&native.entityMergable;
        IsSky = Boolean.ofNativePtr &&native.isSky;
        Sky = SkyParms.ofNativePtr &&native.sky;
        Fog = FogParms.ofNativePtr &&native.fogParms;
        PortalRange = native.portalRange;
        MultitextureEnv = native.multitextureEnv;
        CullType = enum<CullType> (int native.cullType);
        HasPolygonOffset = Boolean.ofNativePtr &&native.polygonOffset;
        HasNoMipMaps = Boolean.ofNativePtr &&native.noMipMaps;
        HasNoPicMip = Boolean.ofNativePtr &&native.noPicMip;
        FogPassType = enum<FogPassType> (int native.fogPass);
        NeedsNormal = Boolean.ofNativePtr &&native.needsNormal;
        NeedsSt1 = Boolean.ofNativePtr &&native.needsST1;
        NeedsSt2 = Boolean.ofNativePtr &&native.needsST2;
        NeedsColor = Boolean.ofNativePtr &&native.needsColor;
        Deforms = []; // TODO:
        Stages = []; // TODO:
        ClampTime = native.clamptime;
        TimeOffset = native.timeOffset;
        CurrentState = native.currentstate;
        ExpireTime = int64 native.expireTime;
        ShaderStates = NativePtr.toList 32 (&&native.shaderStates.value); }

module SkinSurface =
    let ofNativePtr (ptr: nativeptr<skinSurface_t>) =
        let mutable native = NativePtr.read ptr

        {
        Name = NativePtr.toStringAnsi &&native.name 
        Shader = Option.ofNativePtr Shader.ofNativePtr native.shader }

module Skin =
    let ofNativeptr (ptr: nativeptr<skin_t>) =
        let mutable native = NativePtr.read ptr

        {
        Skin.Name = NativePtr.toStringAnsi &&native.name
        Surfaces = List.ofNativePtrArrayMap native.numSurfaces SkinSurface.ofNativePtr native.surfaces.surfaces }

module Bsp =
    // this will be used to prevent major copying
    let mutable lightGridData : byte array = [||]

    let setLightGridData (size: int) (ptr: nativeptr<byte>) =
        lightGridData <- NativePtr.toArray size ptr

module LightGridBounds =
    let ofNativePtr (ptr: nativeptr<int>) =
        LightGridBounds (
            NativePtr.get ptr 0,
            NativePtr.get ptr 1,
            NativePtr.get ptr 2)

module LightGrid =
    let ofNativePtr (ptr: nativeptr<world_t>) =
        let mutable native = NativePtr.read ptr

        {
        Origin = Vec3.ofNativePtr &&native.lightGridOrigin;
        Size = Vec3.ofNativePtr &&native.lightGridSize;
        InverseSize = Vec3.ofNativePtr &&native.lightGridInverseSize;
        Bounds = LightGridBounds.ofNativePtr &&native.lightGridBounds;
        Data = Bsp.lightGridData }

    let optionOfNativePtr (ptr: nativeptr<world_t>) = 
        Option.ofNativePtr ofNativePtr ptr

// TODO: Not finished.
module World =
    let ofNativePtr (ptr: nativeptr<world_t>) =
        let mutable native = NativePtr.read ptr

        {
        Name = NativePtr.toStringAnsi &&native.name.value
        BaseName = NativePtr.toStringAnsi &&native.baseName.value
        DataSize = native.dataSize
        Shaders = []
        BModels = []
        Planes = []
        Nodes = []
        Surfaces = []
        MarkSurfaces = []
        Fogs = []
        LightGrid = LightGrid.ofNativePtr ptr
        ClusterCount = native.numClusters
        ClusterByteCount = native.clusterBytes
        Vis = []
        NoVis = []
        EntityString = NativePtr.toStringAnsi native.entityString
        EntityParsePoint = NativePtr.toStringAnsi native.entityParsePoint }

// TODO: This will need more work over time.
module Renderer =
    let sortDrawSurfaces (size: int) (ptr: nativeptr<drawSurf_t>) (trPtr: nativeptr<trGlobals_t>) =
        let trNative = NativePtr.read trPtr
        let mutable surfs =
            NativePtr.toArray size ptr
                |> Array.sortWith (fun sx sy ->
                let x = NativePtr.read <| NativePtr.get (&&trNative.shaders.value) sx.shaderIndex
                let y = NativePtr.read <| NativePtr.get (&&trNative.shaders.value) sy.shaderIndex
                if x.sort < y.sort then
                    -1
                elif x.sort > y.sort then
                    1
                else
                    // Portals need to be sorted by portalRange.
                    if x.sort = 1.f && y.sort = 1.f && x.portalRange < y.portalRange then
                        -1
                    else
                        let sxt = NativePtr.read sx.surface
                        let syt = NativePtr.read sy.surface
                        sxt.CompareTo(syt))


        for i = 0 to size - 1 do
            NativePtr.set ptr i surfs.[i]

    let mutable state : Renderer option = None

    let inline ofNativePtr (ptr: nativeptr<trGlobals_t>) =
        match state with
        | Some x -> x
        | _ ->

        let mutable native = NativePtr.read ptr

        {
        World = Option.ofNativePtr World.ofNativePtr native.world
        CurrentEntity = Option.ofNativePtr TrRefEntity.ofNativePtr native.currentEntity
        CurrentEntityId = native.currentEntityNum
        CurrentModel = Option.ofNativePtr Model.ofNativePtr native.currentModel
        ViewParms = ViewParms.ofNativePtr &&native.viewParms
        IdentityLight = native.identityLight
        IdentityLightByte = native.identityLightByte
        Refdef = TrRefdef.ofNativePtr &&native.refdef
        Orientation = OrientationR.ofNativePtr &&native.or'
        SunLight = Vec3.ofNativePtr &&native.sunLight
        SunDirection = Vec3.ofNativePtr &&native.sunDirection
        PerfCounters = FrontEndPerformanceCounters.ofNativePtr &&native.pc
        DefaultShaderId = (NativePtr.read<shader_t> native.defaultShader).index
        Shaders = List.ofNativePtrArrayMap native.numShaders Shader.ofNativePtr native.shaders.value }
        //Images = List.ofNativePtrArrayMap native.numImages (fun x -> Image.ofNativePtr x) native.images.value

    let inline toNativeByPtr (ptr: nativeptr<trGlobals_t>) (r: Renderer) =
        let mutable native = NativePtr.read ptr

        // TODO: Map World - Property World
        TrRefEntity.toNativeByPtr native.currentEntity r.CurrentEntity.Value
        native.currentEntityNum <- r.CurrentEntityId
        // TODO: Map Model - Property CurrentModel
        ViewParms.toNativeByPtr &&native.viewParms r.ViewParms
        native.identityLight <- r.IdentityLight
        native.identityLightByte <- r.IdentityLightByte
        TrRefdef.toNativeByPtr &&native.refdef r.Refdef
        OrientationR.toNativeByPtr &&native.or' r.Orientation
        Vec3.toNativeByPtr &&native.sunLight r.SunLight
        Vec3.toNativeByPtr &&native.sunDirection r.SunDirection
        FrontEndPerformanceCounters.toNativeByPtr &&native.pc r.PerfCounters

        // Images - Special Handling
        //List.toNativePtrArrayByPtr native.images.value Image.toNativeByPtr tr.Images

        // TODO: Map DefaultShaderId
        // TODO: Map Shaders
        NativePtr.write ptr native

    let mark (ptr: nativeptr<trGlobals_t>)  =
        match state with
        | Some _ -> failwith "Renderer already marked."
        | _ ->
        state <- Some <| ofNativePtr ptr

    let unmark () =
        state <- None