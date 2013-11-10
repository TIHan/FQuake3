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
        Axis.create
            (Vector3.ofNativePtr <| NativePtr.add ptr 0)
            (Vector3.ofNativePtr <| NativePtr.add ptr 1)
            (Vector3.ofNativePtr <| NativePtr.add ptr 2)

    let inline toNativeByPtr (ptr: nativeptr<vec3_t>) (axis: Axis) =
        let mutable nativeX = NativePtr.get ptr 0
        let mutable nativeY = NativePtr.get ptr 1
        let mutable nativeZ = NativePtr.get ptr 2

        Vector3.toNativeByPtr &&nativeX axis.X
        Vector3.toNativeByPtr &&nativeY axis.Y
        Vector3.toNativeByPtr &&nativeZ axis.Z

        NativePtr.set ptr 0 nativeX
        NativePtr.set ptr 1 nativeY
        NativePtr.set ptr 2 nativeZ

module Orientation =
    let inline ofNativePtr (ptr: nativeptr<orientation_t>) =
        let mutable native = NativePtr.read ptr
        {
            Orientation.Origin = Vector3.ofNativePtr &&native.origin;
            Axis = Axis.ofNativePtr &&native.axis;
        }

    let inline toNativeByPtr (ptr: nativeptr<orientation_t>) (orientation: Orientation) =
        let mutable native = NativePtr.read ptr

        Vector3.toNativeByPtr &&native.origin orientation.Origin
        Axis.toNativeByPtr &&native.axis orientation.Axis

        NativePtr.write ptr native

module OrientationR =
    let inline ofNativePtr (ptr: nativeptr<orientationr_t>) =
        let mutable native = NativePtr.read ptr

        {
            Origin = Vector3.ofNativePtr &&native.origin;
            Axis = Axis.ofNativePtr &&native.axis;
            ViewOrigin = Vector3.ofNativePtr &&native.viewOrigin;
            ModelMatrix = Matrix4x4.ofNativePtr &&native.modelMatrix;
        }

    let inline toNativeByPtr (ptr: nativeptr<orientationr_t>) (orientation: OrientationR) =
        let mutable native = NativePtr.read ptr

        Vector3.toNativeByPtr &&native.origin orientation.Origin
        Axis.toNativeByPtr &&native.axis orientation.Axis
        Vector3.toNativeByPtr &&native.viewOrigin orientation.ViewOrigin
        Matrix4x4.toNativeByPtr &&native.modelMatrix orientation.ModelMatrix

        NativePtr.write ptr native

module Plane =
    let inline ofNativePtr (ptr: nativeptr<cplane_t>) =
        let mutable native = NativePtr.read ptr

        {
            Normal = Vector3.ofNativePtr &&native.normal;
            Distance = native.dist;
            Type = NativePtr.toStructure &&native.type';
            SignBits = native.signbits;
        }

    let inline toNativeByPtr (ptr: nativeptr<cplane_t>) (plane: Plane) =
        let mutable native = NativePtr.read ptr

        Vector3.toNativeByPtr &&native.normal plane.Normal
        native.dist <- plane.Distance
        NativePtr.ofStructure plane.Type &&native.type'
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

module ViewParms =
    let inline ofNativePtr (ptr: nativeptr<viewParms_t>) =
        let mutable native = NativePtr.read ptr

        {
            Orientation = OrientationR.ofNativePtr &&native.or';
            World = OrientationR.ofNativePtr &&native.world;
            PvsOrigin = Vector3.ofNativePtr &&native.pvsOrigin;
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
            ProjectionMatrix = Matrix4x4.ofNativePtr &&native.projectionMatrix;
            Frustum = Frustum.ofNativePtr &&native.frustum;
            VisibilityBounds = Bounds.ofNativePtr &&native.visBounds;
            ZFar = native.zFar;
        }

    let inline toNativeByPtr (ptr: nativeptr<viewParms_t>) (view: ViewParms) =
        let mutable native = NativePtr.read ptr
        
        OrientationR.toNativeByPtr &&native.or' view.Orientation
        OrientationR.toNativeByPtr &&native.world view.World
        Vector3.toNativeByPtr &&native.pvsOrigin view.PvsOrigin
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
        Matrix4x4.toNativeByPtr &&native.projectionMatrix view.ProjectionMatrix
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
            DrawVertex.Vertex = Vector3.ofNativePtr &&native.xyz;
            St0 = NativePtr.get &&native.st 0;
            St1 = NativePtr.get &&native.st 1;
            Lightmap0 = NativePtr.get &&native.lightmap 0;
            Lightmap1 = NativePtr.get &&native.lightmap 1;
            Normal = Vector3.ofNativePtr &&native.normal;
            Color = Rgba.ofNativePtr &&native.color;
        }

module PolyVertex =
    let inline ofNativePtr (ptr: nativeptr<polyVert_t>) =
        let mutable native = NativePtr.read ptr

        {
            Vertex = Vector3.ofNativePtr &&native.xyz;
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
            LocalOrigin = Vector3.ofNativePtr &&native.localOrigin;
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
            LocalOrigin = Vector3.ofNativePtr &&native.localOrigin;
            MeshRadius = native.meshRadius;
            LodOrigin = Vector3.ofNativePtr &&native.lodOrigin;
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
            Origin = Vector3.ofNativePtr &&native.origin;
            Normal = Vector3.ofNativePtr &&native.normal;
            Color = Vector3.ofNativePtr &&native.color;
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

module DrawSurface =
    let inline ofNativePtr (ptr: nativeptr<drawSurf_t>) =
        let mutable native = NativePtr.read ptr

        {
            Sort = native.sort;
            Surface = Surface.ofNativePtr native.surface;
        }

module RefEntity =
    let inline ofNativePtr (ptr: nativeptr<refEntity_t>) =
        let mutable native = NativePtr.read ptr

        {
            RefEntity.Type = enum<RefEntityType> (int native.reType);
            RenderFx = native.renderfx;
            ModelHandle = native.hModel;
            LightingOrigin = Vector3.ofNativePtr &&native.lightingOrigin;
            ShadowPlane = native.shadowPlane;
            Axis = Axis.ofNativePtr &&native.axis;
            HasNonNormalizedAxes = Boolean.ofNativePtr &&native.nonNormalizedAxes;
            Origin = Vector3.ofNativePtr &&native.origin;
            Frame = native.frame;
            OldOrigin = Vector3.ofNativePtr &&native.oldorigin;
            OldFrame = native.oldframe;
            BackLerp = native.backlerp;
            SkinId = native.skinNum;
            CustomSkinHandle = native.customSkin;
            CustomShaderHandle = native.customShader;
            ShaderRgba = Rgba.ofNativePtr &&native.shaderRGBA;
            ShaderTextureCoordinate = Vector2.ofNativePtr &&native.shaderTexCoord;
            ShaderTime = native.shaderTime;
            Radius = native.radius;
            Rotation = native.rotation;
        }

    let inline toNativeByPtr (ptr: nativeptr<refEntity_t>) (entity: RefEntity) =
        let mutable native = NativePtr.read ptr

        native.reType <- enum<refEntityType_t> (int entity.Type)
        native.renderfx <- entity.RenderFx
        native.hModel <- entity.ModelHandle
        Vector3.toNativeByPtr &&native.lightingOrigin entity.LightingOrigin
        native.shadowPlane <- native.shadowPlane
        Axis.toNativeByPtr &&native.axis entity.Axis
        native.nonNormalizedAxes <- Boolean.toNative entity.HasNonNormalizedAxes
        Vector3.toNativeByPtr &&native.origin entity.Origin
        native.frame <- entity.Frame
        Vector3.toNativeByPtr &&native.oldorigin entity.OldOrigin
        native.oldframe <- entity.OldFrame
        native.backlerp <- entity.BackLerp
        native.skinNum <- entity.SkinId
        native.customSkin <- entity.CustomSkinHandle
        native.customShader <- entity.CustomShaderHandle
        Rgba.toNativeByPtr &&native.shaderRGBA entity.ShaderRgba
        Vector2.toNativeByPtr &&native.shaderTexCoord entity.ShaderTextureCoordinate
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
            LightDirection = Vector3.ofNativePtr &&native.lightDir;
            AmbientLight = Vector3.ofNativePtr &&native.ambientLight;
            AmbientLightInt = native.ambientLightInt;
            DirectedLight = Vector3.ofNativePtr &&native.directedLight;
        }

    let inline toNativeByPtr (ptr: nativeptr<trRefEntity_t>) (refEntity: TrRefEntity) =
        let mutable native = NativePtr.read ptr
        
        RefEntity.toNativeByPtr &&native.e refEntity.Entity
        native.axisLength <- refEntity.AxisLength
        native.needDlights <- Boolean.toNative refEntity.NeedDlights
        native.lightingCalculated <- Boolean.toNative refEntity.IsLightingCalculated
        Vector3.toNativeByPtr &&native.lightDir refEntity.LightDirection
        Vector3.toNativeByPtr &&native.ambientLight refEntity.AmbientLight
        native.ambientLightInt <- refEntity.AmbientLightInt
        Vector3.toNativeByPtr &&native.directedLight refEntity.DirectedLight
        
        NativePtr.write ptr native

    module Option =
        let inline ofNativePtr (ptr: nativeptr<trRefEntity_t>) =
            Some <| ofNativePtr ptr

module Refdef =
    let inline ofNativePtr (ptr: nativeptr<refdef_t>) =
        let mutable native = NativePtr.read ptr

        {
            X = native.x;
            Y = native.y;
            Width = native.width;
            Height = native.height;
            ViewOrigin = Vector3.ofNativePtr &&native.vieworg;
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
        Vector3.toNativeByPtr &&native.vieworg refdef.ViewOrigin
        Axis.toNativeByPtr &&native.viewaxis refdef.ViewAxis
        native.time <- refdef.Time
        native.rdflags <- int refdef.RdFlags

        NativePtr.write ptr native

module Dlight =
    let inline ofNativePtr (ptr: nativeptr<dlight_t>) =
        let mutable native = NativePtr.read ptr

        {
            Origin = Vector3.ofNativePtr &&native.origin;
            Color = Vector3.ofNativePtr &&native.color;
            Radius = native.radius;
            Transformed = Vector3.ofNativePtr &&native.transformed;
            Additive = native.additive;
        }

module TrRefdef =
    let inline ofNativePtr (ptr: nativeptr<trRefdef_t>) =
        let mutable native = NativePtr.read ptr

        {
            X = native.x;
            Y = native.y;
            Width = native.width;
            Height = native.height;
            ViewOrigin = Vector3.ofNativePtr &&native.vieworg;
            ViewAxis = Axis.ofNativePtr &&native.viewaxis;
            Time = native.time;
            RdFlags = enum<RdFlags> (native.rdflags);

            AreaMask = ByteString.ofNativePtr 32 &&native.areamask;
            HasAreaMaskModified = native.areamaskModified;

            FloatTime = native.floatTime;
            Text = List.ofNativePtrArrayMap 8 (fun x -> "") &&native.text; // FIXME: This isn't right.
            Entities = List.ofNativePtrArrayMap native.num_entities (fun x -> TrRefEntity.ofNativePtr x) native.enities;
            Dlights = List.ofNativePtrArrayMap native.num_delights (fun x -> Dlight.ofNativePtr x) native.dlights;
            Polys = List.ofNativePtrArrayMap native.numPolys (fun x -> Surface.ofNativePtrPoly x) native.polys;
            DrawSurfaces = List.ofNativePtrArrayMap native.numDrawSurfs (fun x -> DrawSurface.ofNativePtr x) native.drawSurfs;
        }

module FrontEndPerformanceCounters =
    let inline ofNativePtr (ptr: nativeptr<frontEndCounters_t>) =
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

    let inline toNativeByPtr (ptr: nativeptr<frontEndCounters_t>) (value: FrontEndPerformanceCounters) =
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

// TODO: This will need more work over time.
module TrGlobals =
    let inline ofNativePtr (ptr: nativeptr<trGlobals_t>) =
        let mutable native = NativePtr.read ptr

        {
            CurrentEntity = TrRefEntity.Option.ofNativePtr native.currentEntity;
            CurrentEntityId = native.currentEntityNum;
            ViewParms = ViewParms.ofNativePtr &&native.viewParms;
            Refdef = TrRefdef.ofNativePtr &&native.refdef;
            Orientation = OrientationR.ofNativePtr &&native.or';
            PerfCounters = FrontEndPerformanceCounters.ofNativePtr &&native.pc
        }

    let inline toNativeByPtr (ptr: nativeptr<trGlobals_t>) (tr: TrGlobals) =
        let mutable native = NativePtr.read ptr

        TrRefEntity.toNativeByPtr native.currentEntity tr.CurrentEntity.Value
        native.currentEntityNum <- tr.CurrentEntityId
        ViewParms.toNativeByPtr &&native.viewParms tr.ViewParms
        // TODO: Map TrRefDef - Property Refdef
        OrientationR.toNativeByPtr &&native.or' tr.Orientation
        FrontEndPerformanceCounters.toNativeByPtr &&native.pc tr.PerfCounters

        NativePtr.write ptr native

