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
open FSharpx.Core
open FSharpx.Collections
open Engine.Core
open Engine.Math
open Engine.NativeInterop
open Engine.Native
open Engine.Renderer

module Axis =
    let inline ofNative (ptr: nativeptr<vec3_t>) =
        {
            Axis.X = Vector3.ofNative <| NativePtr.get ptr 0;
            Axis.Y = Vector3.ofNative <| NativePtr.get ptr 1;
            Axis.Z = Vector3.ofNative <| NativePtr.get ptr 2;
        }

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

module Bounds =
    let inline ofNative (ptr: nativeptr<vec3_t>) =
        {
            Bounds.Bounds0 = Vector3.ofNative <| NativePtr.get ptr 0;
            Bounds1 = Vector3.ofNative <| NativePtr.get ptr 1;
        }

    let inline toNativeByPtr (ptr: nativeptr<vec3_t>) (bounds: Bounds) =
        let mutable nativeX = NativePtr.get ptr 0
        let mutable nativeY = NativePtr.get ptr 1

        Vector3.toNativeByPtr &&nativeX bounds.Bounds0
        Vector3.toNativeByPtr &&nativeY bounds.Bounds1

        NativePtr.set ptr 0 nativeX
        NativePtr.set ptr 1 nativeY

module Orientation =
    let inline ofNative (native: orientation_t) =
        {
            Orientation.Origin = Vector3.ofNative native.origin;
            Axis = Axis.ofNative &&native.axis;
        }

module OrientationR =
    let inline ofNative (native: orientationr_t) =
        {
            Origin = Vector3.ofNative native.origin;
            Axis = Axis.ofNative &&native.axis;
            ViewOrigin = Vector3.ofNative native.viewOrigin;
            ModelMatrix = Matrix4x4.ofNative &&native.modelMatrix;
        }

    let inline toNativeByPtr (ptr: nativeptr<orientationr_t>) (orientation: OrientationR) =
        let mutable native = NativePtr.read ptr

        Vector3.toNativeByPtr &&native.origin orientation.Origin
        Axis.toNativeByPtr &&native.axis orientation.Axis
        Vector3.toNativeByPtr &&native.viewOrigin orientation.ViewOrigin
        Matrix4x4.toNativeByPtr &&native.modelMatrix orientation.ModelMatrix

        NativePtr.write ptr native

module Plane =
    let inline ofNative (native: cplane_t) =
        {
            Normal = Vector3.ofNative native.normal;
            Distance = native.dist;
            Type = NativePtr.toStructure &&native.type';
            SignBits = native.signbits;
        }

    let inline toNative (plane: Plane) =
        let mutable native = cplane_t ()

        Vector3.toNativeByPtr &&native.normal plane.Normal
        native.dist <- plane.Distance
        NativePtr.ofStructure plane.Type &&native.type'
        native.signbits <- plane.SignBits
        native

module Frustum =
    let inline ofNative (ptr: nativeptr<cplane_t>) =
        {
            Left = Plane.ofNative <| NativePtr.get ptr 0;
            Right = Plane.ofNative <| NativePtr.get ptr 1;
            Bottom = Plane.ofNative <| NativePtr.get ptr 2;
            Top = Plane.ofNative <| NativePtr.get ptr 3;
        }

    let inline toNativeByPtr (ptr: nativeptr<cplane_t>) (frustum: Frustum) =
        NativePtr.set ptr 0 <| Plane.toNative frustum.Left
        NativePtr.set ptr 1 <| Plane.toNative frustum.Right
        NativePtr.set ptr 2 <| Plane.toNative frustum.Bottom
        NativePtr.set ptr 3 <| Plane.toNative frustum.Top

module ViewParms =
    let inline ofNative (native: viewParms_t) =
        {
            Orientation = OrientationR.ofNative native.or';
            World = OrientationR.ofNative native.world;
            PvsOrigin = Vector3.ofNative native.pvsOrigin;
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
            ProjectionMatrix = Matrix4x4.ofNative &&native.projectionMatrix;
            Frustum = Frustum.ofNative &&native.frustum;
            VisibilityBounds = Bounds.ofNative &&native.visBounds;
            ZFar = native.zFar;
        }

    let inline toNativeByPtr (ptr: nativeptr<viewParms_t>) (view: ViewParms) =
        let mutable native = NativePtr.read ptr
        
        OrientationR.toNativeByPtr &&native.or' view.Orientation
        OrientationR.toNativeByPtr &&native.world view.World
        Vector3.toNativeByPtr &&native.pvsOrigin view.PvsOrigin
        native.isPortal <- bool.toNative view.IsPortal
        native.isMirror <- bool.toNative view.IsMirror
        native.frameSceneNum <- view.FrameSceneId
        native.frameCount <- view.FrameCount
        native.portalPlane <- Plane.toNative view.PortalPlane
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
    let inline ofNative (ptr: nativeptr<byte>) =
        {
            R = NativePtr.get ptr 0;
            G = NativePtr.get ptr 1;
            B = NativePtr.get ptr 2;
            A = NativePtr.get ptr 3;
        }

    let inline toNativeByPtr (ptr: nativeptr<byte>) (rgba: Rgba) =
        NativePtr.set ptr 0 rgba.R
        NativePtr.set ptr 1 rgba.G
        NativePtr.set ptr 2 rgba.B
        NativePtr.set ptr 3 rgba.A

module DrawVertex =
    let inline ofNative (native: drawVert_t) =
        {
            DrawVertex.Vertex = Vector3.ofNative native.xyz;
            St0 = NativePtr.get &&native.st 0;
            St1 = NativePtr.get &&native.st 1;
            Lightmap0 = NativePtr.get &&native.lightmap 0;
            Lightmap1 = NativePtr.get &&native.lightmap 1;
            Normal = Vector3.ofNative native.normal;
            Color = Rgba.ofNative &&native.color;
        }

module PolyVertex =
    let inline ofNative (native: polyVert_t) =
        {
            Vertex = Vector3.ofNative native.xyz;
            St0 = NativePtr.get &&native.st 0;
            St1 = NativePtr.get &&native.st 1;
            Modulate0 = NativePtr.get &&native.modulate 0;
            Modulate1 = NativePtr.get &&native.modulate 1;
            Modulate2 = NativePtr.get &&native.modulate 2;
            Modulate3 = NativePtr.get &&native.modulate 3;
        }

module FaceVertexPoints =
    let inline ofNative (ptr: nativeptr<single>) =
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
    let inline ofNativeFace (native: srfSurfaceFace_t) =
        {
            Plane = Plane.ofNative native.plane;
            DlightBit0 = NativePtr.get &&native.dlightBits 0;
            DlightBit1 = NativePtr.get &&native.dlightBits 1;
            PointCount = native.numPoints;
            IndexCount = native.numIndices;
            OffsetIndices = native.ofsIndices;
            Points = FaceVertexPoints.ofNative &&native.points;
        }
        |> Face

    let inline ofNativeTriangles (native: srfTriangles_t) =
        {
            DlightBit0 = NativePtr.get &&native.dlightBits 0;
            DlightBit1 = NativePtr.get &&native.dlightBits 1;
            Bounds = Bounds.ofNative &&native.bounds;
            LocalOrigin = Vector3.ofNative native.localOrigin;
            Radius = native.radius;
            Indices = List.ofNativePtrArray native.numIndexes native.indexes;
            Vertices = List.ofNativePtrArrayMap native.numVerts DrawVertex.ofNative native.verts;
        }
        |> Triangles

    let inline ofNativePoly (native: srfPoly_t) =
        {
            ShaderHandle = native.hShader;
            FogIndex = native.fogIndex;
            Vertices = List.ofNativePtrArrayMap native.numVerts PolyVertex.ofNative native.verts;
        }
        |> Poly

    let inline ofNativeDisplayList (native: srfDisplayList_t) =
        {
            ListId = native.listNum;
        }
        |> DisplayList

    let inline ofNativeGridMesh (native: srfGridMesh_t) =
        {
            DlightBit0 = NativePtr.get &&native.dlightBits 0;
            DlightBit1 = NativePtr.get &&native.dlightBits 1;
            MeshBounds = Bounds.ofNative &&native.meshBounds;
            LocalOrigin = Vector3.ofNative native.localOrigin;
            MeshRadius = native.meshRadius;
            LodOrigin = Vector3.ofNative native.lodOrigin;
            LodRadius = native.lodRadius;
            LodFixed = native.lodFixed;
            LodStitched = native.lodStitched;
            Width = native.width;
            Height = native.height;
            WidthLodError = List.ofNativePtrArray native.width native.widthLodError;
            HeightLodError = List.ofNativePtrArray native.height native.heightLodError;
            Vertex = DrawVertex.ofNative native.verts;
        }
        |> Grid

    let inline ofNativeFlare (native: srfFlare_t) =
        {
            Origin = Vector3.ofNative native.origin;
            Normal = Vector3.ofNative native.normal;
            Color = Vector3.ofNative native.color;
        }
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
            LightingOrigin = Vector3.ofNative native.lightingOrigin;
            ShadowPlane = native.shadowPlane;
            Axis = Axis.ofNative &&native.axis;
            HasNonNormalizedAxes = Convert.ToBoolean native.nonNormalizedAxes;
            Origin = Vector3.ofNative native.origin;
            Frame = native.frame;
            OldOrigin = Vector3.ofNative native.oldorigin;
            OldFrame = native.oldframe;
            BackLerp = native.backlerp;
            SkinId = native.skinNum;
            CustomSkinHandle = native.customSkin;
            CustomShaderHandle = native.customShader;
            ShaderRgba = Rgba.ofNative &&native.shaderRGBA;
            ShaderTextureCoordinate = Vector2.ofNative native.shaderTexCoord;
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
        native.nonNormalizedAxes <- bool.toNative entity.HasNonNormalizedAxes
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
    let inline ofNative (native: trRefEntity_t) =
        {
            Entity = RefEntity.ofNative native.e;
            AxisLength = native.axisLength;
            NeedDlights = Convert.ToBoolean native.needDlights;
            IsLightingCalculated = Convert.ToBoolean native.lightingCalculated;
            LightDirection = Vector3.ofNative native.lightDir;
            AmbientLight = Vector3.ofNative native.ambientLight;
            AmbientLightInt = native.ambientLightInt;
            DirectedLight = Vector3.ofNative native.directedLight;
        }

    let inline toNativeByPtr (ptr: nativeptr<trRefEntity_t>) (refEntity: TrRefEntity) =
        let mutable native = NativePtr.read ptr
        
        RefEntity.toNativeByPtr &&native.e refEntity.Entity
        native.axisLength <- refEntity.AxisLength
        native.needDlights <- bool.toNative refEntity.NeedDlights
        native.lightingCalculated <- bool.toNative refEntity.IsLightingCalculated
        Vector3.toNativeByPtr &&native.lightDir refEntity.LightDirection
        Vector3.toNativeByPtr &&native.ambientLight refEntity.AmbientLight
        native.ambientLightInt <- refEntity.AmbientLightInt
        Vector3.toNativeByPtr &&native.directedLight refEntity.DirectedLight
        
        NativePtr.write ptr native

    module Option =
        let inline ofNativePtr (ptr: nativeptr<trRefEntity_t>) =
            Some << ofNative <| NativePtr.read ptr

module Refdef =
    let inline ofNative (native: refdef_t) =
        {
            X = native.x;
            Y = native.y;
            Width = native.width;
            Height = native.height;
            ViewOrigin = Vector3.ofNative native.vieworg;
            ViewAxis = Axis.ofNative &&native.viewaxis;
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
    let inline ofNative (native: dlight_t) =
        {
            Origin = Vector3.ofNative native.origin;
            Color = Vector3.ofNative native.color;
            Radius = native.radius;
            Transformed = Vector3.ofNative native.transformed;
            Additive = native.additive;
        }

module TrRefdef =
    let inline ofNative (native: trRefdef_t) =
        {
            X = native.x;
            Y = native.y;
            Width = native.width;
            Height = native.height;
            ViewOrigin = Vector3.ofNative native.vieworg;
            ViewAxis = Axis.ofNative &&native.viewaxis;
            Time = native.time;
            RdFlags = enum<RdFlags> (native.rdflags);

            AreaMask = ByteString.ofNativePtr 32 &&native.areamask;
            HasAreaMaskModified = native.areamaskModified;

            FloatTime = native.floatTime;
            Text = List.ofNativePtrArrayMap 8 (fun x -> "") &&native.text;
            Entities = List.ofNativePtrArrayMap native.num_entities (fun x -> TrRefEntity.ofNative x) native.enities;
            Dlights = List.ofNativePtrArrayMap native.num_delights (fun x -> Dlight.ofNative x) native.dlights;
            Polys = List.ofNativePtrArrayMap native.numPolys (fun x -> Surface.ofNativePoly x) native.polys;
            DrawSurfaces = List.ofNativePtrArrayMap native.numDrawSurfs (fun x -> DrawSurface.ofNative x) native.drawSurfs;
        }

// TODO: This will need more work over time.
module TrGlobals =
    let inline ofNative (native: trGlobals_t) =
        {
            CurrentEntity = TrRefEntity.Option.ofNativePtr native.currentEntity;
            CurrentEntityId = native.currentEntityNum;
            ViewParms = ViewParms.ofNative native.viewParms;
            Refdef = TrRefdef.ofNative native.refdef;
            Orientation = OrientationR.ofNative native.or';
        }

    let inline toNativeByPtr (ptr: nativeptr<trGlobals_t>) (tr: TrGlobals) =
        let mutable native = NativePtr.read ptr

        TrRefEntity.toNativeByPtr native.currentEntity tr.CurrentEntity.Value
        native.currentEntityNum <- tr.CurrentEntityId
        ViewParms.toNativeByPtr &&native.viewParms tr.ViewParms
        // TODO: Map TrRefDef - Property Refdef
        OrientationR.toNativeByPtr &&native.or' tr.Orientation

        NativePtr.write ptr native

