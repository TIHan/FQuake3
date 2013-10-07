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
    let inline toNativeByPtr (ptr: nativeptr<vec3_t>) (bounds: Bounds) =
        let mutable nativeX = NativePtr.get ptr 0
        let mutable nativeY = NativePtr.get ptr 1

        Vector3.toNativeByPtr &&nativeX bounds.Bound0
        Vector3.toNativeByPtr &&nativeY bounds.Bound1

        NativePtr.set ptr 0 nativeX
        NativePtr.set ptr 1 nativeY

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

    let inline toNativeByPtr (ptr: nativeptr<orientationr_t>) (orientation: OrientationR) =
        let mutable native = NativePtr.read ptr

        Vector3.toNativeByPtr &&native.origin orientation.Origin
        Axis.toNativeByPtr &&native.axis orientation.Axis
        Vector3.toNativeByPtr &&native.viewOrigin orientation.ViewOrigin
        Matrix16.toNativeByPtr &&native.modelMatrix orientation.ModelMatrix


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
        Matrix16.toNativeByPtr &&native.projectionMatrix view.ProjectionMatrix
        Frustum.toNativeByPtr &&native.frustum view.Frustum
        Bounds.toNativeByPtr &&native.visBounds view.VisibilityBounds
        native.zFar <- view.ZFar

        NativePtr.write ptr native

module Rgba =
    let inline toNativeByPtr (ptr: nativeptr<byte>) (rgba: Rgba) =
        NativePtr.set ptr 0 rgba.R
        NativePtr.set ptr 1 rgba.G
        NativePtr.set ptr 2 rgba.B
        NativePtr.set ptr 3 rgba.A

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
            LightDirection = NativePtr.toStructure &&native.lightDir;
            AmbientLight = NativePtr.toStructure &&native.ambientLight;
            AmbientLightInt = native.ambientLightInt;
            DirectedLight = NativePtr.toStructure &&native.directedLight;
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
            ViewOrigin = NativePtr.toStructure &&native.vieworg;
            ViewAxis = NativePtr.toStructure &&native.viewaxis;
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
        Dlight (
            NativePtr.toStructure &&native.origin,
            NativePtr.toStructure &&native.color,
            native.radius,
            NativePtr.toStructure &&native.transformed,
            native.additive
        )

module TrRefdef =
    let inline ofNative (native: trRefdef_t) =
        {
            X = native.x;
            Y = native.y;
            Width = native.width;
            Height = native.height;
            ViewOrigin = NativePtr.toStructure &&native.vieworg;
            ViewAxis = NativePtr.toStructure &&native.viewaxis;
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

