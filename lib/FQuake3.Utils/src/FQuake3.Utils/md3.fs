module FQuake3.Md3

open System.Runtime.InteropServices
open FSharp.Game.Math
open FQuake3.Math

#nowarn "9"

/// Md3Header
type Md3Header = {
    Ident: string
    Version: int
    Name: string
    Flags: int
    FrameCount: int
    TagCount: int
    SurfaceCount: int
    SkinCount: int
    FramesOffset: int
    TagsOffset: int
    SurfacesOffset: int
    EofOffset: int }

/// Md3Frame
type Md3Frame = {
    Bounds: Bounds
    LocalOrigin: vec3
    Radius: single
    Name: string }

/// Md3Tag
type Md3Tag = {
    Name: string
    Origin: vec3
    Axis: Axis }

/// Md3Shader
type Md3Shader = {
    Name: string
    ShaderId: int }

/// Md3Triangle
[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Md3Triangle =
    val x : int
    val y : int
    val z : int

    new (x, y, z) = { x = x; y = y; z = z }

/// Md3St
[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Md3St =
    val st : vec2

    new (st) = { st = st }

let internal ``2 * PI / 255`` = 2.f * Math.PI / 255.f 
/// Md3Vertex
/// Also known as XyzNormal
[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Md3Vertex =
    val x : int16
    val y : int16
    val z : int16
    val Zenith : byte
    val Azimuth : byte

    new (x, y, z, zenith, azimuth) = { x = x; y = y; z = z; Zenith = zenith; Azimuth = azimuth }

    member this.lat = single this.Zenith * ``2 * PI / 255``
    member this.lng = single this.Azimuth * ``2 * PI / 255``

/// Md3SurfaceHeader
type Md3SurfaceHeader = {
    Ident: string
    Name: string
    Flags: int
    FrameCount: int
    ShaderCount: int
    VertexCount: int
    TriangleCount: int
    TrianglesOffset: int
    ShadersOffset: int
    StOffset: int
    VerticesOffset: int
    EndOffset: int }

/// Md3Surface
type Md3Surface = {
    Header: Md3SurfaceHeader
    Shaders: Md3Shader []
    Triangles: Md3Triangle []
    St: Md3St []
    Vertices: Md3Vertex [] }

/// Md3
type Md3 = {
    Header: Md3Header
    Frames: Md3Frame []
    Tags: Md3Tag []
    Surfaces: Md3Surface [] }
