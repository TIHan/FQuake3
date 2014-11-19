﻿module internal FQuake3.Utils.Internal.PickleMd3

open FSharp.LitePickler.Pickle
open FSharp.Game.Math
open FQuake3.Md3
open FQuake3.Math

let p_vec2 : Pickle<vec2> =
    fun stream v ->
        stream.Write v.X
        stream.Write v.Y

let p_vec3 : Pickle<vec3> =
    fun stream v ->
        stream.Write v.X
        stream.Write v.Y
        stream.Write v.Z

let p_frame =
    p_pipe5 p_vec3 p_vec3 p_vec3 p_single (p_string 16 StringKind.EightBit) <|
    fun (x: Md3Frame) -> 
        x.Bounds.Min,
        x.Bounds.Max,
        x.LocalOrigin,
        x.Radius,
        x.Name

let p_header =
    p_pipe12
        (p_string 4 StringKind.EightBit)
        p_int32
        (p_string 64 StringKind.EightBit)
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32 <|
    fun (x: Md3Header) -> 
        x.Ident,
        x.Version,
        x.Name,
        x.Flags,
        x.FrameCount,
        x.TagCount,
        x.SurfaceCount,
        x.SkinCount,
        x.FramesOffset,
        x.TagsOffset,
        x.SurfacesOffset,
        x.EofOffset

let p_tag : Pickle<Md3Tag> =
    p_pipe5 (p_string 64 StringKind.EightBit) p_vec3 p_vec3 p_vec3 p_vec3 <|
    fun x -> 
        x.Name,
        x.Origin,
        x.Axis.X,
        x.Axis.Y,
        x.Axis.Z

let p_shader : Pickle<Md3Shader> =
    p_pipe2
        (p_string 64 StringKind.EightBit)
        p_int32 <|
    fun x -> 
        x.Name,
        x.ShaderId

let p_triangle : Pickle<Md3Triangle> =
    p_pipe3
        p_int32
        p_int32
        p_int32 <|
    fun x -> 
        x.x, 
        x.y, 
        x.z

let p_st : Pickle<Md3St> = p_vec2 |>> fun x -> x.st
 
let p_vertex : Pickle<Md3Vertex> =
    p_pipe5 p_int16 p_int16 p_int16 p_byte p_byte <|
    fun x -> 
        x.x, 
        x.y, 
        x.z, 
        x.Zenith, 
        x.Azimuth

let p_surfaceHeader : Pickle<Md3SurfaceHeader> =
    p_pipe12
        (p_string 4 StringKind.EightBit)
        (p_string 64 StringKind.EightBit)
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32
        p_int32 <|
    fun x ->
        x.Ident,
        x.Name,
        x.Flags,
        x.FrameCount,
        x.ShaderCount,
        x.VertexCount,
        x.TriangleCount,
        x.TrianglesOffset,
        x.ShadersOffset,
        x.StOffset,
        x.VerticesOffset,
        x.EndOffset
       
let p_frames count offset : Pickle<Md3Frame[]> =
    p_skipBytes offset >>. 
    (p_array count p_frame, fun x -> x)

let p_tags count offset : Pickle<Md3Tag[]> =
    p_skipBytes offset >>. 
    (p_array count p_tag, fun x -> x)

let p_surface : Pickle<Md3Surface> =
    fun stream x ->
        let header = x.Header
        p_lookAhead p_surfaceHeader stream header
        p_lookAhead (p_skipBytes header.TrianglesOffset >>. (p_array header.TriangleCount p_triangle, id)) stream x.Triangles
        p_lookAhead (p_skipBytes header.ShadersOffset >>. (p_array header.ShaderCount p_shader, id)) stream x.Shaders
        p_lookAhead (p_skipBytes header.StOffset >>. (p_array header.VertexCount p_st, id)) stream x.St
        p_lookAhead (p_skipBytes header.VerticesOffset >>. (p_array (header.VertexCount * header.FrameCount) p_vertex, id)) stream x.Vertices

let p_surfaces count offset : Pickle<Md3Surface[]> =
    let f =
        fun stream xs ->
            xs
            |> Array.iteri (fun i x ->
                p_surface stream x
            
                if i + 1 <> count then
                    p_skipBytes x.Header.EndOffset stream x)

    p_skipBytes offset >>.
    (f, id)

let p_md3 : Pickle<_> =
    (p_lookAhead p_header, fun x -> x.Header) >>= fun header ->
    p_pipe3
        (p_lookAhead <| p_frames header.FrameCount header.FramesOffset)
        (p_lookAhead <| p_tags header.TagCount header.TagsOffset)
        (p_lookAhead <| p_surfaces header.SurfaceCount header.SurfacesOffset) <|
    fun (x: Md3) -> 
        x.Frames,
        x.Tags,
        x.Surfaces

