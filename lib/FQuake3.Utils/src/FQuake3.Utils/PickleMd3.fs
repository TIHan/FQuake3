module internal FQuake3.Utils.Internal.PickleMd3

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

let p_frame : Pickle<Md3Frame> =
    p_pipe5 
        (fun x -> x.Bounds.Min, x.Bounds.Max, x.LocalOrigin, x.Radius, x.Name)
        p_vec3
        p_vec3
        p_vec3
        p_single
        (p_string 16 StringKind.EightBit)

let p_header : Pickle<Md3Header> =
    p_pipe12
        (fun x -> 
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
            x.EofOffset)
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
        p_int32

let p_tag : Pickle<Md3Tag> =
    p_pipe5
        (fun x -> x.Name, x.Origin, x.Axis.X, x.Axis.Y, x.Axis.Z)
        (p_string 64 StringKind.EightBit)
        p_vec3
        p_vec3
        p_vec3
        p_vec3

let p_shader : Pickle<Md3Shader> =
    p_pipe2
        (fun x -> x.Name, x.ShaderId)
        (p_string 64 StringKind.EightBit)
        p_int32

let p_triangle : Pickle<Md3Triangle> =
    p_pipe3
        (fun x -> x.x, x.y, x.z)
        p_int32
        p_int32
        p_int32

let p_st : Pickle<Md3St> = p_vec2 |>> (fun x -> x.st)
 
let p_vertex : Pickle<Md3Vertex> =
    p_pipe5
        (fun x -> x.x, x.y, x.z, x.Zenith, x.Azimuth)
        p_int16
        p_int16
        p_int16
        p_byte
        p_byte

let p_surfaceHeader : Pickle<Md3SurfaceHeader> =
    p_pipe12
        (fun x ->
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
            x.EndOffset)
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
        p_int32
       
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

let p_md3 : Pickle<Md3> =
    (p_lookAhead p_header, fun x -> x.Header) >>= fun header ->
    p_pipe3
        (fun x -> x.Frames, x.Tags, x.Surfaces)
        (p_lookAhead <| p_frames header.FrameCount header.FramesOffset)
        (p_lookAhead <| p_tags header.TagCount header.TagsOffset)
        (p_lookAhead <| p_surfaces header.SurfaceCount header.SurfacesOffset)

