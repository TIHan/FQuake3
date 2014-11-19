module internal FQuake3.Utils.Internal.PickleMd3

open FSharp.LitePickler.Pickle
open FSharp.Game.Math
open FQuake3.Md3
open FQuake3.Math

let p_vec2 : Pickle<vec2> =
    fun v stream ->
        stream.Write v.X
        stream.Write v.Y

let p_vec3 : Pickle<vec3> =
    fun v stream ->
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

let p_tag =
    p_pipe5 (p_string 64 StringKind.EightBit) p_vec3 p_vec3 p_vec3 p_vec3 <|
    fun (x: Md3Tag) -> 
        x.Name,
        x.Origin,
        x.Axis.X,
        x.Axis.Y,
        x.Axis.Z

let p_shader =
    p_pipe2
        (p_string 64 StringKind.EightBit)
        p_int32 <|
    fun (x: Md3Shader) -> 
        x.Name,
        x.ShaderId

let p_triangle =
    p_pipe3
        p_int32
        p_int32
        p_int32 <|
    fun (x: Md3Triangle) -> 
        x.x, 
        x.y, 
        x.z

let p_st = p_vec2 |>> fun (x: Md3St) -> x.st
 
let p_vertex =
    p_pipe5 p_int16 p_int16 p_int16 p_byte p_byte <|
    fun (x: Md3Vertex) -> 
        x.x, 
        x.y, 
        x.z, 
        x.Zenith, 
        x.Azimuth

let p_surfaceHeader =
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
    fun (x: Md3SurfaceHeader) ->
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
       
let p_frames count offset =
    p_skipBytes offset >>. p_array count p_frame <| ()

let p_tags count offset =
    p_skipBytes offset >>. p_array count p_tag <| ()

let p_surface : Pickle<Md3Surface> =
    fun x stream ->
        let header = x.Header
        p_lookAhead p_surfaceHeader header stream
        p_lookAhead (p_skipBytes header.TrianglesOffset >>. p_array header.TriangleCount p_triangle <| ()) x.Triangles stream
        p_lookAhead (p_skipBytes header.ShadersOffset >>. p_array header.ShaderCount p_shader <| ()) x.Shaders stream
        p_lookAhead (p_skipBytes header.StOffset >>. p_array header.VertexCount p_st <| ()) x.St stream
        p_lookAhead (p_skipBytes header.VerticesOffset >>. p_array (header.VertexCount * header.FrameCount) p_vertex <| ()) x.Vertices stream

let p_surfaces count offset =
    let f =
        fun xs stream ->
            xs
            |> Array.iteri (fun i x ->
                p_surface x stream
            
                if i + 1 <> count then
                    p_skipBytes x.Header.EndOffset x stream)

    p_skipBytes offset >>. f <| ()

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

