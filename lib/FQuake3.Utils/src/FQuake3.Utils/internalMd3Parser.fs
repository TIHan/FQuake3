module internal FQuake3.Utils.Internal.Md3Parser

open FSharp.LitePickler.Unpickle
open FSharp.Game.Math
open FQuake3.Md3
open FQuake3.Math

let u_vec2 : Unpickle<vec2> =
    fun stream ->
        vec2 (
            stream.Read<single> (),
            stream.Read<single> ())

let u_vec3 : Unpickle<vec3> =
    fun stream ->
        vec3 (
            stream.Read<single> (),
            stream.Read<single> (),
            stream.Read<single> ())

let u_frame =
    u_pipe5 u_vec3 u_vec3 u_vec3 u_single (u_string 16) <|
    fun mins maxs localOrigin radius name ->
        {
        Bounds = Bounds (mins, maxs)
        LocalOrigin = localOrigin
        Radius = radius
        Name = name }

let u_header =
    u_pipe12
        (u_string 4)
        u_int32
        (u_string 64)
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32 <|
    fun ident version name flags frameCount tagCount surfaceCount skinCount framesOffset tagsOffset surfacesOffset eofOffset ->
        {
        Ident = ident
        Version = version
        Name = name
        Flags = flags
        FrameCount = frameCount
        TagCount = tagCount
        SurfaceCount = surfaceCount
        SkinCount = skinCount
        FramesOffset = framesOffset
        TagsOffset = tagsOffset
        SurfacesOffset = surfacesOffset
        EofOffset = eofOffset }

let u_tag =
    u_pipe5 (u_string 64) u_vec3 u_vec3 u_vec3 u_vec3 <|
    fun name origin axisX axisY axisZ ->
        { Name = name; Origin = origin; Axis = Axis (axisX, axisY, axisZ) }

let pshader =
    u_pipe2 (u_string 64) u_int32 <|
    fun name shaderId -> { Name = name; ShaderId = shaderId }

let ptriangle =
    u_pipe3 u_int32 u_int32 u_int32 <|
    fun x y z -> Md3Triangle (x, y, z)

let u_st = u_vec2 |>> fun x -> Md3St (x)
 
let u_vertex : Unpickle<Md3Vertex> =
    u_pipe5 u_int16 u_int16 u_int16 u_byte u_byte <|
    fun x y z zenith azimuth -> Md3Vertex (x, y, z, zenith, azimuth)

let u_surfaceHeader =
    u_pipe12
        (u_string 4)
        (u_string 64)
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32
        u_int32 <|
    fun ident name flags frameCount shaderCount vertexCount triangleCount trianglesOffset shadersOffset stOffset verticesOffset endOffset ->
        {
        Ident = ident
        Name = name
        Flags = flags
        FrameCount = frameCount
        ShaderCount = shaderCount
        VertexCount = vertexCount
        TriangleCount = triangleCount
        TrianglesOffset = trianglesOffset
        ShadersOffset = shadersOffset
        StOffset = stOffset
        VerticesOffset = verticesOffset
        EndOffset = endOffset }

let u_frames count offset =
    u_skipBytes offset >>.
    u_array count u_frame |>> fun x -> x

let u_tags count offset =
    u_skipBytes offset >>.
    u_array count u_tag |>> fun x -> x

let u_surfaces count offset =
    u_skipBytes offset >>.
    fun stream ->
    Array.init count (fun i ->
        let header = u_lookAhead u_surfaceHeader stream
        let triangles = u_lookAhead (u_skipBytes header.TrianglesOffset >>. u_array header.TriangleCount ptriangle) stream
        let shaders = u_lookAhead (u_skipBytes header.ShadersOffset >>. u_array header.ShaderCount pshader) stream
        let st = u_lookAhead (u_skipBytes header.StOffset >>. u_array header.VertexCount u_st) stream
        let vertices = u_lookAhead (u_skipBytes header.VerticesOffset >>. u_array (header.VertexCount * header.FrameCount) u_vertex) stream

        if i + 1 <> count then
            u_skipBytes header.EndOffset stream |> ignore
        {
        Header = header
        Shaders = shaders
        Triangles = triangles
        St = st
        Vertices = vertices })

let u_md3 : Unpickle<_> =
    u_lookAhead u_header >>= fun header ->
    u_pipe3
        (u_lookAhead <| u_frames header.FrameCount header.FramesOffset)
        (u_lookAhead <| u_tags header.TagCount header.TagsOffset)
        (u_lookAhead <| u_surfaces header.SurfaceCount header.SurfacesOffset) <|
    fun frames tags surfaces ->
        {
        Header = header
        Frames = frames
        Tags = tags
        Surfaces = surfaces }

