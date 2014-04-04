module internal FQuake3.Utils.Internal.Md3Parser

open FSharp.LiteParsec
open FSharp.Game.Math
open FQuake3.Md3
open FQuake3.Math

let anyVec2 : Parser<vec2> =
    fun stream ->
        vec2 (
            stream.Read<single> (),
            stream.Read<single> ())

let anyVec3 : Parser<vec3> =
    fun stream ->
        vec3 (
            stream.Read<single> (),
            stream.Read<single> (),
            stream.Read<single> ())

let pframe =
    pipe5 anyVec3 anyVec3 anyVec3 anySingle (anyString 16) <|
    fun mins maxs localOrigin radius name ->
        {
        Bounds = Bounds (mins, maxs)
        LocalOrigin = localOrigin
        Radius = radius
        Name = name }

let pheader =
    pipe12
        (anyString 4)
        anyInt32
        (anyString 64)
        anyInt32
        anyInt32
        anyInt32
        anyInt32
        anyInt32
        anyInt32
        anyInt32
        anyInt32
        anyInt32 <|
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

let ptag =
    pipe5 (anyString 64) anyVec3 anyVec3 anyVec3 anyVec3 <|
    fun name origin axisX axisY axisZ ->
        { Name = name; Origin = origin; Axis = Axis (axisX, axisY, axisZ) }

let pshader =
    pipe2 (anyString 64) anyInt32 <|
    fun name shaderId -> { Name = name; ShaderId = shaderId }

let ptriangle =
    pipe3 anyInt32 anyInt32 anyInt32 <|
    fun x y z -> Md3Triangle (x, y, z)

let pst = anyVec2 |>> fun x -> Md3St (x)

let ``2 * PI / 255`` = 2.f * Math.PI / 255.f  
let pvertex : Parser<Md3Vertex> =
    pipe5 anyInt16 anyInt16 anyInt16 anyByte anyByte <|
    fun x y z zenith azimuth -> Md3Vertex (x, y, z, single zenith * ``2 * PI / 255``, single azimuth * ``2 * PI / 255``)

let psurfaceHeader =
    pipe12
        (anyString 4)
        (anyString 64)
        anyInt32
        anyInt32
        anyInt32
        anyInt32
        anyInt32
        anyInt32
        anyInt32
        anyInt32
        anyInt32
        anyInt32 <|
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

let pframes count offset =
    skipAnyBytes offset >>.
    parray count pframe |>> fun x -> x

let ptags count offset =
    skipAnyBytes offset >>.
    parray count ptag |>> fun x -> x

let psurfaces count offset =
    skipAnyBytes offset >>.
    fun stream ->
    Array.init count (fun i ->
        let header = lookAhead psurfaceHeader stream
        let triangles = lookAhead (skipAnyBytes header.TrianglesOffset >>. parray header.TriangleCount ptriangle) stream
        let shaders = lookAhead (skipAnyBytes header.ShadersOffset >>. parray header.ShaderCount pshader) stream
        let st = lookAhead (skipAnyBytes header.StOffset >>. parray header.VertexCount pst) stream
        let vertices = lookAhead (skipAnyBytes header.VerticesOffset >>. parray (header.VertexCount * header.FrameCount) pvertex) stream

        if i + 1 <> count then
            skipAnyBytes header.EndOffset stream |> ignore
        {
        Header = header
        Shaders = shaders
        Triangles = triangles
        St = st
        Vertices = vertices })

let pmd3 : Parser<_> =
    lookAhead pheader >>= fun header ->
    pipe3
        (lookAhead <| pframes header.FrameCount header.FramesOffset)
        (lookAhead <| ptags header.TagCount header.TagsOffset)
        (lookAhead <| psurfaces header.SurfaceCount header.SurfacesOffset) <|
    fun frames tags surfaces ->
        {
        Header = header
        Frames = frames
        Tags = tags
        Surfaces = surfaces }

