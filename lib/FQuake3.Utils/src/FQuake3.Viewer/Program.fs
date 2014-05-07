module FQuake3.Viewer.Main

#nowarn "9"
#nowarn "51"

open System.Threading;
open System.Security
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.IO

open FSharp.Game.Math
open FQuake3.Utils
open Ferop

type Native = FeropProvider<"FQuake3.Viewer.Native", "bin/Release">

[<Struct>]
type Triangle =
    val X : single
    val Y : single
    val Z : single

    new (x, y, z) = { X = x; Y = y; Z = z }

let clear () = Native.App.clear ()

let depthTest () = Native.App.depthTest ()

let draw app = Native.App.draw app

let loadShaders () =
    let mutable vertexFile = System.Text.Encoding.ASCII.GetBytes (File.ReadAllText ("v.vertex"))
    let mutable fragmentFile = System.Text.Encoding.ASCII.GetBytes (File.ReadAllText ("f.fragment"))

    // Fixed
    let vh = GCHandle.Alloc (vertexFile, GCHandleType.Pinned)
    let vp = vh.AddrOfPinnedObject ()

    let fh = GCHandle.Alloc (fragmentFile, GCHandleType.Pinned)
    let fp = fh.AddrOfPinnedObject ()

    let programId = Native.App.loadShaders (vp, fp)

    vh.Free ()
    fh.Free ()
    // End Fixed
    programId

let makeVao () = Native.App.generateVao ()

let makeVbo (data: Triangle []) = 
    let h = GCHandle.Alloc (data, GCHandleType.Pinned)
    let p = h.AddrOfPinnedObject ()
    let vbo = Native.App.generateVbo (data.Length * sizeof<Triangle>, p)
    h.Free ()
    vbo

let makeElementVbo (data: int []) = 
    let h = GCHandle.Alloc (data, GCHandleType.Pinned)
    let p = h.AddrOfPinnedObject ()
    let vbo = Native.App.generateElementVbo (data.Length * sizeof<int>, p)
    h.Free ()
    vbo

let init () = Native.App.init ()

let exit app = Native.App.exit app

// http://wiki.libsdl.org/SDL_EventType
let shouldQuit () = Native.App.shouldQuit ()

let enableUniformMVP (mvpId: uint32) (mvp: mat4) =
    let dh = GCHandle.Alloc (mvp, GCHandleType.Pinned)
    let dp = dh.AddrOfPinnedObject ()

    Native.App.enableUniformMVP (mvpId, dp)

    dh.Free () 

let trigcoord = (1.f / 64.f)

[<EntryPoint>]
let main args =
    let bytes = File.ReadAllBytes ("../../../FQuake3.Utils.Tests/Resources/models/players/arachnatron/lower.md3")
    let md3 = Md3.parse bytes
    let surface = md3.Surfaces.[0]

    let data = surface.Vertices |> Array.map (fun x -> Triangle (single x.x * trigcoord, single x.y * trigcoord, single x.z * trigcoord))
    let normals =
        surface.Vertices
        |> Array.map (fun x -> Triangle (cos x.lat * sin x.lng, sin x.lat * sin x.lng, cos x.lng))
    let indices = surface.Triangles |> Array.map (fun x -> [|x.x;x.y;x.z|]) |> Array.reduce (fun x y -> Array.append x y)

    let projection = Mat4.createPerspective 45.f<deg> (4.f / 3.f) 0.1f 10000.f |> Mat4.transpose 
    let view = Mat4.lookAt (vec3 (100.f, 0.f, 100.f)) (vec3 0.f) (vec3 (0.f, -1.f, 0.f)) |> Mat4.transpose 
    let model = Mat4.identity |> Mat4.transpose
    let pos = Mat4.createTranslation 0.f 0.f 0.f |> Mat4.transpose
    let rot = Mat4.createRotation (Vec3.right) 90.f<deg> |> Mat4.transpose
    let rot2 = Mat4.createRotation (Vec3.back) 45.f<deg> |> Mat4.transpose
    let mvp = projection * view * (pos * rot * rot2 * model) |> Mat4.transpose

    printfn "Frame Count: %i" md3.Frames.Length
    printfn "Triangle Count: %i" surface.Triangles.Length
    printfn "Vertex Count: %i" surface.Vertices.Length

    //let data = Array.append data normals

    let app = init ()

    let vao = makeVao ()
    let vbo = makeVbo data
    let ebo = makeElementVbo indices

    let programId = loadShaders ()
    let mvpId = Native.App.uniformMVP (programId)

    while not <| shouldQuit () do
        clear ()
        depthTest ()
        enableUniformMVP mvpId mvp
        Native.App.drawData (vbo, ebo, (surface.Triangles.Length * 3))
        draw app

    exit (app)
