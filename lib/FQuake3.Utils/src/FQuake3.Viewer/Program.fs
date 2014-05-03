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

    Native.App.loadShaders (vp, fp)

    vh.Free ()
    fh.Free ()
    // End Fixed

let makeVao () = Native.App.generateVao ()

let makeVbo (data: Triangle []) = 
    let h = GCHandle.Alloc (data, GCHandleType.Pinned)
    let p = h.AddrOfPinnedObject ()
    let vbo = Native.App.generateVbo (data.Length * sizeof<Triangle>, p)
    h.Free ()
    vbo

let drawVbo (data: Triangle []) vbo =
    let h = GCHandle.Alloc (data, GCHandleType.Pinned)
    let p = h.AddrOfPinnedObject ()
    Native.App.drawVbo (data.Length * sizeof<Triangle>, p, vbo)
    h.Free ()

let init () = Native.App.init ()

let exit app = Native.App.exit app

// http://wiki.libsdl.org/SDL_EventType
let shouldQuit () = Native.App.shouldQuit ()

let trigcoord = (1.f / 64.f)

[<EntryPoint>]
let main args =
    let bytes = File.ReadAllBytes ("../../../FQuake3.Utils.Tests/Resources/models/players/arachnatron/head.md3")
    let md3 = Md3.parse bytes
    let surface = md3.Surfaces.[0]
    let data = surface.Vertices |> Array.map (fun x -> Triangle (single x.x * trigcoord, single x.y * trigcoord, single x.z * trigcoord))

    let dater = surface.Triangles |> Array.map (fun x -> Triangle (data.[x.x].X, data.[x.y].Y, data.[x.z].Z))


    let _,dater =
        dater
        |> Array.fold (fun (i, data) x ->
            match i % 3 = 0 with
            | true -> (i + 1, Array.append data [|x;x|])
            | _ -> (i + 1, Array.append data [|x|])
        ) (1, [||])

    printfn "Frame Count: %i" md3.Frames.Length
    printfn "Triangle Count: %i" surface.Triangles.Length
    printfn "Vertex Count: %i" surface.Vertices.Length

    let app = init ()

    let vao = makeVao ()
    let vbo = makeVbo dater

    loadShaders ()

    while not <| shouldQuit () do
        clear ()
        depthTest ()
        drawVbo dater vbo
        draw app

    exit (app)
