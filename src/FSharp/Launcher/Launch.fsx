module Launcher.Runner

#r "Engine.dll"
#r "Engine.Renderer.dll"
#r "CGame.dll" // temporary
#r "FSharpx.Core.dll"

open Engine.System

let start () =
    async {
        System.Start ()
    } |> Async.Start;