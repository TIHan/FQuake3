open System
open System.IO
open Engine.System

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices
open Microsoft.FSharp.Compiler.Interactive.Shell

// Intialize output and input streams
let sbOut = new Text.StringBuilder ()
let sbErr = new Text.StringBuilder ()
let inStream = new StringReader ("")
let outStream = new StringWriter (sbOut)
let errStream = new StringWriter (sbErr)

// Build command line arguments & start FSI session
let argv = [| "fsi.exe" |]
let allArgs = Array.append argv [|"--noninteractive"|]

let fsiConfig =
    FsiEvaluationSession.GetDefaultConfiguration ()
let fsiSession =
    FsiEvaluationSession (
        fsiConfig,
        allArgs,
        inStream,
        outStream,
        errStream)

[<EntryPoint>]
let main argv = 
    async {

        let prevTime = 
            ref Unchecked.defaultof<DateTime>

        while true do
            do! Async.Sleep (10)

            let time = File.GetLastWriteTime "weapons.fsx"
            match prevTime.contents <> time with
            | false -> ()
            | _ ->
            prevTime.contents <- time

            printfn "Evaluating weapons.fsx"
            fsiSession.EvalScript ("weapons.fsx")
    }
    |> Async.Start

    System.Start ()
    0
