module FQuake3.Utils.Benchmark.Main

open System
open System.Diagnostics

open FSharp.Game.Math

let benchmark name iterations f =
    let rec benchmark results = function
        | n when n = iterations -> List.reduce (fun x y -> x + y) results / iterations
        | n ->
            let stopwatch = Stopwatch.StartNew ()
            f ()
            stopwatch.Stop ()
            benchmark (int stopwatch.ElapsedMilliseconds :: results) (n + 1)

    printfn "%s: %A" name <| benchmark [] 0

[<EntryPoint>]
let main args =
    benchmark "4x4 Matrix Multiplication" 10 (fun () ->
        let m1 = mat4 (1337.f)
        let m2 = mat4 (1337.f)
        let mutable m = mat4 (0.f)
        for i = 1 to 100000 do
            m <- m1 * m2)
    0

