namespace OpenFK.UnitTests

open System
open NUnit.Framework
open FsUnit
open OpenFK.Math

[<TestFixture>]
type MatrixTests () =

    [<Test>]
    member this.TestMe () =
        let m = Matrix16 ()

        for i = 0 to 1000000 do
            let x = m * m
            ()
        ()


