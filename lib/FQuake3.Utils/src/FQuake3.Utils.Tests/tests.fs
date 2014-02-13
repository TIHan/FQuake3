module FQuake3.Utils.Tests

open System.IO
open FsUnit
open NUnit.Framework
open FQuake3.Md3
open FQuake3.Utils.Md3

[<Test>]
let ``with an arachnatron head md3, parsing should succeed and have a valid surface.`` () = 
    let bytes = File.ReadAllBytes ("../../Resources/models/players/arachnatron/head.md3")
    let md3 = Md3.parse bytes
    md3.Surfaces.Length |> should be (greaterThan 0)
    md3.Surfaces.[0].Header.Ident |> should equal "IDP3"

[<Test>]
let ``with an arachnatron upper md3, parsing should succeed and have a valid surface.`` () = 
    let bytes = File.ReadAllBytes ("../../Resources/models/players/arachnatron/upper.md3")
    let md3 = Md3.parse bytes
    md3.Surfaces.Length |> should be (greaterThan 0)
    md3.Surfaces.[0].Header.Ident |> should equal "IDP3"

[<Test>]
let ``with an arachnatron lower md3, parsing should succeed and have a valid surface.`` () = 
    let bytes = File.ReadAllBytes ("../../Resources/models/players/arachnatron/lower.md3")
    let md3 = Md3.parse bytes
    md3.Surfaces.Length |> should be (greaterThan 0)
    md3.Surfaces.[0].Header.Ident |> should equal "IDP3"