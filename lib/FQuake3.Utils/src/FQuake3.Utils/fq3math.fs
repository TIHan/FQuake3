module FQuake3.Math

#nowarn "9"

open System
open System.Runtime.InteropServices
open FSharp.Game.Math

/// Axis
[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Axis =
    val X : vec3
    val Y : vec3
    val Z : vec3

    new (x, y, z) = { X = x; Y = y; Z = z }

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.X | 1 -> this.Y | 2 -> this.Z
            | _ -> raise <| IndexOutOfRangeException ()

    member inline this.Set (?x: vec3, ?y: vec3, ?z: vec3) =
        Axis (
            (match x with | Some x -> x | None -> this.X),
            (match y with | Some y -> y | None -> this.Y),
            (match z with | Some z -> z | None -> this.Z))

/// Axis Module
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Axis =
    let zero =      Axis (Vec3.zero, Vec3.zero, Vec3.zero)
    let identity =  Axis (Vec3.right, Vec3.up, Vec3.forward)

/// Bounds
[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Bounds =
    val Min : vec3
    val Max : vec3 

    new (min, max) = { Min = min; Max = max }

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.Min | 1 -> this.Max
            | _ -> raise <| IndexOutOfRangeException ()

    override this.ToString () =
        sprintf "{\nmin: %A\nmax: %A\n}" this.Min this.Max

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Bounds =
    [<Literal>]
    let cornerCount = 8

    let inline corner i (bounds: Bounds) =
        vec3 (bounds.[i &&& 1].X, bounds.[(i >>> 1) &&& 1].Y, bounds.[(i >>> 2) &&& 1].Z)

    let inline corners (bounds: Bounds) =
            [
            vec3 (bounds.Min.X, bounds.Max.Y, bounds.Max.Z)
            vec3 (bounds.Max.X, bounds.Max.Y, bounds.Max.Z)
            vec3 (bounds.Max.X, bounds.Min.Y, bounds.Max.Z)
            vec3 (bounds.Min.X, bounds.Min.Y, bounds.Max.Z)
            vec3 (bounds.Min.X, bounds.Max.Y, bounds.Min.Z)
            vec3 (bounds.Max.X, bounds.Max.Y, bounds.Min.Z)
            vec3 (bounds.Max.X, bounds.Min.Y, bounds.Min.Z)
            vec3 (bounds.Min.X, bounds.Min.Y, bounds.Min.Z)]    
        
    let radius (bounds: Bounds) =
        let inline f i =
            let a = abs bounds.Min.[i]
            let b = abs bounds.Max.[i]
            if a > b then a else b
        
        Vec3.magnitude <| vec3 (f 0, f 1, f 2)

