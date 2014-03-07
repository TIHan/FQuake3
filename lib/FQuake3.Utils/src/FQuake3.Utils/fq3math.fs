module FQuake3.Math

#nowarn "9"

open System
open System.Runtime.InteropServices
open FSharp.Game.Math

/// Axis
[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type Axis =
    val x : vec3
    val y : vec3
    val z : vec3

    new (x, y, z) = { x = x; y = y; z = z }

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.x | 1 -> this.y | 2 -> this.z
            | _ -> raise <| IndexOutOfRangeException ()

    member inline this.Set (?x: vec3, ?y: vec3, ?z: vec3) =
        Axis (
            (match x with | Some x -> x | None -> this.x),
            (match y with | Some y -> y | None -> this.y),
            (match z with | Some z -> z | None -> this.z))

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
    val min : vec3
    val max : vec3 

    new (min, max) = { min = min; max = max }

    member inline this.Item
        with get (i) =
            match i with
            | 0 -> this.min | 1 -> this.max
            | _ -> raise <| IndexOutOfRangeException ()

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Bounds =
    [<Literal>]
    let cornerCount = 8

    let inline corner i (bounds: Bounds) =
        vec3 (bounds.[i &&& 1].x, bounds.[(i >>> 1) &&& 1].y, bounds.[(i >>> 2) &&& 1].z)

    let inline corners (bounds: Bounds) =
            [
            vec3 (bounds.min.x, bounds.max.y, bounds.max.z)
            vec3 (bounds.max.x, bounds.max.y, bounds.max.z)
            vec3 (bounds.max.x, bounds.min.y, bounds.max.z)
            vec3 (bounds.min.x, bounds.min.y, bounds.max.z)
            vec3 (bounds.min.x, bounds.max.y, bounds.min.z)
            vec3 (bounds.max.x, bounds.max.y, bounds.min.z)
            vec3 (bounds.max.x, bounds.min.y, bounds.min.z)
            vec3 (bounds.min.x, bounds.min.y, bounds.min.z)]    
        
    let radius (bounds: Bounds) =
        let inline f i =
            let a = abs bounds.min.[i]
            let b = abs bounds.max.[i]
            if a > b then a else b
        
        Vec3.length <| vec3 (f 0, f 1, f 2)

