
#if INTERACTIVE
#r "FQuake3.Utils.dll"
#r "Engine.dll"
#r "Engine.Renderer.dll"
#r "CGame.dll"
#else
module CGame.Players
#endif

open FSharp.Game.Math
open Engine.Renderer
open CGame.Core
open FQuake3.Math

#if INTERACTIVE
#else
let mutable headTransformFsx
    : vec3 -> Axis -> CGame -> (int * vec3 * Axis) =
        fun origin axis _ -> (0, origin, axis)
#endif

let headTransform (origin: vec3) (axis: Axis) (cg: CGame) =
#if INTERACTIVE
    (0, origin, axis)
#else
    headTransformFsx origin axis cg
#endif

#if INTERACTIVE
CGame.Players.headTransformFsx
    <- headTransform
#endif