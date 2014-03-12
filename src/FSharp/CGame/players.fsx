
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
let mutable scaleHeadFsx
    : Axis -> CGame -> Axis =
        fun axis _ -> axis
#endif

let scaleHead (axis: Axis) (cg: CGame) =
#if INTERACTIVE
    axis
#else
    scaleHeadFsx axis cg
#endif

#if INTERACTIVE
CGame.Players.scaleHeadFsx
    <- scaleHead
#endif