
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

let mutable spritesFsx : unit -> int =
    fun _ -> 0
#endif

let scaleHead (axis: Axis) (cg: CGame) =
#if INTERACTIVE
    axis
#else
    scaleHeadFsx axis cg
#endif

let sprites () =
#if INTERACTIVE
    0
#else
    spritesFsx ()
#endif

#if INTERACTIVE
CGame.Players.scaleHeadFsx
    <- scaleHead

CGame.Players.spritesFsx
    <- sprites
#endif

#if INTERACTIVE
#else
let floatSpriteOrigin (spriteOrigin: vec3) (entOrigin: vec3) =
    spriteOrigin.Set (z = spriteOrigin.z - 10.f)
#endif