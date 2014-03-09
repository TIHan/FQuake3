(*
Copyright (C) 2013 William F. Smith

This program is free software; you can redistribute it
and/or modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the License,
or (at your option) any later version.

This program is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

Derivative of Quake III Arena source:
Copyright (C) 1999-2005 Id Software, Inc.
*)

module CGame.Weapons

open System
open System.IO
open System.Reflection
open System.Diagnostics.Contracts
open FSharp.Game.Math
open Engine.Renderer
open CGame.Core

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

let mutable 
    calculateWeaponPositionFsx 
    : CGame -> (vec3 * vec3) = 
        fun _ -> (Vec3.zero, Vec3.zero)

let mutable prevTime = Unchecked.defaultof<DateTime>

[<Pure>]
let calculateWeaponPosition (cg: CGame) =
    let time = File.GetLastWriteTime ("weapons.fsx")

    if prevTime <> time then
        prevTime <- time
        printfn "Evaluating weapons.fsx"
        fsiSession.EvalScript "weapons.fsx"
        
    calculateWeaponPositionFsx cg
