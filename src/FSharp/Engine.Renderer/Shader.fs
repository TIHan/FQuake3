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

module Engine.Renderer.Shader

open System
open System.IO
open System.Text
open System.Security
open System.Diagnostics.Contracts
open System.Runtime.InteropServices
open Engine.NativeInterop
open Engine.Renderer.Core

module private Native =
    [<SuppressUnmanagedCodeSecurity>]
    [<DllImport (LibQuake3, CallingConvention = DefaultCallingConvention)>]
    extern void CreateInternalShaders ()

    [<SuppressUnmanagedCodeSecurity>]
    [<DllImport (LibQuake3, CallingConvention = DefaultCallingConvention)>]
    extern void ScanAndLoadShaderFiles ()

    [<SuppressUnmanagedCodeSecurity>]
    [<DllImport (LibQuake3, CallingConvention = DefaultCallingConvention)>]
    extern void CreateExternalShaders ()

    [<SuppressUnmanagedCodeSecurity>]
    [<DllImport (LibQuake3, CallingConvention = DefaultCallingConvention)>]
    extern int RE_RegisterShaderNoMip (string name)

/// RendererShaderState
type RendererShaderState = 
    {
        /// Based on Q3: hashTable in tr_shader.c
        ShaderHashMap: Map<int, Shader>
    }

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module RendererShaderState =
    let create () =
        {
            ShaderHashMap = Map.empty<int, Shader>
        }

let registerShaderNoMip name = Native.RE_RegisterShaderNoMip name

/// Based on Q3: generateHashValue
/// generateFileNameHashCode
///
/// Note: Path does not mean the actual physical path location.
[<Pure>]
let generateFileNameHashCode (filePath: string) =
    filePath.GetHashCode ()

/// Based on Q3: R_GetShaderByHandle
/// ShaderById
///
/// When a handle is passed in by another module, this range checks
/// it and returns a valid (possibly default) shader_t to be used internally.
let shaderById id (r: Renderer) =
    r.Shaders.[id]

/// FIXME: We should never have global variables.
let mutable globalRendererShaderState = Unchecked.defaultof<RendererShaderState>

let mutable shaderIdDon = 0
let mutable shaderIdFsharp = 0
let mutable shaderIdThomas = 0

/// Based on Q3: R_InitShaders
/// Init
let init () =
    printfn "Initializing Shaders"

    globalRendererShaderState <- RendererShaderState.create ()

    Native.CreateInternalShaders ()
    Native.ScanAndLoadShaderFiles ()
    Native.CreateExternalShaders ()

    shaderIdDon <- registerShaderNoMip "don.tga"
    shaderIdFsharp <- registerShaderNoMip "fsharp.tga"
    shaderIdThomas <- registerShaderNoMip "thomas.tga"
    ()
