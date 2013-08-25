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

module Engine.NativeInterop

open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

[<Literal>]
let LibQuake3 = "quake3.dll"

[<Literal>]
let LibEngine = "Engine.Native.dll"

[<Literal>]
let DefaultCallingConvention = CallingConvention.Cdecl

/// <summary>
/// NativePtr
/// </summary>
module NativePtr =
    let inline toStructure<'T,'U when 'T : struct and 'U : unmanaged> (native: nativeptr<'U>) =
        System.Runtime.InteropServices.Marshal.PtrToStructure (NativePtr.toNativeInt native, typeof<'T>) :?> 'T

    let inline ofStructure<'T,'U when 'T : struct and 'U : unmanaged> (structure: 'T) (native: nativeptr<'U>) =
        System.Runtime.InteropServices.Marshal.StructureToPtr (structure, NativePtr.toNativeInt native, true)

    let inline toString (native: nativeptr<'T>) =
        System.Runtime.InteropServices.Marshal.PtrToStringAuto (NativePtr.toNativeInt native)

/// <summary>
/// List
/// </summary>
module List =
    let inline ofNativePtrArray<'T when 'T : unmanaged> size (native: nativeptr<'T>) =
        List.init size (fun i ->
            NativePtr.get native i
        )

    let inline ofNativePtrArrayMap<'T, 'U when 'T : unmanaged> size (f: 'T -> 'U) (native: nativeptr<'T>) =
        List.init size (fun i ->
            f <| NativePtr.get native i
        )

