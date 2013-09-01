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
    let inline toStructure<'T,'U when 'T : struct and 'U : unmanaged> (x: nativeptr<'U>) =
        System.Runtime.InteropServices.Marshal.PtrToStructure (NativePtr.toNativeInt x, typeof<'T>) :?> 'T

    let inline ofStructure<'T,'U when 'T : struct and 'U : unmanaged> (structure: 'T) (x: nativeptr<'U>) =
        System.Runtime.InteropServices.Marshal.StructureToPtr (structure, NativePtr.toNativeInt x, true)

    let inline toString (x: nativeptr<'T>) =
        System.Runtime.InteropServices.Marshal.PtrToStringAuto (NativePtr.toNativeInt x)

    let inline toArray (size: int) (x: nativeptr<'T>) =
        let arr = Array.zeroCreate<'T> size
        let arrPtr = &&arr.[0]
        for i = 0 to size - 1 do
            NativePtr.set arrPtr i <| NativePtr.get x i
        arr

    let inline toNativePtr x =
        NativePtr.toNativeInt x |> NativePtr.ofNativeInt

/// <summary>
/// List
/// </summary>
module List =
    let inline ofNativePtrArray<'T when 'T : unmanaged> size (x: nativeptr<'T>) =
        List.init size (fun i ->
            NativePtr.get x i
        )

    let inline ofNativePtrArrayMap<'T, 'U when 'T : unmanaged> size (f: 'T -> 'U) (x: nativeptr<'T>) =
        List.init size (fun i ->
            f <| NativePtr.get x i
        )

