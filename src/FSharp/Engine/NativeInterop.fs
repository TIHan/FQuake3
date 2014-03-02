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

#nowarn "9"
#nowarn "51"

[<Literal>]
let LibQuake3 = "quake3.dll"

[<Literal>]
let DefaultCallingConvention = CallingConvention.Cdecl

// Hmm, I wonder if this is ok...
let inline fixed' (f: nativeptr<_> -> 'a) (a: obj) =
    let handle = GCHandle.Alloc (a, GCHandleType.Pinned)
    let addr = handle.AddrOfPinnedObject ()

    f <| NativePtr.ofNativeInt addr

    handle.Free ()

let inline fixed2' (f: nativeptr<_> -> nativeptr<_> -> 'a) (a: obj) (b: obj) =
    let handle = GCHandle.Alloc (a, GCHandleType.Pinned)
    let handle2 = GCHandle.Alloc (b, GCHandleType.Pinned)
    let addr = handle.AddrOfPinnedObject ()
    let addr2 = handle2.AddrOfPinnedObject ()

    f (NativePtr.ofNativeInt addr) <| NativePtr.ofNativeInt addr2

    handle.Free ()
    handle2.Free ()

/// NativePtr
module NativePtr =
    let inline toNativePtr<'a, 'b when 'a : unmanaged and 'b : unmanaged> (x: nativeptr<'a>) : nativeptr<'b> =
        NativePtr.toNativeInt x |> NativePtr.ofNativeInt<'b>

    let inline toStructure<'T,'U when 'T : struct and 'U : unmanaged> (x: nativeptr<'U>) =
        System.Runtime.InteropServices.Marshal.PtrToStructure (NativePtr.toNativeInt x, typeof<'T>) :?> 'T

    let inline toStringAnsi (x: nativeptr<_>) =
        System.Runtime.InteropServices.Marshal.PtrToStringAnsi (NativePtr.toNativeInt x)

    let inline toArray (size: int) (x: nativeptr<'T>) =
        Array.init size (fun i -> NativePtr.get x i)

    let inline toList (size: int) (x: nativeptr<'T>) =
        List.init size (fun i -> NativePtr.get x i)

    let inline isValid x =
        NativePtr.toNativeInt x <> System.IntPtr.Zero

    let inline zeroCreate () = NativePtr.ofNativeInt System.IntPtr.Zero

module String =
    let inline toStructure<'T when 'T : struct> (x: string) : 'T =
        let mutable struct' = Unchecked.defaultof<'T>
        match System.String.IsNullOrWhiteSpace (x) with
        | true -> struct'
        | _ ->

        let bytes = System.Text.Encoding.Default.GetBytes (x)

        let handle = GCHandle.Alloc (struct', GCHandleType.Pinned)
        let addr = handle.AddrOfPinnedObject ()

        System.Runtime.InteropServices.Marshal.Copy (bytes, 0, addr, x.Length)

        handle.Free ()

        struct'

        
/// List
module List =
    let inline ofNativePtrArray<'T when 'T : unmanaged> size (x: nativeptr<'T>) =
        List.init size (fun i -> NativePtr.get x i)

    let inline ofNativePtrArrayMap<'T, 'U when 'T : unmanaged> size (f: nativeptr<'T> -> 'U) (x: nativeptr<'T>) =
        List.init size (fun i -> f <| NativePtr.add x i)

    let inline toNativePtrArrayByPtr<'T, 'U when 'T : unmanaged> (ptr: nativeptr<'T>) (f: nativeptr<'T> -> 'U -> unit) (value: 'U list) =
        List.iteri (fun i x -> f (NativePtr.add ptr i) x) value

/// Seq
module Seq =
    let inline ofNativePtrArray<'T when 'T : unmanaged> size (x: nativeptr<'T>) =
        Seq.init size (fun i -> NativePtr.get x i)

/// Option
module Option =
    let inline ofNativePtr (f: nativeptr<'a> -> 'b) (ptr: nativeptr<'a>) =
        match NativePtr.isValid ptr with
        | false -> None
        | _ -> Some <| f ptr

