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

// Disable native interop warnings
#nowarn "9"
#nowarn "51"

namespace Engine.Native

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading
open System.Diagnostics
open Microsoft.FSharp.NativeInterop
open FSharpx.Collections
open Engine.Core
open Engine.Net
open Engine.Math
open Engine.NativeInterop

module bool =
    let inline ofNative (native: qboolean) =
        match native with
        | qboolean.qtrue -> true
        | _ -> false

    let inline toNativeByPtr (ptr: nativeptr<qboolean>) (value: bool) =
        let mutable native = NativePtr.read ptr

        native <- if value then qboolean.qtrue else qboolean.qfalse

        NativePtr.write ptr native

    let inline toNative (value: bool) =
        if value then qboolean.qtrue else qboolean.qfalse

module Vector2 =
    let inline ofNative (native: vec2_t) =
        { Vector2.X = native.value; Y = native.value1 }

    let inline toNativeByPtr (ptr: nativeptr<vec2_t>) (v: Vector2) =
        let mutable native = NativePtr.read ptr

        native.value <- v.X
        native.value1 <- v.Y

        NativePtr.write ptr native  

module Vector3 =
    let inline ofNative (native: vec3_t) =
        { Vector3.X = native.value; Y = native.value1; Z = native.value2 }

    let inline toNativeByPtr (ptr: nativeptr<vec3_t>) (v: Vector3) =
        let mutable native = NativePtr.read ptr

        native.value <- v.X
        native.value1 <- v.Y
        native.value2 <- v.Z

        NativePtr.write ptr native   
        
module Vector4 =
    let inline ofNative (native: vec4_t) =
        { Vector4.X = native.value; Y = native.value1; Z = native.value2; W = native.value3 }

    let inline toNativeByPtr (ptr: nativeptr<vec4_t>) (v: Vector4) =
        let mutable native = NativePtr.read ptr

        native.value <- v.X
        native.value1 <- v.Y
        native.value2 <- v.Z
        native.value3 <- v.W

        NativePtr.write ptr native  

module Matrix4x4 =
    let inline ofNative (ptr: nativeptr<single>) =
        Matrix4x4.create
            (NativePtr.get ptr 0)
            (NativePtr.get ptr 1)
            (NativePtr.get ptr 2)
            (NativePtr.get ptr 3)
            (NativePtr.get ptr 4)
            (NativePtr.get ptr 5)
            (NativePtr.get ptr 6)
            (NativePtr.get ptr 7)
            (NativePtr.get ptr 8)
            (NativePtr.get ptr 9)
            (NativePtr.get ptr 10)
            (NativePtr.get ptr 11)
            (NativePtr.get ptr 12)
            (NativePtr.get ptr 13)
            (NativePtr.get ptr 14)
            (NativePtr.get ptr 15)

    let inline toNativeByPtr (ptr: nativeptr<single>) (m: Matrix4x4) =
        NativePtr.set ptr 0 m.[0, 0]
        NativePtr.set ptr 1 m.[0, 1]
        NativePtr.set ptr 2 m.[0, 2]
        NativePtr.set ptr 3 m.[0, 3]
        NativePtr.set ptr 4 m.[1, 0]
        NativePtr.set ptr 5 m.[1, 1]
        NativePtr.set ptr 6 m.[1, 2]
        NativePtr.set ptr 7 m.[1, 3]
        NativePtr.set ptr 8 m.[2, 0]
        NativePtr.set ptr 9 m.[2, 1]
        NativePtr.set ptr 10 m.[2, 2]
        NativePtr.set ptr 11 m.[2, 3]
        NativePtr.set ptr 12 m.[3, 0]
        NativePtr.set ptr 13 m.[3, 1]
        NativePtr.set ptr 14 m.[3, 2]
        NativePtr.set ptr 15 m.[3, 3]

module Cvar =
    let inline ofNative (native: cvar_t) =
        {
            Name = NativePtr.toString native.name;
            String = NativePtr.toString native.string;
            ResetString = NativePtr.toString native.resetString;
            LatchedString = NativePtr.toString native.latchedString;
            Flags = native.flags;
            IsModified = Convert.ToBoolean native.modified;
            ModificationCount = native.modificationCount;
            Value = native.value;
            Integer = native.integer;
        }

module ByteString =
    let inline ofNativePtr (size: int) (nativePtr: nativeptr<byte>) =
        ByteString.create <| NativePtr.toArray size nativePtr

module Message =
    let inline ofNative (native: msg_t) =
        {
            IsAllowedOverflow = Convert.ToBoolean native.allowoverflow;
            IsOverflowed = Convert.ToBoolean native.overflowed;
            IsOutOfBand = Convert.ToBoolean native.oob;
            Data = ByteString.ofNativePtr native.cursize native.data;
            MaxSize = native.maxsize;
            ReadCount = native.readcount;
            Bit = native.bit;
        }

module IPAddress =
    let inline ofNative (native: netadr_t) =
        {
            Type = enum<AddressType> (int native.type');
            IP = NativePtr.toStructure &&native.ip;
            Port = native.port;
        }


