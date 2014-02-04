﻿(*
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

open System.Runtime.InteropServices

module Constants =
    [<Literal>]
    let MaxQpath = 64

[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = Constants.MaxQpath)>]
type MAX_QPATH =
    [<FieldOffset (0)>]
    val mutable value : sbyte

type qboolean =
    | qfalse = 0
    | qtrue = 1

type qhandle_t = int

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type vec2_t =
    val mutable value : single
    val mutable value1 : single

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type vec3_t =
    val mutable value : single
    val mutable value1 : single
    val mutable value2 : single

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type vec4_t =
    val mutable value : single
    val mutable value1 : single
    val mutable value2 : single
    val mutable value3 : single

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type cvar_t =
    val mutable name : nativeptr<sbyte>
    val mutable string : nativeptr<sbyte>
    val mutable resetString : nativeptr<sbyte>
    val mutable latchedString : nativeptr<sbyte>
    val mutable flags : int
    val mutable modified : qboolean
    val mutable modificationCount : int
    val mutable value : single
    val mutable integer : int
    val mutable next : nativeptr<cvar_t>
    val mutable hashNext : nativeptr<cvar_t>

type netadrtype_t =
    | NA_BOT = 0
    | NA_BAD = 1
    | NA_LOOPBACK = 2
    | NA_BROADCAST = 3
    | NA_IP = 4
    | NA_IPX = 5
    | NA_BROADCAST_IPX = 6

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type netadr_t =
    val mutable type' : netadrtype_t
    val mutable ip : byte
    val private ip1 : byte
    val private ip2 : byte
    val private ip3 : byte
    val mutable ipx : byte
    val private ipx1 : byte
    val private ipx2 : byte
    val private ipx3 : byte
    val private ipx4 : byte
    val private ipx5 : byte
    val private ipx6 : byte
    val private ipx7 : byte
    val private ipx8 : byte
    val private ipx9 : byte
    val mutable port : uint16

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type msg_t =
    val mutable allowoverflow : qboolean
    val mutable overflowed : qboolean
    val mutable oob : qboolean
    val mutable data : nativeptr<byte>
    val mutable maxsize : int
    val mutable cursize : int
    val mutable readcount : int
    val mutable bit : int

[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = 16)>]
type md3Frame_t_name =
    [<FieldOffset (0)>]
    val mutable value : sbyte

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type md3Frame_t =
    val mutable bounds : vec3_t
    val private bounds1 : vec3_t
    val mutable localOrigin : vec3_t
    val mutable radius : single
    val mutable name : md3Frame_t_name

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type md3Tag_t =
    val mutable name : MAX_QPATH
    val mutable origin : vec3_t
    val mutable axis : vec3_t
    val private axis1 : vec3_t
    val private axis2 : vec3_t

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type md3Surface_t =
    val mutable ident : int
    val mutable name : MAX_QPATH
    val mutable flags : int
    val mutable numFrames : int
    val mutable numShaders : int
    val mutable numVerts : int
    val mutable numTriangles : int
    val mutable ofsTriangles : int
    val mutable ofsShaders : int
    val mutable ofsSt : int
    val mutable ofsXyzNormals : int
    val mutable ofsend : int

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type md3Shader_t =
    val mutable name : MAX_QPATH
    val mutable shaderIndex : int

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type md3Triangle_t =
    val mutable indexes : int
    val private indexes1 : int
    val private indexes2 : int

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type md3St_t =
    val mutable st : single
    val private st1 : single

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type md3XyzNormal_t =
    val mutable xyz : int16
    val private xyz1 : int16
    val private xyz2 : int16
    val mutable normal : int16

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type md3Header_t =
    val mutable ident : int
    val mutable version : int
    val mutable name : MAX_QPATH
    val mutable flags : int
    val mutable numFrames : int
    val mutable numTags : int
    val mutable numSurfaces : int
    val mutable numSkins : int
    val mutable ofsFrames : int
    val mutable ofsTags : int
    val mutable ofsSurfaces : int
    val mutable ofsEnd : int
#if FQ3_OLD
#else
    val mutable lod : int
#endif

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type md4Header_t =
    val mutable ident : int
    val mutable version : int
    val mutable name : MAX_QPATH
    val mutable numFrames : int
    val mutable numBones : int
    val mutable ofsBoneNames : int
    val mutable ofsFrames : int
    val mutable numLODs : int
    val mutable ofsLODs : int
    val mutable ofsEnd : int

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type fileInPack_t =
    val mutable name : nativeptr<sbyte>
    val mutable pos : uint32
    val mutable next : nativeptr<fileInPack_t>

[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = 256)>]
type MAX_OSPATH =
    [<FieldOffset (0)>]
    val mutable value : sbyte

[<Struct>]
[<StructLayout (LayoutKind.Explicit, Size = 256)>]
type MAX_ZPATH =
    [<FieldOffset (0)>]
    val mutable value : sbyte

type unzFile = nativeint

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type pack_t =
    val mutable pakFilename : MAX_OSPATH
    val mutable pakBasename : MAX_OSPATH
    val mutable pakGamename : MAX_OSPATH
    val mutable handle : unzFile
    val mutable checksum : int
    val mutable pure_checksum : int
    val mutable numfiles : int
    val mutable referenced : int
    val mutable hashSize : int
    val mutable hashTable : nativeptr<nativeptr<fileInPack_t>>
    val mutable buildBuffer : nativeptr<fileInPack_t>

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type directory_t =
    val mutable path : MAX_OSPATH
    val mutable gamedir : MAX_OSPATH

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type searchpath_t =
    val mutable next : nativeptr<searchpath_t>
    val mutable pack : nativeptr<pack_t>
    val mutable directory : nativeptr<directory_t>

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type qfile_gut =
    val mutable o : nativeint
    val mutable z : unzFile

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type qfile_ut =
    val mutable file : qfile_gut
    val mutable unique : qboolean

[<Struct>]
[<StructLayout (LayoutKind.Sequential)>]
type fileHandleData_t =
    val mutable handleFiles : qfile_ut
    val mutable handleSync : qboolean
    val mutable baseOffset : int
    val mutable fileSize : int
    val mutable zipFilePos : int
    val mutable zipFile : qboolean
    val mutable streamed : qboolean
    val mutable name : MAX_ZPATH