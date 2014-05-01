(*
Copyright (C) 2013-2014 William F. Smith

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

namespace Engine.Renderer.Native

[<Struct>]
type Plane =
    val X : double
    val Y : double
    val Z : double
    val W : double

    new (x, y, z, w) = { X = x; Y = y; Z = z; W = w }

module GLS =
    type SrcBlend =
        | None =                0x00000000
        | Zero =                0x00000001
        | One =                 0x00000002
        | DstColor =            0x00000003
        | OneMinusDstColor =    0x00000004
        | SrcAlpha =            0x00000005
        | OneMinusSrcAlpha =    0x00000006
        | DstAlpha =            0x00000007
        | OneMinusDstAlpha =    0x00000008
        | AlphaSaturate =       0x00000009
        | Bits =                0x0000000f

    type DstBlend =
        | None =                0x00000000
        | Zero =                0x00000010
        | One =                 0x00000020
        | SrcColor =            0x00000030
        | OneMinusSrcColor =    0x00000040
        | SrcAlpha =            0x00000050
        | OneMinusSrcAlpha =    0x00000060
        | DstAlpha =            0x00000070
        | OneMinusDstAlpha =    0x00000080
        | Bits =                0x000000f0

    [<Literal>]
    let DepthMaskTrue =         0x00000100

    [<Literal>]
    let PolyModeLine =          0x00001000

    [<Literal>]
    let DepthTestDisable =      0x00010000

    [<Literal>]
    let DepthFuncEqual =        0x00020000

    type ATest =
        | None =                0x00000000
        | Gt0 =                 0x10000000
        | Lt80 =                0x20000000
        | Ge80 =                0x40000000
        | Bits =                0x70000000

    [<Literal>]
    let Default = DepthMaskTrue