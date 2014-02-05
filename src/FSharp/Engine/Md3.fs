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

namespace Engine.Files

open Engine.Core
open Engine.Math

/// Based on Q3: md3Header_t
/// Md3Header
type Md3Header = {
    Id: int;
    Version: int;

    /// model name
    Name: string;
    Flags: int;
    FrameCount: int;
    TagCount: int;
    SurfaceCount: int;
    SkinCount: int;

    /// first surface
    FrameOffset: int;

    /// numFrames * numTags
    TagOffset: int;

    /// first surface, others follow
    SurfaceOffset: int;

    /// end of file
    EndOffset: int }

/// Based on Q3: md3Frame_t
/// Md3Frame
type Md3Frame = {
    Bounds: Bounds;
    LocalOrigin: vec3;
    Radius: single;
    Name: string }

/// Based on Q3: md3Tag_t
/// Md3Tag
type Md3Tag = {
    Name: string
    Origin: vec3
    Axis: Axis }

/// Based on Q3: md3Shader_t
/// Md3Shader
type Md3Shader = {
    Name: string
    ShaderId: int }

/// Based on Q3: md3Surface_t
/// Md3Surface
/// TODO: Not finished.
type Md3Surface = {
    Id: int
    Name: string
    Flags: int }

/// Md3
type Md3 = {
    Header: Md3Header;
    Frames: Md3Frame list
    Surfaces: Md3Surface list }

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Md3 =
    /// (('3'<<24)+('P'<<16)+('D'<<8)+'I')
    /// Is this right?
    [<Literal>]
    let Ident = 16uy

    [<Literal>]
    let Version = 15

    /// 1.0 / 64.0
    [<Literal>]
    let XyzScale = 0.015625f

    module Limits = 
        [<Literal>]
        let MaxLods = 3

        /// per surface
        [<Literal>]
        let MaxTriangles = 8192

        /// per surface
        [<Literal>]
        let MaxVertices = 4096

        /// per surface
        [<Literal>]
        let MaxShaders = 256

        /// per model
        [<Literal>]
        let MaxFrames = 1024

        /// per model
        [<Literal>]
        let MaxSurfaces = 32

        /// per frame
        [<Literal>]
        let MaxTags = 16

