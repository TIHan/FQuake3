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

/// Based on Q3: md4Header_t
/// Md4Header
type Md4Header =
    {
        Id: int;
        Version: int;

        /// model name
        Name: string;

        // frames and bones are shared by all levels of detail
        Framecount: int;
        BoneCount: int;

        /// char name[ MAX_QPATH ]
        BoneNameOffset: int;

        /// md4Frame_t[numFrames]
        FrameOffset: int;

        // each level of detail has completely separate sets of surfaces
        LodCount: int;
        LodOffset: int;

        /// end of file
        EndOffset: int;
    }

