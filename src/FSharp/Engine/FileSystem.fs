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

module Engine.FileSystem

open System
open System.IO

/// Based on Q3: pack_t
/// Pak
/// Not pure
type Pak =
    {
        FileInfo: FileInfo;
        Checksum: int;
        PureChecksum: int;
        FileCount: int;
        // TODO:
    }

/// Based on Q3: searchpath_t
/// SearchPath
/// Not pure
type SearchPath =
    {
        DirectoryInfo: DirectoryInfo option;
    }

/// Based on Q3: fileHandleData_t, qfile_us, qfile_gus
/// FileHandleData
/// Not pure
type FileData =
    {
        File: File;
        IsUnique: bool;
    }

/// FileSystem
/// Not pure
type FileSystem = 
    {
        SearchPaths: SearchPath list;
        ServerPakChecksums: int list;
        // TODO:
    }

let create searchPaths serverPakChecksums =
    {
        SearchPaths = searchPaths;
        ServerPakChecksums = serverPakChecksums;
    }

/// Based on Q3: FS_Initialized
/// IsInitialized
let isInitialized (fs: FileSystem) =
    fs.SearchPaths.Length <> 0

/// Based on Q3: FS_PakIsPure
/// IsPakPure
let isPakPure (pak: Pak) (fs: FileSystem) =
    fs.ServerPakChecksums.Length = 0 ||
    List.exists (fun x -> x = pak.Checksum) fs.ServerPakChecksums