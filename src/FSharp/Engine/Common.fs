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

namespace Engine.Common

open System.Runtime.InteropServices
open Engine.NativeInterop

module private Native =
    [<DllImport(LibQuake3, CallingConvention = DefaultCallingConvention)>]
    extern void Com_Init (string commandLine)

    [<DllImport(LibQuake3, CallingConvention = DefaultCallingConvention)>]
    extern bool Com_IsDedicated ()

    [<DllImport(LibQuake3, CallingConvention = DefaultCallingConvention)>]
    extern bool Com_IsViewLogEnabled ()

    [<DllImport(LibQuake3, CallingConvention = DefaultCallingConvention)>]
    extern void Com_Frame ()

    [<DllImport(LibQuake3, CallingConvention = DefaultCallingConvention)>]
    extern void Com_Printf (string fmt);

/// <summary>
/// Common
///
/// Note: Revisit to make purely functional.
/// </summary
module Common =
    let Init commandLine =
        Native.Com_Init commandLine

    let CheckIsDedicated () =
        Native.Com_IsDedicated ()

    let CheckIsViewLogEnabled () =
        Native.Com_IsViewLogEnabled ()

    let Frame () =
        Native.Com_Frame ()

    let Printf fmt =
        Native.Com_Printf fmt

