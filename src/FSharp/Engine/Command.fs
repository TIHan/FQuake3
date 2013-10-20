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

namespace Engine.Command

open System
open System.IO
open System.Text
open System.Runtime.InteropServices
open System.Threading
open System.Diagnostics
open Microsoft.FSharp.NativeInterop
open Engine.NativeInterop

module private Native =
    type XCommand = delegate of unit -> unit

    [<DllImport (LibQuake3, CallingConvention = DefaultCallingConvention)>]
    extern void Cmd_AddCommand (string cmdName, XCommand func)

    [<DllImport (LibQuake3, CallingConvention = DefaultCallingConvention)>]
    extern int Cmd_Argc ()

    [<DllImport (LibQuake3, CallingConvention = DefaultCallingConvention)>]
    extern void Cmd_ArgsBuffer (StringBuilder buffer, int length)

    [<DllImport (LibQuake3, CallingConvention = DefaultCallingConvention)>]
    extern void Cbuf_ExecuteText(int execWhen, string text)

type CommandExecutionType =
    /// don't return until completed, a VM should NEVER use this,
    /// because some commands might cause the VM to be unloaded...
    | Now = 0

    // insert at current position, but don't run yet
    | Insert = 1

    // add to end of the command buffer (normal case)
    | Append = 2

module Command =
    let Add (name: string) (f: unit -> unit) =
        let cmd = Native.XCommand (f)
        GCHandle.Alloc (cmd, GCHandleType.Pinned) |> ignore // FIXME: We are only doing this to prevent GC when passed to unmanaged.
        Native.Cmd_AddCommand (name, cmd)

    let Argc () =
        Native.Cmd_Argc ()

    let ArgsBuffer () =
        let sb = StringBuilder (1024)
        Native.Cmd_ArgsBuffer (sb, 1024)
        sb.ToString ()

    let ExecuteText (cmdExec: CommandExecutionType) (text: string) =
        Native.Cbuf_ExecuteText (int cmdExec, text)

