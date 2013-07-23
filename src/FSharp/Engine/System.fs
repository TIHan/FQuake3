(*
Copyright © 2013 OpenFK

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
Copyright © 1999-2005 Id Software, Inc.
*)

namespace Engine

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading
open System.Diagnostics
open Microsoft.FSharp.NativeInterop

module private Native =

    [<Literal>]
    let libQuake3 = "quake3.dll"

    [<Literal>]
    let callingConvention = CallingConvention.Cdecl

    [<DllImport(libQuake3, CallingConvention = callingConvention)>]
    extern void Sys_CreateConsole ()

    [<DllImport(libQuake3, CallingConvention = callingConvention)>]
    extern void Sys_Milliseconds ()

    [<DllImport(libQuake3, CallingConvention = callingConvention)>]
    extern void Sys_InitStreamThread ()

    [<DllImport(libQuake3, CallingConvention = callingConvention)>]
    extern void Com_Init (string commandLine)

    [<DllImport(libQuake3, CallingConvention = callingConvention)>]
    extern void NET_Init ()

    [<DllImport(libQuake3, CallingConvention = callingConvention)>]
    extern bool Com_IsDedicated ()

    [<DllImport(libQuake3, CallingConvention = callingConvention)>]
    extern bool Com_IsViewLogEnabled ()

    [<DllImport(libQuake3, CallingConvention = callingConvention)>]
    extern void IN_Frame ()

    [<DllImport(libQuake3, CallingConvention = callingConvention)>]
    extern void Com_Frame ()

    [<DllImport(libQuake3, CallingConvention = callingConvention)>]
    extern void Sys_ShowConsole (int level, bool quitOnClose)

module Input =
    let Frame () =
        Native.IN_Frame ()


module Common =
    let Frame () =
        Native.Com_Frame ()

    let IsDedicated () =
        Native.Com_IsDedicated ()


module Network =
    let Init () =
        Native.NET_Init ()


module FileSystem =
    let GetCurrentDirectory () =
        Directory.GetCurrentDirectory ()


module System =
    let private _stopwatch = new Stopwatch ()

    let private SetupUnhandledExceptions () =
        let errorFilename = "error.txt"
        let UnhandledException (sender: obj) (e: UnhandledExceptionEventArgs) =
            let exceptionObject = (e.ExceptionObject :?> Exception)
            let msg = sprintf "%s %s\n" (exceptionObject.ToString ()) (exceptionObject.Message)

            let innerMsg = 
                match exceptionObject.InnerException = null with
                | true -> ""
                | _ -> exceptionObject.InnerException.Message
            
            let fullMsg = sprintf "%s %s" msg innerMsg

            File.WriteAllText (errorFilename, fullMsg)

        File.Delete (errorFilename)
        AppDomain.CurrentDomain.UnhandledException.AddHandler (new UnhandledExceptionEventHandler (UnhandledException))

    let Sleep (milliseconds: int) =
        Thread.Sleep (milliseconds)

    let Milliseconds () =
        _stopwatch.ElapsedMilliseconds

    let Init () =
        SetupUnhandledExceptions ()

        // done before Com/Sys_Init since we need this for error output
        Native.Sys_CreateConsole ()

        // get the initial time base
        _stopwatch.Start ()

        Native.Sys_InitStreamThread ()

        Native.Com_Init ("")
        Network.Init ()

        // hide the early console since we've reached the point where we
        // have a working graphics subsystems
        match (Native.Com_IsDedicated (), Native.Com_IsViewLogEnabled ()) with
        | (false, false) -> Native.Sys_ShowConsole (0, false)
        | _ -> ()

        printfn "Working directory: %s\n" (FileSystem.GetCurrentDirectory ())

        // main game loop
        while true do
            // if not running as a game client, sleep a bit
            match Common.IsDedicated () with
            | true -> Sleep (5)
            | _ -> ()

            // make sure mouse and joystick are only called once a frame
            Input.Frame ()

            // run the game
            Common.Frame ();
        ()
