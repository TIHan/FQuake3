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

namespace Engine.System

open System
open System.IO
open System.Text
open System.Runtime.InteropServices
open System.Threading
open System.Diagnostics
open System.Reflection
open Microsoft.FSharp.NativeInterop
open Engine.Input
open Engine.Common
open Engine.Net
open Engine.IO
open Engine.Command
open Engine.NativeInterop

module private Native =
    [<DllImport (LibQuake3, CallingConvention = DefaultCallingConvention)>]
    extern void Sys_CreateConsole ()

    [<DllImport (LibQuake3, CallingConvention = DefaultCallingConvention)>]
    extern void Sys_Milliseconds ()

    [<DllImport (LibQuake3, CallingConvention = DefaultCallingConvention)>]
    extern void Sys_InitStreamThread ()

    [<DllImport (LibQuake3, CallingConvention = DefaultCallingConvention)>]
    extern void Sys_ShowConsole (int level, bool quitOnClose)

/// <summary>
/// System
///
/// Note: Revisit to make purely functional.
/// </summary
module System =
    let private stopwatch = new Stopwatch ()

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
        stopwatch.ElapsedMilliseconds

    let CreateConsole () = 
        Native.Sys_CreateConsole ()

    let ShowConsole level canQuitOnClose =
        Native.Sys_ShowConsole (level, canQuitOnClose)

    let StartStreamThread () =
        Native.Sys_InitStreamThread ()

    let Start () =
        SetupUnhandledExceptions ()

        use io = new StandardIO ()

        io.RedirectOut Common.Printf

        // done before Com/Sys_Init since we need this for error output
        CreateConsole ()

        // get the initial time base
        stopwatch.Start ()

        StartStreamThread ()

        Common.Init ""
        Net.Init ()

        Command.Add ("erl_ping") (fun () -> 
            ErlNet.send Ping
            printfn "%s" <| ErlNet.receive ()
        )

        // hide the early console since we've reached the point where we
        // have a working graphics subsystems
        match (Common.CheckIsDedicated (), Common.CheckIsViewLogEnabled ()) with
        | (false, false) -> ShowConsole 0 false
        | _ -> ()

        printfn "Working directory: %s" (IO.GetCurrentDirectory ())

        // main game loop
        while true do
            // if not running as a game client, sleep a bit
            match Common.CheckIsDedicated () with
            | true -> Sleep (5)
            | _ -> ()

            // make sure mouse and joystick are only called once a frame
            Input.Frame ()

            // run the game
            Common.Frame ();

            // Flush standard out
            io.FlushOut ()
        ()
