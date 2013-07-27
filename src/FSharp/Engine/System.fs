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
open OpenFK

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

    [<DllImport(libQuake3, CallingConvention = callingConvention)>]
    extern void Com_Printf (string fmt);

    type XCommand = delegate of unit -> unit

    [<DllImport(libQuake3, CallingConvention = callingConvention)>]
    extern void Cmd_AddCommand( string cmdName, XCommand func)

// WIP
type StandardIO () =
    let ms = new MemoryStream ()
    let sw = new StreamWriter (ms)
    let sr = new StreamReader (ms)

    [<DefaultValue>] val mutable private redirectOut : string -> unit

    member this.RedirectOut (f: string -> unit) =
        Console.SetOut sw
        this.redirectOut <- f

    member this.FlushOut () =
            sw.Flush ()
            match sr.BaseStream.Length <> 0L with
            | true ->
                sr.BaseStream.Position <- 0L
                this.redirectOut <| sr.ReadToEnd ()
                sr.BaseStream.SetLength 0L
            | _ -> ()

    interface IDisposable with
        member this.Dispose () =
            sr.Dispose ()
            sw.Dispose ()
            ms.Dispose ()

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


module Command =
    let Add (name: string) (f: unit -> unit) =
        let cmd = Native.XCommand (f)
        GCHandle.Alloc (cmd, GCHandleType.Pinned) |> ignore
        Native.Cmd_AddCommand (name, cmd)


module System =
    let private fsiProcess = FsiSession @"C:\Program Files (x86)\Microsoft SDKs\F#\3.0\Framework\v4.0\Fsi.exe"
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

    let Init () =
        SetupUnhandledExceptions ()

        fsiProcess.OutputReceived.Add (fun e ->
            printfn "%s" <| e.Data.ToString ()
        )
        use io = new StandardIO ()

        io.RedirectOut Native.Com_Printf

        // done before Com/Sys_Init since we need this for error output
        Native.Sys_CreateConsole ()

        // get the initial time base
        stopwatch.Start ()

        Native.Sys_InitStreamThread ()

        Native.Com_Init ("")
        Network.Init ()

        Command.Add "fsi" (fun _ -> fsiProcess.Start ())

        // hide the early console since we've reached the point where we
        // have a working graphics subsystems
        match (Native.Com_IsDedicated (), Native.Com_IsViewLogEnabled ()) with
        | (false, false) -> Native.Sys_ShowConsole (0, false)
        | _ -> ()

        printfn "Working directory: %s" (FileSystem.GetCurrentDirectory ())

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
            
            // Flush standard out
            io.FlushOut ()
        ()
