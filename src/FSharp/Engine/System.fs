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

    [<DllImport(libQuake3)>]
    extern void Sys_CreateConsole ()

    [<DllImport(libQuake3)>]
    extern void Sys_Milliseconds ()

    [<DllImport(libQuake3)>]
    extern void Sys_InitStreamThread ()

    [<DllImport(libQuake3)>]
    extern void Com_Init (string commandLine)

    [<DllImport(libQuake3)>]
    extern void NET_Init ()

    [<DllImport(libQuake3)>]
    extern bool Com_IsDedicated ()

    [<DllImport(libQuake3)>]
    extern bool Com_IsViewLogEnabled ()

    [<DllImport(libQuake3)>]
    extern void IN_Frame ()

    [<DllImport(libQuake3)>]
    extern void Com_Frame ()

    [<DllImport(libQuake3)>]
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

    let Sleep (milliseconds: int) =
        Thread.Sleep (milliseconds)

    let Milliseconds () =
        _stopwatch.ElapsedMilliseconds

    let Init () =
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
