namespace idTech3

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading
open System.Diagnostics
open Microsoft.FSharp.NativeInterop

module private Native =
    [<DllImport("quake3.dll")>]
    extern void Sys_CreateConsole ()

    [<DllImport("quake3.dll")>]
    extern void Sys_Milliseconds ()

    [<DllImport("quake3.dll")>]
    extern void Sys_InitStreamThread ()

    [<DllImport("quake3.dll")>]
    extern void Com_Init (char *commandLine)

    [<DllImport("quake3.dll")>]
    extern void NET_Init ()

    [<DllImport("quake3.dll")>]
    extern bool Com_IsDedicated ()

    [<DllImport("quake3.dll")>]
    extern bool Com_IsViewLogEnabled ()

    [<DllImport("quake3.dll")>]
    extern void IN_Frame ()

    [<DllImport("quake3.dll")>]
    extern void Com_Frame ()

    [<DllImport("quake3.dll")>]
    extern void Sys_ShowConsole (int level, bool quitOnClose)


module idInput =
    let Frame () =
        Native.IN_Frame ()


module idCommon =
    let Frame () =
        Native.Com_Frame ()

    let IsDedicated () =
        Native.Com_IsDedicated ()


module idNetwork =
    let Init () =
        Native.NET_Init ()


module idFileSystem =
    let GetCurrentDirectory () =
        Directory.GetCurrentDirectory ()


module idSystem =
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

        let mutable commandLine = 't'
        Native.Com_Init (&&commandLine)
        idNetwork.Init ()

        // hide the early console since we've reached the point where we
        // have a working graphics subsystems
        match (Native.Com_IsDedicated (), Native.Com_IsViewLogEnabled ()) with
        | (false, false) -> Native.Sys_ShowConsole (0, false)
        | _ -> ()

        printfn "Working directory: %s\n" (idFileSystem.GetCurrentDirectory ())

        // main game loop
        while true do
            // if not running as a game client, sleep a bit
            match idCommon.IsDedicated () with
            | true -> Sleep (5)
            | _ -> ()

            // make sure mouse and joystick are only called once a frame
            idInput.Frame ()

            // run the game
            idCommon.Frame ();
        ()
