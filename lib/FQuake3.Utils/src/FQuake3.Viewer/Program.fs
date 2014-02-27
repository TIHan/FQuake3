module FQuake3.Viewer.Main

#nowarn "9"
#nowarn "51"

open System.Threading;
open System.Security
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

[<SuppressUnmanagedCodeSecurity>]
[<DllImport ("FQ3Viewer.dll", CallingConvention = CallingConvention.Cdecl)>]
extern nativeint app_init (string title)

[<SuppressUnmanagedCodeSecurity>]
[<DllImport ("FQ3Viewer.dll", CallingConvention = CallingConvention.Cdecl)>]
extern int app_exit (nativeint app)

type AppLoopCallback = delegate of unit -> unit

[<SuppressUnmanagedCodeSecurity>]
[<DllImport ("FQ3Viewer.dll", CallingConvention = CallingConvention.Cdecl)>]
extern void app_loop (nativeint app, AppLoopCallback callback)

[<SuppressUnmanagedCodeSecurity>]
[<DllImport ("FQ3Viewer.dll", CallingConvention = CallingConvention.Cdecl)>]
extern void app_clear_window (nativeint app)

[<EntryPoint>]
let main args =
    let app = app_init ("FQuake3 Viewer")

    printfn "FQuake3 Viewer started."

    let callback = AppLoopCallback (fun () -> ())
    app_loop (app, callback)

    printfn "FQuake3 Viewer shutting down..."

    app_exit (app)

