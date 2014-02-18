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

[<EntryPoint>]
let main args =
    let app = app_init ("FQuake3 Viewer")
    Thread.Sleep (10000)
    app_exit (app)

