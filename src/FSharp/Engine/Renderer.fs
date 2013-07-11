namespace Engine

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading
open System.Diagnostics
open Microsoft.FSharp.NativeInterop

module private NativeRenderer =
    
    [<Literal>]
    let libQuake3 = "quake3.dll"

    [<Literal>]
    let callingConvention = CallingConvention.Cdecl

    //[<DllImport (libQuake3, CallingConvention = callingConvention)>]
    //extern int R_CullLocalBox ([<MarshalAs (UnmanagedType.LPArray, SizeConst = 2)>] Vector3[] bounds)

    [<DllImport (libQuake3, CallingConvention = callingConvention)>]
    extern bool Cvar_GetNoCull ()

type CullType =
    | In = 0
    | Clip = 1
    | Out = 2

module Cvar =
    
    let GetNoCull () =
        NativeRenderer.Cvar_GetNoCull ()


module Renderer =
    
    let CullLocalBox (startBound: Vector3) (endBound: Vector3) = 
        
        match Cvar.GetNoCull () with
        | true -> CullType.Clip
        | _ ->

        CullType.Clip
