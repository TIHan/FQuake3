(*
Copyright (c) 2013 OpenFK

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*)

#nowarn "9" // No warnings for interop; we know what we are doing.
#nowarn "51"

namespace OpenFK.Core

open System
open System.Threading
open System.Collections
open System.Collections.Generic
open System.Collections.Concurrent
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

module NativeCpu =
    [<DllImport ("OpenFK.Native.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern int fk_cpu_get_physical_core_count ()

module Cpu  =
    let inline GetPhysicalCoreCount () =
        NativeCpu.fk_cpu_get_physical_core_count ()

module ConcurrentExtensions =
    type ConcurrentQueue<'T> with
        member __.Dequeue () =
            let mutable item : 'T = Unchecked.defaultof<'T>
        
            match __.TryDequeue (&item) with
            | false -> raise <| Exception ("Unable to dequeue item in concurrent queue.")
            | _ -> item
    
    

