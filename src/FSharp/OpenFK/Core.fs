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

/// <summary>
///
/// </summary>
type AgentPriorityType =
    | Low = 0
    | Medium = 1
    | High = 2

/// <summary>
///
/// </summary>
type IAgent =
    abstract member Process : unit -> unit
  
/// <summary>
///
/// </summary>  
type Agent<'State, 'Msg> (initialState: 'State, f: 'State * 'Msg -> 'State) =
    let mutable state: 'State = initialState  
    let mutable msgs = Queue<'Msg> ()
    
    member val Priority = AgentPriorityType.Medium with get, set
    
    member __.Post msg =
        msgs.Enqueue msg

    interface IAgent with
        member this.Process () =
            while msgs.Count <> 0 do
                state <- f (state, msgs.Dequeue ())

/// <summary>
///
/// </summary>
type internal EventAgentThread () =
    let resetEvent = new AutoResetEvent (false)
    let waitResetEvent = new AutoResetEvent (false)
    let agents = List<IAgent> ()

    let thread =
        Thread (fun () ->
            let rec loop () =
                async {
                    resetEvent.WaitOne () |> ignore

                    agents.ForEach (fun x -> x.Process ())

                    waitResetEvent.Set () |> ignore
                    return! loop ()
                }

            Async.StartImmediate (loop ())
        )

    member __.Start () =
        thread.Start ()

    member __.Set () =
        resetEvent.Set () |> ignore
        
    member __.Wait () =
        waitResetEvent.WaitOne () |> ignore

    member __.AddAgent agent =
        agents.Add agent

    member __.Id with get () = thread.ManagedThreadId

    member __.AgentCount with get () = agents.Count

/// <summary>
///
/// </summary>
type AgentPool (threadCount: int) as this =
    let threads =
        Array.init threadCount (fun i ->
            EventAgentThread ()
        )

    do
        threads
        |> Array.iter (fun x ->
            x.Start ()
        )
        this.Process ()

    member __.CreateAgent<'State, 'Msg> (initialState: 'State) (f: 'State * 'Msg -> 'State) =
        let minCount =
            query {
                for thread in threads do
                minBy thread.AgentCount
            }
        let agents =
            query {
                for thread in threads do
                where (thread.AgentCount = minCount)
                head
            }
        let agent = Agent<'State, 'Msg> (initialState, f)
        agents.AddAgent agent
        agent
        
    member __.Process () =
        threads
        |> Array.iter (fun x ->
            x.Set ()
        )
        
        threads
        |> Array.iter (fun x ->
            x.Wait ()
        )

    
    

