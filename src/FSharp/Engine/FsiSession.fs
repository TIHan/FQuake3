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

namespace Engine.Fsi

open System.Threading
open System.Text
open System.Diagnostics

// based on: http://clear-lines.com/blog/post/Using-FSI-to-execute-FSharp-code-from-a-dot-NET-app.aspx
type FsiSession (fsiPath: string) =

    let info = ProcessStartInfo ()

    let mutable fsiProcess = Unchecked.defaultof<Process>

    let outputObj = obj ()
    let sbOutput = StringBuilder ()

    let errorObj = obj ()
    let sbError = StringBuilder ()

    do
        info.RedirectStandardInput <- true
        info.RedirectStandardOutput <- true
        info.RedirectStandardError <- true
        info.UseShellExecute <- false
        info.CreateNoWindow <- true
        info.FileName <- fsiPath

    member this.IsRunning = fsiProcess <> null && not fsiProcess.HasExited

    member this.WriteLine (line: string) =
        fsiProcess.StandardInput.WriteLine (line)
        fsiProcess.StandardInput.Flush ()

    member this.ReadOutput () =
        lock outputObj (fun _ ->
            let output = sbOutput.ToString ()
            sbOutput.Clear ()
            output
        )

    member this.ReadError () =
        lock errorObj (fun _ ->
            let error = sbError.ToString ()
            sbError.Clear ()
            error
        )

    member this.Start () =
        match this.IsRunning with
        | true -> ()
        | _ ->

        match fsiProcess = null with
        | false ->
            // cleanup
            fsiProcess.Close ()
            fsiProcess.Dispose ()
        | _ -> ()

        fsiProcess <- Process ()
        fsiProcess.StartInfo <- info
        fsiProcess.Start ()
        |> ignore

        sbOutput.Clear ()
        sbError.Clear ()

        fsiProcess.OutputDataReceived.Add (fun args ->
            lock outputObj (fun _ ->
                sbOutput.AppendLine args.Data
                |> ignore
            )
        )
        fsiProcess.BeginOutputReadLine ()

        // handle standard error asynchronously
        async {
            while not fsiProcess.HasExited do
                while not fsiProcess.StandardError.EndOfStream do
                    lock errorObj (fun _ ->
                        sbError.AppendLine <| fsiProcess.StandardError.ReadLine ()
                        |> ignore
                    )
                // sleep a small amount
                Async.Sleep 5
            printfn "F# Interactive Terminated"
        } |> Async.Start

    static member inline StartNew (fsiPath: string) =
        let session = FsiSession fsiPath
        session.Start ()
        session