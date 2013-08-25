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

open System.Diagnostics

type FsiSession (fsiPath: string) =

    let info = new ProcessStartInfo ()
    let fsiProcess = new Process ()

    do
        info.RedirectStandardInput <- true
        info.RedirectStandardOutput <- true
        info.RedirectStandardError <- true
        info.UseShellExecute <- false
        info.CreateNoWindow <- true
        info.FileName <- fsiPath

        fsiProcess.StartInfo <- info

    [<CLIEvent>]
    member this.OutputReceived = fsiProcess.OutputDataReceived

    [<CLIEvent>]
    member this.ErrorReceived = fsiProcess.ErrorDataReceived

    member this.Start () =
        fsiProcess.Start ()
        fsiProcess.BeginOutputReadLine ()

    member this.AddLine (line: string) =
        fsiProcess.StandardInput.WriteLine (line)

    member this.Evaluate () =
        this.AddLine(";;")
        fsiProcess.StandardInput.Flush ()

    member this.WaitForExit () =
        fsiProcess.WaitForExit ()

    member this.ReadError () =
        fsiProcess.StandardError.ReadToEnd ()

    static member inline StartNew (fsiPath: string) =
        let session = FsiSession fsiPath
        session.Start ()
        session