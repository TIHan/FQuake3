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

namespace Engine.IO

open System
open System.IO

/// Standard IO
/// Can easily redirect the output based on a function.
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

module IO =
    let GetCurrentDirectory () =
        Directory.GetCurrentDirectory ()

