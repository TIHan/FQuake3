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

namespace Engine.Net

open System.Net
open System.Net.Sockets
open FSharp.Control

type ErlNetClient () =
    let socket_ = Socket (AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp, NoDelay = true)
    let buffer_ = Array.zeroCreate<byte> 8192

    member this.Connect (host: string) port =
        socket_.Connect (host, port)

    member this.TryConnect host port =
        try
            this.Connect host port
            true
        with
        _ -> false

    member this.Disconnect () =
        socket_.Disconnect true

    member this.TryDisconnect () =
        try
            this.Disconnect ()
            true
        with
        _ -> false

    member this.Send (bytes: byte []) =
        socket_.Send bytes

    member this.TrySend bytes =
        try
            let size = this.Send bytes
            size = bytes.Length
        with
        _ -> false

    member this.Receive () =
        try
            let size = socket_.Receive(buffer_)
            buffer_.[..size - 1]
        with
        _ -> [||]

module ErlNet =
    [<Literal>]
    let private Ping = 255uy

    [<Literal>]
    let private Pong = 255uy

    type private ErlNetMessage =
        | Connect of AsyncReplyChannel<bool>
        | Disconnect of AsyncReplyChannel<bool>
        | Call of byte [] * AsyncReplyChannel<byte []>

    let private callAgent = Agent<ErlNetMessage>.Start(fun agent ->
        let rec loop (client: ErlNetClient) =
            async {
                let! msg = agent.Receive()

                match msg with
                | Connect channel ->
                    channel.Reply <| client.TryConnect "localhost" 37950
                    return! loop client
                | Disconnect channel ->
                    channel.Reply <| client.TryDisconnect ()
                    return! loop client
                | Call (bytes, channel) ->
                    match client.TrySend bytes with
                    | false ->
                        channel.Reply [||]
                    | _ ->
                        channel.Reply <| client.Receive ()
                    return! loop client
                | _ ->
                    return! loop client
            }
        loop (ErlNetClient ())
    )

    let tryConnect () =
        callAgent.PostAndReply (fun x -> Connect x)

    let tryDisconnect () =
        callAgent.PostAndReply (fun x -> Disconnect x)

    let tryInit () =
        tryConnect ()

    let call (bytes: byte []) =
        callAgent.PostAndReply (fun x -> Call (bytes, x))

    let ping () =
        match call [| Ping |] with
        | [| Pong |] -> true
        | _ -> false

