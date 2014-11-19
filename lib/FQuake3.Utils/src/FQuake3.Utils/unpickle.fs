(*
Copyright (c) 2014 William F. Smith

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

module FSharp.LitePickler.Unpickle

open System
open System.IO
open System.Text
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

#nowarn "9"
#nowarn "51"

type ILiteReadStream =
    abstract Position : int
    abstract Length : int
    abstract Seek : int -> unit
    abstract Peek : int -> byte
    abstract Skip : int -> unit
    abstract ReadByte : unit -> byte
    abstract ReadBytes : int -> byte []
    abstract ReadString  : int -> string
    abstract Read<'a when 'a : unmanaged> : unit -> 'a

type ByteReadStream (bytes: byte []) =
    let mutable position = 0
    let length = bytes.Length

    interface ILiteReadStream with
        member this.Position = position

        member this.Length = length

        member this.Seek offset = position <- offset

        member this.Peek offset = bytes.[position + offset]

        member this.Skip n = position <- position + n

        member this.ReadByte () =
            let result = bytes.[position]
            position <- position + 1
            result

        member this.ReadBytes n =
            let i = position
            position <- position + n
            bytes.[i..position]

        member this.ReadString n = 
            let s : nativeptr<sbyte> = (NativePtr.ofNativeInt <| NativePtr.toNativeInt &&bytes.[position])
            let result = String (s, 0, n)
            position <- position + n
            result

        member this.Read<'a when 'a : unmanaged> () =
            let result = NativePtr.read (NativePtr.ofNativeInt<'a> <| NativePtr.toNativeInt &&bytes.[position])
            position <- position + sizeof<'a>
            result

type Unpickle<'a> = ILiteReadStream -> 'a

let u_byte : Unpickle<byte> =
    fun stream -> stream.ReadByte ()

let u_bytes n : Unpickle<byte []> =
    fun stream -> stream.ReadBytes (n)

let u_int16 : Unpickle<int16> =
    fun stream -> stream.Read<int16> ()

let u_int32 : Unpickle<int> =
    fun stream -> stream.Read<int32> ()

let u_single : Unpickle<single> =
    fun stream -> stream.Read<single> ()

let inline u_string n : Unpickle<string> =
    fun stream -> stream.ReadString n

let inline u_pipe2 a b f : Unpickle<_> =
    fun stream -> f (a stream) (b stream)

let inline u_pipe3 a b c f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream)

let inline u_pipe4 a b c d f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream)

let inline u_pipe5 a b c d e f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream)

let inline u_pipe6 a b c d e g f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream)

let inline u_pipe7 a b c d e g h f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream)

let inline u_pipe8 a b c d e g h i f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream) (i stream)

let inline u_pipe9 a b c d e g h i j f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream) (i stream) (j stream)

let inline u_pipe10 a b c d e g h i j k f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream) (i stream) (j stream) (k stream)

let inline u_pipe11 a b c d e g h i j k l f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream) (i stream) (j stream) (k stream) (l stream)

let inline u_pipe12 a b c d e g h i j k l m f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream) (i stream) (j stream) (k stream) (l stream) (m stream)

let inline u_pipe13 a b c d e g h i j k l m n f : Unpickle<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream) (i stream) (j stream) (k stream) (l stream) (m stream) (n stream)

let inline u<'a when 'a : unmanaged> : Unpickle<_> =
    fun stream -> stream.Read<'a> ()

let inline u_array n (p: Unpickle<'a>) =
    fun stream ->
        match n with
        | 0 -> [||]
        | _ -> Array.init n (fun _ -> p stream)

let inline u_skipBytes n : Unpickle<_> =
    fun stream -> stream.Skip (n)

let inline u_lookAhead (p: Unpickle<'a>) : Unpickle<'a> =
    fun stream ->
        let prevPosition = stream.Position
        let result = p stream
        stream.Seek (prevPosition)
        result

let inline (>>=) (p: Unpickle<'a>) (f: 'a -> Unpickle<'b>) : Unpickle<'b> =
    fun stream -> f (p stream) stream

let inline (>>.) (p1: Unpickle<unit>) (p2: Unpickle<'a>) =
    fun stream ->
        p1 stream
        p2 stream

let inline (|>>) p f =
    fun stream -> f (p stream)

let inline u_run (p: Unpickle<_>) (bytes: byte []) = p <| ByteReadStream (bytes)
