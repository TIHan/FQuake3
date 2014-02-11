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

module FSharp.LiteParsec

open System
open System.Text
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

#nowarn "9"
#nowarn "51"

type ByteStream (bytes: byte []) =
    let mutable position = 0
    let length = bytes.Length

    member this.Position = position

    member this.Length = length

    member this.Seek offset = position <- offset

    member this.Peek offset = bytes.[position + offset]

    member this.Skip n = position <- position + n

    member this.ReadByte () =
        position <- position + 1
        bytes.[position]

    member this.ReadBytes n =
        let i = position
        position <- position + n
        bytes.[i..position]

    member this.ReadString n = 
        let s : nativeptr<sbyte> = (NativePtr.ofNativeInt <| NativePtr.toNativeInt &&bytes.[position])
        let result = String (s, position, n, Encoding.Unicode)
        position <- position + n
        result

    member this.Read<'a when 'a : unmanaged> () =
        let result = NativePtr.read (NativePtr.ofNativeInt<'a> <| NativePtr.toNativeInt &&bytes.[position])
        position <- position + sizeof<'a>
        result

type Parser<'a> = ByteStream -> 'a

let anyByte : Parser<byte> =
    fun stream -> stream.ReadByte ()

let anyBytes n : Parser<byte []> =
    fun stream -> stream.ReadBytes (n)

let anyInt16 : Parser<int16> =
    fun stream -> stream.Read<int16> ()

let anyInt32 : Parser<int> =
    fun stream -> stream.Read<int32> ()

let anySingle : Parser<single> =
    fun stream -> stream.Read<single> ()

let inline anyString n : Parser<string> =
    fun stream -> stream.ReadString n

let inline (|>>) p f =
    fun stream -> f (p stream)

let inline pipe2 a b f : Parser<_> =
    fun stream -> f (a stream) (b stream)

let inline pipe3 a b c f : Parser<_> =
    fun stream -> f (a stream) (b stream) (c stream)

let inline pipe4 a b c d f : Parser<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream)

let inline pipe5 a b c d e f : Parser<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream)

let inline pipe6 a b c d e g f : Parser<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream)

let inline pipe7 a b c d e g h f : Parser<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream)

let inline pipe8 a b c d e g h i f : Parser<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream) (i stream)

let inline pipe9 a b c d e g h i j f : Parser<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream) (i stream) (j stream)

let inline pipe10 a b c d e g h i j k f : Parser<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream) (i stream) (j stream) (k stream)

let inline pipe11 a b c d e g h i j k l f : Parser<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream) (i stream) (j stream) (k stream) (l stream)

let inline pipe12 a b c d e g h i j k l m f : Parser<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream) (i stream) (j stream) (k stream) (l stream) (m stream)

let inline pipe13 a b c d e g h i j k l m n f : Parser<_> =
    fun stream -> f (a stream) (b stream) (c stream) (d stream) (e stream) (g stream) (h stream) (i stream) (j stream) (k stream) (l stream) (m stream) (n stream)

let inline any1<'a when 'a : unmanaged> : Parser<_> =
    fun stream -> stream.Read<'a> ()

let inline parray n (p: Parser<'a>) =
    fun stream ->
        match n with
        | 0 -> [||]
        | _ -> Array.init n (fun _ -> p stream)

let inline skipAnyBytes n : Parser<_> =
    fun stream -> stream.Skip (n)

let inline lookAhead (p: Parser<'a>) : Parser<'a> =
    fun stream ->
        let prevPosition = stream.Position
        let result = p stream
        stream.Seek (prevPosition)
        result

let inline (>>=) (p: Parser<'a>) (f: 'a -> Parser<'b>) =
    fun stream -> f (p stream) stream

let inline (>>.) (p1: Parser<'a>) (p2: Parser<'b>) =
    fun stream ->
        p1 stream |> ignore
        p2 stream

let inline run (p: Parser<_>) (bytes: byte []) = p <| ByteStream (bytes)
