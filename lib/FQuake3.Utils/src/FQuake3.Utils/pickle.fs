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

module FSharp.LitePickler.Pickle

open System
open System.IO
open System.Text
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

#nowarn "9"
#nowarn "51"

type StringKind =
    | Default
    | EightBit
    | ASCII
    | BigEndianUnicode
    | Unicode
    | UTF32
    | UTF7
    | UTF8

type Pickle<'a> = 'a -> Stream -> unit

module Stream =
    let write<'a when 'a : unmanaged> (a: 'a) (stream: Stream) =
        let mutable a = a
        let size = sizeof<'a>
        let ptr : nativeptr<byte> = &&a |> NativePtr.toNativeInt |> NativePtr.ofNativeInt

        for i = 1 to size do
            stream.WriteByte (NativePtr.get ptr (i - 1))

    let writeString n kind (string: string) (stream: Stream) =
        match kind with
        | EightBit ->
            let length = string.Length

            for i = 0 to n - 1 do
                if i >= length
                then stream.WriteByte (0uy)
                else stream.WriteByte (byte <| sbyte string.[i])
 
        | _ ->
            let encoding =
                match kind with
                | ASCII -> System.Text.Encoding.ASCII
                | BigEndianUnicode -> System.Text.Encoding.BigEndianUnicode
                | Unicode -> System.Text.Encoding.Unicode
                | UTF32 -> System.Text.Encoding.UTF32
                | UTF7 -> System.Text.Encoding.UTF7
                | UTF8 -> System.Text.Encoding.UTF8
                | _ -> System.Text.Encoding.Default

            let bytes = encoding.GetBytes (string)
            let length = bytes.Length

            for i = 0 to n - 1 do
                if i >= length
                then stream.WriteByte (0uy)
                else stream.WriteByte (bytes.[i])

let p_byte : Pickle<byte> =
    fun x stream -> stream.WriteByte x

let inline p_bytes n : Pickle<byte []> =
    fun xs stream -> stream.Write (xs, 0, n)

let p_int16 : Pickle<int16> =
    fun x stream -> Stream.write<int16> x stream

let p_int32 : Pickle<int32> =
    fun x stream -> Stream.write<int32> x stream

let p_single : Pickle<single> =
    fun x stream -> Stream.write<single> x stream

let p_string n kind : Pickle<string> =
    fun x stream -> Stream.writeString n kind x stream

let inline p_pipe2 a b f : Pickle<_> =
    fun x stream -> 
        let a',b' = f x
        (a a' stream)
        (b b' stream)

let inline p_pipe3 a b c f : Pickle<_> =
    fun x stream -> 
        let a',b',c' = f x
        (a a' stream)
        (b b' stream)
        (c c' stream)

let inline p_pipe4 a b c d f : Pickle<_> =
    fun x stream -> 
        let a',b',c',d' = f x
        (a stream a')
        (b stream b')
        (c stream c')
        (d stream d')

let inline p_pipe5 a b c d e f : Pickle<_> =
    fun x stream -> 
        let a',b',c',d',e' = f x
        (a a' stream)
        (b b' stream)
        (c c' stream)
        (d d' stream)
        (e e' stream)

let inline p_pipe6 a b c d e g f : Pickle<_> =
    fun x stream -> 
        let a',b',c',d',e',g' = f x
        (a stream a')
        (b stream b')
        (c stream c')
        (d stream d')
        (e stream e')
        (g stream g')

let inline p_pipe7 a b c d e g h f : Pickle<_> =
    fun x stream -> 
        let a',b',c',d',e',g',h' = f x
        (a stream a')
        (b stream b')
        (c stream c')
        (d stream d')
        (e stream e')
        (g stream g')
        (h stream h')

let inline p_pipe8 a b c d e g h i f : Pickle<_> =
    fun x stream -> 
        let a',b',c',d',e',g',h',i' = f x
        (a stream a')
        (b stream b')
        (c stream c')
        (d stream d')
        (e stream e')
        (g stream g')
        (h stream h')
        (i stream i')

let inline p_pipe9 a b c d e g h i j f : Pickle<_> =
    fun x stream -> 
        let a',b',c',d',e',g',h',i',j' = f x
        (a stream a')
        (b stream b')
        (c stream c')
        (d stream d')
        (e stream e')
        (g stream g')
        (h stream h')
        (i stream i')
        (j stream j')

let inline p_pipe10 a b c d e g h i j k f : Pickle<_> =
    fun x stream -> 
        let a',b',c',d',e',g',h',i',j',k' = f x
        (a stream a')
        (b stream b')
        (c stream c')
        (d stream d')
        (e stream e')
        (g stream g')
        (h stream h')
        (i stream i')
        (j stream j')
        (k stream k')

let inline p_pipe11 a b c d e g h i j k l f : Pickle<_> =
    fun x stream -> 
        let a',b',c',d',e',g',h',i',j',k', l' = f x
        (a stream a')
        (b stream b')
        (c stream c')
        (d stream d')
        (e stream e')
        (g stream g')
        (h stream h')
        (i stream i')
        (j stream j')
        (k stream k')
        (l stream l')

let inline p_pipe12 a b c d e g h i j k l m f : Pickle<_> =
    fun x stream -> 
        let a',b',c',d',e',g',h',i',j',k', l',m' = f x
        (a a' stream)
        (b b' stream)
        (c c' stream)
        (d d' stream)
        (e e' stream)
        (g g' stream)
        (h h' stream)
        (i i' stream)
        (j j' stream)
        (k k' stream)
        (l l' stream)
        (m m' stream)

let inline p_pipe13 a b c d e g h i j k l m n f : Pickle<_> =
    fun x stream -> 
        let a',b',c',d',e',g',h',i',j',k', l',m',n' = f x
        (a stream a')
        (b stream b')
        (c stream c')
        (d stream d')
        (e stream e')
        (g stream g')
        (h stream h')
        (i stream i')
        (j stream j')
        (k stream k')
        (l stream l')
        (m stream m')
        (n stream n')

let p : Pickle<_> =
    fun x stream -> stream.Write x

let inline p_array n (p: Pickle<'a>) : Pickle<'a[]> =
    fun xs stream ->
        match n with
        | 0 -> ()
        | _ -> for i = 0 to n - 1 do p xs.[i] stream

let inline p_skipBytes n : Pickle<_> =
    fun _ stream -> stream.Seek (n, SeekOrigin.Current) |> ignore

let inline p_lookAhead (p: Pickle<_>) : Pickle<_> =
    fun x stream ->
        let prevPosition = stream.Position
        p x stream
        stream.Seek (prevPosition, SeekOrigin.Begin) |> ignore

// contramap
let inline (>>|) (p: Pickle<'a>) (f: 'b -> 'a) : Pickle<'b> =
    fun b' stream -> p (f b') stream

// ?
let inline (=>>) (p: Pickle<'a>) (f: 'b -> 'a * Pickle<'b>) : Pickle<'b> =
    fun b' stream ->
        let a', p2 = f b'
        p a' stream
        p2 b' stream

let inline p_run (p: Pickle<_>) x stream = p x stream

