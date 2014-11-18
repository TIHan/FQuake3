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

type ILiteWriteStream =
    abstract Position : int
    abstract Length : int
    abstract Seek : int -> unit
    abstract Skip : int -> unit
    abstract WriteByte : byte -> unit
    abstract WriteBytes : int -> byte [] -> unit
    abstract WriteString : int -> StringKind -> string -> unit
    abstract Write<'a when 'a : unmanaged> : 'a -> unit

type ByteResizeWriteStream (bytes: byte ResizeArray) =
    let mutable position = 0

    let insert i byte =
        if bytes.Count < i + 1 then 
            bytes.Capacity <- i + 1

        let diff = i + 1 - bytes.Count
        if diff > 0 then
            bytes.AddRange (Array.zeroCreate diff)

        bytes.[i] <- byte

    let insertRange i (bytes': byte[]) =
        if bytes.Count < i + bytes'.Length then
            bytes.Capacity <- i + bytes'.Length

        let diff = i + bytes'.Length - bytes.Count
        if diff > 0 then
            bytes.AddRange (Array.zeroCreate diff)

        bytes' |> Array.iter (fun byte -> bytes.[i] <- byte)

    interface ILiteWriteStream with
        member this.Position = position

        member this.Length = bytes.Count

        member this.Seek offset = position <- offset

        member this.Skip n = position <- position + n

        member this.WriteByte byte =
            insert position byte
            position <- position + 1

        member this.WriteBytes n bytes' =
            insertRange position bytes'.[..n - 1]
            position <- position + n

        // TODO: please fix
        member this.WriteString n kind string = 
            match kind with
            | EightBit ->
                let length = string.Length

                for i = 1 to length do
                    insert position (byte <| sbyte string.[i - 1])
                    position <- position + 1
 
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

                let bytes' = encoding.GetBytes (string)
                insertRange position bytes'
                position <- position + bytes'.Length

        member this.Write<'a when 'a : unmanaged> (a: 'a) =
            let mutable a = a
            let size = sizeof<'a>
            let ptr : nativeptr<byte> = &&a |> NativePtr.toNativeInt |> NativePtr.ofNativeInt

            for i = 1 to size do
                insert position (NativePtr.get ptr (i - 1))
                position <- position + 1

type Pickle<'a> = ILiteWriteStream -> 'a -> unit

let p_byte : Pickle<_> =
    fun stream x -> stream.WriteByte x

let inline p_bytes n : Pickle<_> =
    fun stream xs -> stream.WriteBytes n xs

let p_int16 : Pickle<_> =
    fun stream x -> stream.Write<int16> x

let p_int32 : Pickle<_> =
    fun stream x -> stream.Write<int32> x

let p_single : Pickle<_> =
    fun stream x -> stream.Write<single> x

let p_string n kind : Pickle<_> =
    fun stream x -> stream.WriteString n kind x

let inline p_pipe2 f a b : Pickle<_> =
    fun stream x -> 
        let a',b' = f x
        (a stream a')
        (b stream b')

let inline p_pipe3 f a b c : Pickle<_> =
    fun stream x -> 
        let a',b',c' = f x
        (a stream a')
        (b stream b')
        (c stream c')

let inline p_pipe4 f a b c d : Pickle<_> =
    fun stream x -> 
        let a',b',c',d' = f x
        (a stream a')
        (b stream b')
        (c stream c')
        (d stream d')

let inline p_pipe5 f a b c d e : Pickle<_> =
    fun stream x -> 
        let a',b',c',d',e' = f x
        (a stream a')
        (b stream b')
        (c stream c')
        (d stream d')
        (e stream e')

let inline p_pipe6 f a b c d e g : Pickle<_> =
    fun stream x -> 
        let a',b',c',d',e',g' = f x
        (a stream a')
        (b stream b')
        (c stream c')
        (d stream d')
        (e stream e')
        (g stream g')

let inline p_pipe7 f a b c d e g h : Pickle<_> =
    fun stream x -> 
        let a',b',c',d',e',g',h' = f x
        (a stream a')
        (b stream b')
        (c stream c')
        (d stream d')
        (e stream e')
        (g stream g')
        (h stream h')

let inline p_pipe8 f a b c d e g h i : Pickle<_> =
    fun stream x -> 
        let a',b',c',d',e',g',h',i' = f x
        (a stream a')
        (b stream b')
        (c stream c')
        (d stream d')
        (e stream e')
        (g stream g')
        (h stream h')
        (i stream i')

let inline p_pipe9 f a b c d e g h i j : Pickle<_> =
    fun stream x -> 
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

let inline p_pipe10 f a b c d e g h i j k : Pickle<_> =
    fun stream x -> 
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

let inline p_pipe11 f a b c d e g h i j k l : Pickle<_> =
    fun stream x -> 
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

let inline p_pipe12 f a b c d e g h i j k l m : Pickle<_> =
    fun stream x -> 
        let a',b',c',d',e',g',h',i',j',k', l',m' = f x
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

let inline p_pipe13 f a b c d e g h i j k l m n : Pickle<_> =
    fun stream x -> 
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
    fun stream x -> stream.Write x

let inline p_array n (p: Pickle<'a>) : Pickle<'a[]> =
    fun stream xs ->
        match n with
        | 0 -> ()
        | _ -> xs.[..n - 1] |> Array.iter (p stream)

let inline p_skipBytes n : Pickle<_> =
    fun stream _ -> stream.Skip n

let inline p_lookAhead (p: Pickle<_>) : Pickle<_> =
    fun stream x ->
        let prevPosition = stream.Position
        p stream x
        stream.Seek (prevPosition)

let inline (>>=) (p: Pickle<'a>, g) (f: 'a -> Pickle<'b>) : Pickle<'b> =
    fun stream (x: 'b) ->
        let gx = g x
        p stream gx
        (f gx) stream x

let inline (>>.) (p1: Pickle<'a>) ((p2: Pickle<'b>), f) =
    fun stream x ->
        p1 stream x
        p2 stream (f x)

let (|>>) a f : Pickle<_> =
    fun stream x -> (a stream (f x))

let inline p_run (p: Pickle<_>) bytes = p <| ByteResizeWriteStream (bytes)

