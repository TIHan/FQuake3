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

        bytes.[i] <- byte

    let insertRange i (bytes': byte[]) =
        if bytes.Count < i + bytes'.Length then
            bytes.Capacity <- i + bytes'.Length

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
            insertRange position bytes'.[0..n]
            position <- position + n

        // TODO: please fix
        member this.WriteString n kind string = 
            match kind with
            | EightBit ->
                let length = string.Length

                for i = 1 to length do
                    insert position (byte <| sbyte string.[i - 1])
                    position <- position + i
 
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
                bytes.Add (NativePtr.get ptr (i - 1))
                position <- position + i

type Pickle = ILiteWriteStream -> unit

let inline p_byte x : Pickle =
    fun stream -> stream.WriteByte x

let inline p_bytes n xs : Pickle =
    fun stream -> stream.WriteBytes n xs

let inline p_int16 x : Pickle =
    fun stream -> stream.Write<int16> x

let inline p_int32 x : Pickle =
    fun stream -> stream.Write<int32> x

let inline p_single x : Pickle =
    fun stream -> stream.Write<single> x

let inline p_string n kind x : Pickle =
    fun stream -> stream.WriteString n kind x

let inline p_pipe2 a b : Pickle =
    fun stream -> 
        a stream
        b stream

let inline p x : Pickle =
    fun stream -> stream.Write x

let inline p_array n (arr: 'a []) : Pickle =
    fun stream ->
        match n with
        | 0 -> ()
        | _ -> arr.[0..n] |> Array.iter (fun x -> p x stream)

let inline p_skipBytes n : Pickle =
    fun stream -> stream.Skip n

let inline p_lookAhead (p: Pickle) : Pickle =
    fun stream ->
        let prevPosition = stream.Position
        let result = p stream
        stream.Seek (prevPosition)
        result

let inline (>>=) (p: Pickle) (f: unit -> Pickle) =
    fun stream -> f (p stream) stream

let (>>.) = p_pipe2
//
//let inline (>>=) (p: Reader<'a>) (f: 'a -> Reader<'b>) =
//    fun stream -> f (p stream) stream
//
//let inline (>>.) (p1: Reader<'a>) (p2: Reader<'b>) =
//    fun stream ->
//        p1 stream |> ignore
//        p2 stream
//
//let inline (|>>) p f =
//    fun stream -> f (p stream)
//
//let inline u_run (p: Reader<_>) (bytes: byte []) = p <| ByteReadStream (bytes)

