module FQuake3.Utils.Md3

open FSharp.LitePickler.Unpickle
open FSharp.LitePickler.Pickle
open FQuake3.Md3
open FQuake3.Utils.Internal.Md3Parser
open FQuake3.Utils.Internal.PickleMd3

let parse bytes = u_run u_md3 bytes

let pickle md3 = 
    let name = "test.md3"
    let path = System.IO.Path.Combine (System.Environment.CurrentDirectory, name)
    use file = System.IO.File.Create (name)
    p_run p_md3 md3 <| WriteStream (file)
    file.Close ()
    System.IO.File.ReadAllBytes (path)