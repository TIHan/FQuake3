module FQuake3.Utils.Md3

open FSharp.LitePickler.Unpickle
open FSharp.LitePickler.Pickle
open FQuake3.Md3
open FQuake3.Utils.Internal.Md3Parser
open FQuake3.Utils.Internal.PickleMd3

let parse bytes = u_run u_md3 bytes

let pickle md3 = 
    // hard coding this for now
    let bytes = Array.zeroCreate 1306652
    p_run p_md3 bytes md3
    bytes