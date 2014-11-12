module FQuake3.Utils.Md3

open FSharp.LitePickler.Reader
open FQuake3.Md3
open FQuake3.Utils.Internal.Md3Parser

let parse bytes = u_run u_md3 bytes