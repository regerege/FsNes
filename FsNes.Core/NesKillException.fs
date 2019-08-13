namespace FsNes.Core

open System

type NesKillException =
    inherit Exception 
    new () = { inherit Exception() }
    new (message:string) = { inherit Exception(message) }
    new (message:string, ex:Exception) = { inherit Exception(message, ex) }
