namespace FsNes.Core

open System.Runtime.CompilerServices

module Common =
    let inline getByte xs = Seq.head xs |> byte
    let inline getByteWord xs =
        xs
        |> Seq.take 2
        |> Seq.map (int)
        |> Seq.mapi (fun i n -> n <<< (i * 8))
        |> Seq.reduce ((|||))

[<Extension>]
type ByteArrayExtensions =
    [<Extension>]
    static member inline ByteWord(arr: byte[]) = Common.getByteWord arr

[<Extension>]
type ByteListExtensions =
    [<Extension>]
    static member inline ByteWord(l: byte list) = Common.getByteWord l
