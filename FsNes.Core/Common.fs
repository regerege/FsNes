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

    type Destination with
        member x.Is(expected:Destination) =
            x &&& expected = expected
        member x.IsNone = x &&& Destination.None = Destination.None
        member x.IsAddress = x &&& Destination.Address = Destination.Address
        member x.IsMemory = x &&& Destination.Memory = Destination.Memory
        member x.IsA = x &&& Destination.A = Destination.A
        member x.IsX = x &&& Destination.X = Destination.X
        member x.IsY = x &&& Destination.Y = Destination.Y
        member x.IsPC = x &&& Destination.PC = Destination.PC
        member x.IsS = x &&& Destination.S = Destination.S
        member x.IsC = x &&& Destination.C = Destination.C
        member x.IsZ = x &&& Destination.Z = Destination.Z
        member x.IsI = x &&& Destination.I = Destination.I
        member x.IsD = x &&& Destination.D = Destination.D
        member x.IsB = x &&& Destination.B = Destination.B
        member x.IsV = x &&& Destination.V = Destination.V
        member x.IsN = x &&& Destination.N = Destination.N
        member x.IsP = x &&& Destination.P = Destination.P

[<Extension>]
type ByteArrayExtensions =
    [<Extension>]
    static member inline ByteWord(arr: byte[]) = Common.getByteWord arr

[<Extension>]
type ByteListExtensions =
    [<Extension>]
    static member inline ByteWord(l: byte list) = Common.getByteWord l

