namespace FsNes.Core.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsNes.Core

[<TestClass>]
type CommonTest () =

    [<TestMethod>]
    member this.ByteWordTest () =
        let list = [ 0xAAuy; 0xCCuy; ]
        let array = [| 0xBBuy; 0xDDuy; |]

        list.ByteWord().Is(0xCCAA, "Byte�^���X�g����ByteWord�̕ϊ��Ɏ��s���܂����B")
        array.ByteWord().Is(0xDDBB, "Byte�^�z�񂩂�ByteWord�̕ϊ��Ɏ��s���܂����B")

