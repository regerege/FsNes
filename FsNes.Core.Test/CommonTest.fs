namespace FsNes.Core.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsNes.Core
open FsNes.Core.Test.AssertModule

[<TestClass>]
type CommonTest () =
    [<TestMethod>]
    [<TestCategory("Common Tests")>]
    member this.ByteWordTest () =
        let list = [ 0xAAuy; 0xCCuy; ]
        let array = [| 0xBBuy; 0xDDuy; |]

        is "Byte�^���X�g����ByteWord�̕ϊ��Ɏ��s���܂����B"
            <| 0xCCAA
            <| list.ByteWord()
        is "Byte�^���X�g����ByteWord�̕ϊ��Ɏ��s���܂����B"
            <| 0xDDBB
            <| array.ByteWord()

