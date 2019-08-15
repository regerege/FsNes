namespace FsNes.Core.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsNes.Core
open FsNes.Core.Test.AssertModule

[<TestClass>]
type TestReaderTest () =
    [<TestMethod>]
    [<TestCategory("Test Common Tests")>]
    member this.``test file read test`` () =
        let a = TestReader.Read @"..\..\..\InstructionTests\Base.test"
        let b : Config =
            {
                CpuSkip = 0
                PpuSkip = 0
                Register = { A = 0uy; X = 0uy; Y = 0uy; PC = 0us; S = 0uy; P = { C = 0uy; Z = 0uy; I = 0uy; D = 0uy; B = 0uy; V = 0uy; N = 0uy; } }
                WRAM = [| for i in 0..0xFFFF -> 0uy |]
                VRAM = [| for i in 0..0xFFFF -> 0uy |]
                Interrupt = Interrupt.Empty
            }
        isConfig b a
