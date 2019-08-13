namespace FsNes.Core.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsNes.Core
open FsNes.Core.Test.TestCommon

[<TestClass>]
type CpuTest () =
    [<TestMethod>]
    [<TestCategory("CPU Functions")>]
    member this.StackTest () =
        let config : Config =
            {
                CpuSkip = 0
                PpuSkip = 0
                Register = { A = 0uy; X = 0uy; Y = 0uy; PC = 0us; S = 0uy; P = { C = 0uy; Z = 0uy; I = 0uy; D = 0uy; B = 0uy; V = 0uy; N = 0uy; } }
                WRAM = [| for i in 0..0xFFFF -> 0uy |]
                VRAM = [| for i in 0..0xFFFF -> 0uy |]
                Interrupt = Interrupt.Empty
            }
        let acm = Cpu.createAccumulator config

        // PUSHのテスト
        let acm2 = Cpu.push acm [| 0x11uy; 0x33uy; 0x55uy; 0x77uy; |]
        acm2.Register.S.Is(0xFCuy)
        acm2.WRAM.[0x0100].Is(0x77uy)
        acm2.WRAM.[0x01FF].Is(0x55uy)
        acm2.WRAM.[0x01FE].Is(0x33uy)
        acm2.WRAM.[0x01FD].Is(0x11uy)

        // POPのテスト
        let acm3,value = Cpu.pop acm2 5
        acm3.Register.S.Is(0x01uy)
        value.[0].Is(0x11uy)
        value.[1].Is(0x33uy)
        value.[2].Is(0x55uy)
        value.[3].Is(0x77uy)
        value.[4].Is(0x00uy)
        ()

    [<TestMethod>]
    [<TestCategory("CPU Instructions")>]
    [<TestCategory("(Indirect, X)")>]
    member this.``0x61 ADC (Indirect, X) :: Carry On`` () =
        // A <- A + {data] + C
        // 0xFDuy + 5uy + 1uy = 2uy

        let config = TestReader.Read @"..\..\..\InstructionTests\0x61_ADC\IN_CarryOn.test"
        let expected = TestReader.Read @"..\..\..\InstructionTests\0x61_ADC\OUT_CarryOn.test"

        // Testing
        Cpu.step config
        |> isConfig expected
