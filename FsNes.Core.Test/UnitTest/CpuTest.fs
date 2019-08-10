namespace FsNes.Core.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsNes.Core

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
        let acm = Cpu.createAccumulator 0x00 config

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
    [<TestCategory("0x61")>]
    [<TestCategory("ADC")>]
    [<TestCategory("(Indirect, X)")>]
    member this.``0x61 ADC (Indirect, X) :: Carry On`` () =
        // A <- A + {data] + C
        // 0xFDuy + 5uy + 0uy = 2uy
        // A = 2uy. //

        // Expected
        let expected : Config =
            {
                CpuSkip = 6
                PpuSkip = 0
                Register =
                    {
                        A = 2uy
                        X = 3uy
                        Y = 5uy
                        PC = 0x8002us
                        S = 0uy
                        P =
                            {
                                C = 1uy
                                Z = 0uy
                                I = 0uy
                                D = 0uy
                                B = 0uy
                                V = 0uy
                                N = 0uy
                            }
                    }
                WRAM = [| for i in 0..0xFFFF -> 0uy |]
                VRAM = [| for i in 0..0xFFFF -> 0uy |]
                Interrupt = Interrupt.Empty
            }
        expected.WRAM.[0x8000] <- 0x61uy      // opcode
        expected.WRAM.[0x8001] <- 0x11uy      // oprand :: 0x11 + 0x03 = 0x14
        expected.WRAM.[0x0014] <- 0x33uy      // data (pointer)
        expected.WRAM.[0x0033] <- 0x05uy      // data (target)

        // Test Value
        let config : Config =
            {
                CpuSkip = 0
                PpuSkip = 0
                Register =
                    {
                        A = 0xFDuy
                        X = 3uy
                        Y = 5uy
                        PC = 0x8000us
                        S = 0uy
                        P =
                            {
                                C = 0uy
                                Z = 1uy
                                I = 0uy
                                D = 0uy
                                B = 0uy
                                V = 1uy
                                N = 1uy
                            }
                    }
                WRAM = [| for i in 0..0xFFFF -> 0uy |]
                VRAM = [| for i in 0..0xFFFF -> 0uy |]
                Interrupt = Interrupt.Empty
            }
        config.WRAM.[0x8000] <- 0x61uy      // opcode
        config.WRAM.[0x8001] <- 0x11uy      // oprand :: 0x11 + 0x03 = 0x14
        config.WRAM.[0x0014] <- 0x33uy      // data (pointer)
        config.WRAM.[0x0033] <- 0x05uy      // readdata

        // Testing
        let ret = Cpu.run config
        ret.IsConfig(expected);
