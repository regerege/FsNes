namespace FsNes.Core.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsNes.Core

[<TestClass>]
type CpuTest () =
    [<TestMethod>]
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

        // PUSH�̃e�X�g
        let acm2 = Cpu.push acm [| 0x11uy; 0x33uy; 0x55uy; 0x77uy; |]
        acm2.Register.S.Is(0xFCuy)
        acm2.WRAM.[0x0100].Is(0x77uy)
        acm2.WRAM.[0x01FF].Is(0x55uy)
        acm2.WRAM.[0x01FE].Is(0x33uy)
        acm2.WRAM.[0x01FD].Is(0x11uy)

        // POP�̃e�X�g
        let acm3,value = Cpu.pop acm2 5
        acm3.Register.S.Is(0x01uy)
        value.[0].Is(0x11uy)
        value.[1].Is(0x33uy)
        value.[2].Is(0x55uy)
        value.[3].Is(0x77uy)
        value.[4].Is(0x00uy)
        ()

    [<TestMethod>]
    member this.AdcTest () =
        let config : Config =
            {
                CpuSkip = 0
                PpuSkip = 0
                Register = { A = 0xFDuy; X = 3uy; Y = 5uy; PC = 0x8000us; S = 0uy; P = { C = 0uy; Z = Masks.StatusFlag.Z; I = 0uy; D = 0uy; B = 0uy; V = Masks.StatusFlag.V; N = Masks.StatusFlag.N; } }
                WRAM = [| for i in 0..0xFFFF -> 0uy |]
                VRAM = [| for i in 0..0xFFFF -> 0uy |]
                Interrupt = Interrupt.Empty
            }

        // 0x61 ::  ADC ($43,X)  ::  (Indirect, X)
        config.WRAM.[0x8000] <- 0x61uy
        config.WRAM.[0x8001] <- 0x11uy       // 0x11 + 0x03 = 0x14
        config.WRAM.[0x0014] <- 0x33uy
        config.WRAM.[0x0033] <- 0x05uy
        let ret61 =
            Cpu.createAccumulator 0x61 config
            |> (Cpu.iix >> Cpu._adc >> Cpu.updateNZ)
        ret61.Value.IsStructuralEqual([0x02uy])
        ret61.Register.P.Value.Is(Masks.StatusFlag.C)