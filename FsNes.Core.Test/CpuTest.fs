namespace FsNes.Core.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsNes.Core
open FsNes.Core.Test.AssertModule

[<TestClass>]
type CpuTest () =
    let toCpuAccumulator (config:Config) : CpuAccumulator =
        {
            Size = 0us
            Cycle = -1
            Opcode = -1
            Oprand = List.empty
            Address = -1
            Value = List.empty
            Destination = Destination.None
            Register = config.Register
            WRAM = config.WRAM
        }

    let read relative =
        System.IO.Path.Combine(@"..\..\..\InstructionTests\",relative)
        |> TestReader.Read
    let readToCpuAccumulator = read >> toCpuAccumulator

    let test (actual:string) (expected:string) =
        let config = read actual
        let expected = read expected
        Cpu.step config
        |> isConfig expected

    [<TestMethod>]
    [<TestCategory("CPU Functions")>]
    member this.StackTest () =
        let acm = readToCpuAccumulator @"Stack\INIT_DATA.test"

        // test
        let acm1 = Cpu.push acm [| 0x11uy; 0x33uy; 0x55uy; 0x77uy; |]
        isCpuAccumulator
            <| readToCpuAccumulator @"Stack\PUSH_Expected.test"
            <| acm1

        // POP‚ÌƒeƒXƒg
        let acm2,value = Cpu.pop acm1 5
        isCpuAccumulator
            <| readToCpuAccumulator @"Stack\POP_Expected.test"
            <| acm2
        is "Pop Value is not equal."
            <| [| 0x11uy; 0x33uy; 0x55uy; 0x77uy; 0x00uy; |]
            <| value

    [<TestMethod>]
    [<TestCategory("CPU Instructions")>]
    [<TestCategory("(Indirect, X)")>]
    member this.``0x61 ADC (Indirect, X) :: Carry On`` () =
        // ** Carry On Test
        // A <- A + {data] + C
        // 0xFDuy + 5uy + 1uy = 2uy
        test @"0x61_ADC\IN_CarryOn.test" @"0x61_ADC\OUT_CarryOn.test"
