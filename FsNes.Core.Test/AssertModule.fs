namespace FsNes.Core.Test

open System
open System.Collections
open FsNes.Core
open Microsoft.VisualStudio.TestTools.UnitTesting

module AssertModule =
    [<System.Diagnostics.DebuggerStepThroughAttribute>]
    let isIEnumerable obj =
        let t = obj.GetType()
        t <> typeof<string> && typeof<IEnumerable>.IsAssignableFrom(t)

    [<System.Diagnostics.DebuggerStepThroughAttribute>]
    let toSeq (e:IEnumerable) =
        seq [ for a in e -> a ]

    [<System.Diagnostics.DebuggerStepThroughAttribute>]
    let rec is message expected actual =
        if isIEnumerable expected then
            let e = unbox<IEnumerable>(expected)
            let a = unbox<IEnumerable>(actual)
            Seq.zip
                <| toSeq e
                <| toSeq a
            |> Seq.iter (fun (e,a) -> is message e a)
        else
            Assert.AreEqual(expected, actual, message)

    let isConfig (expected:Config) (actual:Config) =
        is "CpuSkip is not equal." expected.CpuSkip actual.CpuSkip
        is "PpuSkip is not equal." expected.PpuSkip actual.PpuSkip
        is "Register A is not equal." expected.Register.A actual.Register.A
        is "Register X is not equal." expected.Register.X actual.Register.X
        is "Register Y is not equal." expected.Register.Y actual.Register.Y
        is "Register PC is not equal." expected.Register.PC actual.Register.PC
        is "Register S is not equal." expected.Register.S actual.Register.S
        is "Register P(C) is not equal." expected.Register.P.C actual.Register.P.C
        is "Register P(Z) is not equal." expected.Register.P.Z actual.Register.P.Z
        is "Register P(I) is not equal." expected.Register.P.I actual.Register.P.I
        is "Register P(D) is not equal." expected.Register.P.D actual.Register.P.D
        is "Register P(B) is not equal." expected.Register.P.B actual.Register.P.B
        is "Register P(V) is not equal." expected.Register.P.V actual.Register.P.V
        is "Register P(N) is not equal." expected.Register.P.N actual.Register.P.N
        is "WRAM is not equal." expected.WRAM actual.WRAM
        is "VRAM is not equal." expected.VRAM actual.VRAM
        is "Interrupt is not equal." expected.Interrupt actual.Interrupt

    let isCpuAccumulator (expected:CpuAccumulator) (actual:CpuAccumulator) =
        is "Size is not equal." expected.Size actual.Size
        is "Cycle is not equal." expected.Cycle actual.Cycle
        is "Opcode is not equal." expected.Opcode actual.Opcode
        is "Oprand is not equal." expected.Oprand actual.Oprand
        is "Address is not equal." expected.Address actual.Address
        is "Value is not equal." expected.Value actual.Value
        is "Destination is not equal." expected.Destination actual.Destination
        is "Register A is not equal." expected.Register.A actual.Register.A
        is "Register X is not equal." expected.Register.X actual.Register.X
        is "Register Y is not equal." expected.Register.Y actual.Register.Y
        is "Register PC is not equal." expected.Register.PC actual.Register.PC
        is "Register S is not equal." expected.Register.S actual.Register.S
        is "Register P(C) is not equal." expected.Register.P.C actual.Register.P.C
        is "Register P(Z) is not equal." expected.Register.P.Z actual.Register.P.Z
        is "Register P(I) is not equal." expected.Register.P.I actual.Register.P.I
        is "Register P(D) is not equal." expected.Register.P.D actual.Register.P.D
        is "Register P(B) is not equal." expected.Register.P.B actual.Register.P.B
        is "Register P(V) is not equal." expected.Register.P.V actual.Register.P.V
        is "Register P(N) is not equal." expected.Register.P.N actual.Register.P.N
        is "WRAM is not equal." expected.WRAM actual.WRAM
        