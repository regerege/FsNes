namespace FsNes.Core

open System.Runtime.CompilerServices
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsNes.Core

[<Extension>]
type ConfigExtensions =
    [<Extension>]
    static member inline IsConfig(actual : Config, expected : Config) =
        actual.CpuSkip.Is(expected.CpuSkip, "CpuSkip is not equal.")
        actual.PpuSkip.Is(expected.PpuSkip, "PpuSkip is not equal.")
        actual.Register.A.Is(expected.Register.A, "Register A is not equal.")
        actual.Register.X.Is(expected.Register.X, "Register X is not equal.")
        actual.Register.Y.Is(expected.Register.Y, "Register Y is not equal.")
        actual.Register.PC.Is(expected.Register.PC, "Register PC is not equal.")
        actual.Register.S.Is(expected.Register.S, "Register S is not equal.")
        actual.Register.P.C.Is(expected.Register.P.C, "Register P(C) is not equal.")
        actual.Register.P.Z.Is(expected.Register.P.Z, "Register P(Z) is not equal.")
        actual.Register.P.I.Is(expected.Register.P.I, "Register P(I) is not equal.")
        actual.Register.P.D.Is(expected.Register.P.D, "Register P(D) is not equal.")
        actual.Register.P.B.Is(expected.Register.P.B, "Register P(B) is not equal.")
        actual.Register.P.V.Is(expected.Register.P.V, "Register P(V) is not equal.")
        actual.Register.P.N.Is(expected.Register.P.N, "Register P(N) is not equal.")
        actual.WRAM.Is(expected.WRAM, "WRAM is not equal.")
        actual.VRAM.Is(expected.VRAM, "VRAM is not equal.")
        actual.Interrupt.Is(expected.Interrupt, "Interrupt is not equal.")
