namespace FsNes.Core

module Cpu =
    let private Bytes =
        [
         // 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
            1; 2; 1; 2; 2; 2; 2; 2; 2; 2; 2; 2; 1; 2; 2; 2; // 0x0*
            2; 2; 1; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; // 0x1*
            2; 2; 1; 2; 2; 2; 2; 2; 2; 2; 2; 2; 1; 2; 2; 2; // 0x2*
            2; 2; 1; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; // 0x3*
            2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 1; 2; 2; 2; // 0x4*
            2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; // 0x5*
            2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 1; 2; 2; 2; // 0x6*
            2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; // 0x7*
            2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 1; 2; 2; 2; // 0x8*
            2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; // 0x9*
            2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 1; 2; 2; 2; // 0xA*
            2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; // 0xB*
            2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 1; 2; 2; 2; // 0xC*
            2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; // 0xD*
            2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 1; 2; 2; 2; // 0xE*
            2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; // 0xF*
        ]

    /// Addressing Mode : Zero page indexd
    let zpx (c:Config) =
        let a = (int16)c.WRAM.[(int)c.Register.X]
        let b = { c with Register = { c.Register with PC = c.Register.PC + 1s } }
        a,b
    /// Addressing Mode : Zero page indexed
    let zpy (c:Config) =
        let a = (int16)c.WRAM.[(int)c.Register.Y]
        let b = { c with Register = { c.Register with PC = c.Register.PC + 1s } }
        a,b
    /// Addressing Mode : Absolute indexed
    let aix (c:Config) =
        let a = (int16)c.WRAM.[(int)c.Register.Y]
        let b = { c with Register = { c.Register with PC = c.Register.PC + 2s } }
        a,b
    /// Addressing Mode : Absolute indexed
    let aiy (c:Config) = 1
    /// Addressing Mode : Indexed indirect
    let iix (c:Config) = 1
    /// Addressing Mode : Indirect indexed
    let iiy (c:Config) = 1
    /// Addressing Mode : Implisicit
    let imp (c:Config) = 1
    /// Addressing Mode : Accumlator
    let acm (c:Config) = 1
    /// Addressing Mode : Immediate
    let imm (c:Config) = 1
    /// Addressing Mode : Zero page
    let zp_ (c:Config) = 1
    /// Addressing Mode : Absolute
    let abs (c:Config) = 1
    /// Addressing Mode : Relative
    let rel (c:Config) = 1
    /// Addressing Mode : Indirect
    let ind (c:Config) = 1

    /// Read Opecode
    let readOpCode (c:Config) =
        let opc = (int)c.WRAM.[c.Register.PC]
        match 
        let opr = 

    let hello name =
        let a = AD.dx <| Common.getInitConfig()
        printfn "Hello %s" name

