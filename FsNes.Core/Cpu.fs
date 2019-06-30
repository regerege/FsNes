﻿namespace FsNes.Core

module Cpu =
    let public bitcount (v:byte) =
        let a = (v &&& 0xAAuy >>> 1) + (v &&& 0x55uy)
        let b = (a &&& 0xCCuy >>> 2) + (a &&& 0x33uy)
        let c = (b &&& 0xF0uy >>> 4) + (b &&& 0x0Fuy)
        c

    /// Increment and Decrement Active Pettern.
    let public (|IDC|IDX|IDY|) = function
        | Some v, None, None -> IDC v
        | None, Some v, None -> IDX v
        | None, None, Some v -> IDY v
        | _ -> failwith "存在しないパターンです。"

    /// JMP Instruction Active Pattern
    let public (|JMPABS|JMPIND|) op =
        match op with
        | 0x4C -> JMPABS
        | 0x6C -> JMPIND
        | _ -> failwith "存在しないパターンです。"

    /// Add
    let public calcStatusCA a b = if a > b then Masks.StatusFlag.C else 0uy
    /// Sub
    let public calcStatusCS a b = if a >= b then Masks.StatusFlag.C else 0uy
    /// Right Bit shift
    let public calcStatusCR v = v &&& Masks.StatusFlag.C
    /// Left Bit shift
    let public calcStatusCL v = v >>> 7 &&& Masks.StatusFlag.C
    let public calcStatusZ v = if v = 0uy then Masks.StatusFlag.Z else 0uy
    let public calcStatusV a b = (a &&& 0x7Fuy) ^^^ (b &&& 0x7Fuy) >>> 1
    let public calcStatusN v = v &&& Masks.StatusFlag.N

    /// Stack Function
    let public push acm (value:byte array) =
        let s = acm.Register.S
        let addr i = 0x0100 ||| int(s - i)
        let set i = acm.WRAM.[addr i] <- value.[value.Length - 1 - int i]
        Seq.init value.Length id
        |> Seq.iter (byte >> set)
        let s = int s - value.Length |> byte
        { acm with Register = { acm.Register with S = s } }
    /// Stack Function
    let public pop acm len =
        let s = acm.Register.S
        let get i = acm.WRAM.[(0x0100 ||| int(s + i))]
        let value =
            Seq.init len ((+)1)
            |> Seq.map (byte >> get)
            |> Seq.toArray
        let s = s + byte len
        { acm with Register = { acm.Register with S = s } },value

    /// Compare Main Function
    let public compare acm a =
        let b = acm.Value.[0]
        let value = a - b
        let c = calcStatusCS a b
        let z = calcStatusZ value
        let n = calcStatusN value
        { acm with Register = { acm.Register with P = { acm.Register.P with N = n; Z = z; C = c } }}

    /// Branch Main Function
    let public branch (digits, comp) acm =
        if acm.Register.P.[digits] = comp then
            let a = acm.Register.PC + 1us
            let b = a + (uint16)acm.Value.[0]
            let cycle = acm.Cycle + 1 + (int)((a ^^^ b) >>> 8)
            { acm with Cycle = cycle; Register = { acm.Register with PC = b } }
        else
            acm

    /// Increment and Decrement Main Function.
    let public incAdec op acm o =
        let a,dest =
            match o with
            | IDC v -> v,Destination.Memory
            | IDX v -> v,Destination.X
            | IDY v -> v,Destination.Y
        let value = op a 1uy
        let z = calcStatusZ value
        let n = calcStatusN value
        if dest = Destination.Memory then
            acm.WRAM.[0] <- 0xFFuy
        { acm with
            Value = [value]
            Destination = dest
            Register =
                { acm.Register with
                    P = { acm.Register.P with N = n; Z = z; }
                    X = if dest = Destination.X then value else acm.Register.X
                    Y = if dest = Destination.Y then value else acm.Register.Y
                }
        }
    
    /// Plus Instruction.
    let public _adc acm =
        let a = acm.Register.A
        let value = acm.Register.A + acm.Value.[0] + (acm.Register.P.C &&& 1uy)
        let c = calcStatusCA a value
        let v = calcStatusV a value
        { acm with
            Value = [value]
            Destination = Destination.Memory
            Register =
                { acm.Register with
                    P = { acm.Register.P with V = v; C = c; }
                }
        }

    /// Other Illegal Opcode.
    let public _anc acm =
        let value = acm.Register.A &&& acm.Value.[0]
        { acm with
            Value = [value]
            Destination = Destination.A
            Register = { acm.Register with A = value } }

    /// Logical Conjunction.
    let public _and acm =
        let value = acm.Register.A &&& acm.Value.[0]
        { acm with
            Value = [value]
            Destination = Destination.A
            Register = { acm.Register with A = value } }

    /// Unoffical Opcode.
    let public _ane acm =
        let value = (acm.Register.A ||| 0xEEuy) &&& acm.Register.X &&& acm.Value.[0]
        { acm with
            Value = [value]
            Destination = Destination.A
            Register = { acm.Register with A = value } }

    /// Unoffical Opcode.
    let public _arr acm =
        let a = acm.Register.A &&& acm.Value.[0]
        let value = (a >>> 1) ||| (acm.Register.P.C <<< 7)
        let c = calcStatusCR a
        { acm with
            Value = [value]
            Destination = Destination.A
            Register =
                { acm.Register with
                    A = value
                    P = { acm.Register.P with C = c } } }
        
    /// Left Rotate
    let public _asl acm =
        let v = acm.Value.[0]
        let value = v <<< 1
        let c = calcStatusCL v
        let a =
            if acm.Destination = Destination.A then
                value
            else acm.Register.A
        if acm.Destination = Destination.Memory then
            acm.WRAM.[acm.Address] <- value
        { acm with
            Value = [value]
            Register =
                { acm.Register with
                    A = a
                    P = { acm.Register.P with C = c } } }

    /// Other Illegal Opcode.
    let public _asr acm =
        let v = acm.Register.A &&& acm.Value.[0]
        let value = v >>> 1
        let c = calcStatusCR v
        { acm with
            Value = [value]
            Destination = Destination.A
            Register =
                { acm.Register with
                    A = value
                    P = { acm.Register.P with C = c } } }

    /// Branch : C = 0
    let public _bcc = branch (0, 0uy)
    /// Branch : C = 1
    let public _bcs = branch (0, 1uy)
    /// Branch : Z = 0
    let public _bne = branch (1, 0uy)
    /// Branch : Z = 1
    let public _beq = branch (1, 1uy)
    /// Branch : V = 0
    let public _bvc = branch (6, 0uy)
    /// Branch : V = 1
    let public _bvs = branch (6, 1uy)
    /// Branch : N = 0
    let public _bpl = branch (7, 0uy)
    /// Branch : N = 1
    let public _bmi = branch (7, 1uy)

    /// Clear Flag : C <- 0
    let public _clc acm : CpuAccumulator = { acm with Register = { acm.Register with P = { acm.Register.P with C = 0uy } } }
    /// Clear Flag : I <- 0
    let public _cli acm : CpuAccumulator = { acm with Register = { acm.Register with P = { acm.Register.P with I = 0uy } } }
    /// Clear Flag : V <- 0
    let public _clv acm : CpuAccumulator = { acm with Register = { acm.Register with P = { acm.Register.P with V = 0uy } } }
    /// Clear Flag : D <- 0
    let public _cld acm : CpuAccumulator = { acm with Register = { acm.Register with P = { acm.Register.P with D = 0uy } } }
        
    /// Set Flag : C <- 1
    let public _stc acm : CpuAccumulator = { acm with Register = { acm.Register with P = { acm.Register.P with C = Masks.StatusFlag.C } } }
    /// Set Flag : I <- 1
    let public _sti acm : CpuAccumulator = { acm with Register = { acm.Register with P = { acm.Register.P with I = Masks.StatusFlag.I } } }
    /// Set Flag : D <- 1
    let public _std acm : CpuAccumulator = { acm with Register = { acm.Register with P = { acm.Register.P with D = Masks.StatusFlag.D } } }

    /// Comparison operation. A & {mem}
    let public _bit acm =
        let a = acm.Register.A
        let b = acm.Value.[0]
        let value = a &&& b
        let z = calcStatusZ value
        let v = value &&& Masks.StatusFlag.V
        let n = value &&& Masks.StatusFlag.N
        { acm with
            Register =
                { acm.Register with
                    P = { acm.Register.P with
                            Z = z
                            V = v
                            N = n } } }

    /// Comparison operation. A - {mem}
    let public _cmp acm = compare acm acm.Register.A
    /// Comparison operation. X - {mem}
    let public _cpx acm = compare acm acm.Register.X
    /// Comparison operation. Y - {mem}
    let public _cpy acm = compare acm acm.Register.Y

    /// Other Illegal Opcode.
    let public _dcp acm =
        let a = acm.Register.A
        let b = acm.Value.[0] - 1uy
        let value = a - b
        let c = calcStatusCS a b
        let z = calcStatusZ value
        let n = calcStatusN value
        acm.WRAM.[acm.Address] <- value
        { acm with
            Value = [value]
            Destination = Destination.Memory
            Register =
                { acm.Register with
                    P = { acm.Register.P with
                            N = n
                            Z = z
                            C = c } } }

    /// Xor A, Memory
    let public _eor acm =
        let a = acm.Register.A
        let b = acm.Value.[0]
        let value = a ^^^ b
        let z = calcStatusZ value
        let n = calcStatusN value
        { acm with
            Value = [value]
            Destination = Destination.A
            Register =
                { acm.Register with
                    A = value
                    P = { acm.Register.P with
                            N = n
                            Z = z} } }

    /// Decrement Memory
    let public _dec acm = incAdec (-) acm (Some acm.Value.[0], None, None)
    /// Decrement Index X
    let public _dex acm = incAdec (-) acm (None, Some acm.Register.X, None)
    /// Decrement Index Y
    let public _dey acm = incAdec (-) acm (None, None, Some acm.Register.Y)
    /// Increment Memory
    let public _inc acm = incAdec (+) acm (Some acm.Value.[0], None, None)
    /// Increment Index X
    let public _inx acm = incAdec (+) acm (None, Some acm.Register.X, None)
    /// Increment Index Y
    let public _iny acm = incAdec (+) acm (None, None, Some acm.Register.Y)

    /// Other Illegal Opcode.
    let public _isb acm =
        let a = acm.Value.[0]
        let b = 1uy
        let value = a - b
        let c = calcStatusCS a value
        let z = calcStatusZ value
        let v = calcStatusV a value
        let n = calcStatusN value
        acm.WRAM.[acm.Address] <- value
        { acm with
            Value = [value]
            Destination = Destination.Memory
            Register =
                { acm.Register with
                    P = { acm.Register.P with
                            N = n
                            V = v
                            Z = z
                            C = c } } }

    let public _jmp acm =
        let pc =
            match acm.Opcode with
            | JMPABS -> (uint16)acm.Address
            | JMPIND ->
                let addrL = acm.Address
                let addrH = addrL &&& 0xFF00 ||| (addrL + 1) &&& 0x00FF
                (uint16)acm.WRAM.[addrL] ||| ((uint16)acm.WRAM.[addrH] <<< 8)
        { acm with Register = { acm.Register with PC = pc } }

    let public _jsr acm =
        let value = acm.Register.PC + 2us
        let addrL = (byte)value
        let addrH = (byte)(value >>> 8)
        let acm2 = push acm [| addrH; addrL; |]
        let pc = acm.Address - 1 |> uint16
        { acm2 with Register = { acm2.Register with PC = pc } }

    /// CPU Cycle Count
    let public Cycles =
        [
         // 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
            7; 6; 0; 8; 3; 3; 5; 5; 3; 2; 2; 2; 4; 4; 6; 6; // 0x0*
            2; 5; 0; 8; 4; 4; 6; 6; 2; 4; 2; 7; 4; 4; 6; 7; // 0x1*
            6; 6; 0; 8; 3; 3; 5; 5; 4; 2; 2; 2; 4; 4; 6; 6; // 0x2*
            2; 5; 0; 8; 4; 4; 6; 6; 2; 4; 2; 7; 4; 4; 7; 7; // 0x3*
            6; 6; 0; 8; 3; 3; 5; 5; 3; 2; 2; 2; 3; 4; 6; 6; // 0x4*
            2; 5; 0; 8; 4; 4; 6; 6; 2; 4; 2; 7; 4; 4; 7; 7; // 0x5*
            6; 6; 0; 8; 3; 3; 5; 5; 4; 2; 2; 2; 5; 4; 6; 6; // 0x6*
            2; 5; 0; 8; 4; 4; 6; 6; 2; 4; 2; 7; 4; 4; 7; 7; // 0x7*
            2; 6; 2; 6; 3; 3; 3; 3; 2; 2; 2; 2; 4; 4; 4; 4; // 0x8*
            2; 5; 0; 6; 4; 4; 4; 4; 2; 4; 2; 5; 5; 4; 5; 5; // 0x9*
            2; 6; 2; 6; 3; 3; 3; 3; 2; 2; 2; 2; 4; 4; 4; 4; // 0xA*
            2; 5; 0; 5; 4; 4; 4; 4; 2; 4; 2; 4; 4; 4; 4; 4; // 0xB*
            2; 6; 2; 8; 3; 3; 5; 5; 2; 2; 2; 2; 4; 4; 6; 6; // 0xC*
            2; 5; 0; 8; 4; 4; 6; 6; 2; 4; 2; 7; 4; 4; 6; 7; // 0xD*
            2; 6; 2; 8; 3; 3; 5; 5; 2; 2; 2; 2; 4; 4; 6; 6; // 0xE*
            2; 5; 0; 8; 4; 4; 6; 6; 2; 4; 2; 7; 4; 4; 7; 7; // 0xF*
        ]

    /// Oprand + Opcode Summary Bytes Count
    let public Bytes =
        [
         // 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
            1; 2; 1; 2; 2; 2; 2; 2; 1; 2; 1; 2; 3; 3; 3; 3; // 0x0*
            2; 2; 1; 2; 2; 2; 2; 2; 1; 3; 1; 3; 3; 3; 3; 3; // 0x1*
            3; 2; 1; 2; 2; 2; 2; 2; 1; 2; 1; 2; 3; 3; 3; 3; // 0x2*
            2; 2; 1; 2; 2; 2; 2; 2; 1; 3; 1; 3; 3; 3; 3; 3; // 0x3*
            1; 2; 1; 2; 2; 2; 2; 2; 1; 2; 1; 2; 3; 3; 3; 3; // 0x4*
            2; 2; 1; 2; 2; 2; 2; 2; 1; 3; 1; 3; 3; 3; 3; 3; // 0x5*
            1; 2; 1; 2; 2; 2; 2; 2; 1; 2; 1; 2; 3; 3; 3; 3; // 0x6*
            2; 2; 1; 2; 2; 2; 2; 2; 1; 3; 1; 3; 3; 3; 3; 3; // 0x7*
            2; 2; 2; 2; 2; 2; 2; 2; 1; 2; 1; 2; 3; 3; 3; 3; // 0x8*
            2; 2; 1; 2; 2; 2; 2; 2; 1; 3; 1; 3; 3; 3; 3; 3; // 0x9*
            2; 2; 2; 2; 2; 2; 2; 2; 1; 2; 1; 2; 3; 3; 3; 3; // 0xA*
            2; 2; 1; 2; 2; 2; 2; 2; 1; 3; 1; 3; 3; 3; 3; 3; // 0xB*
            2; 2; 2; 2; 2; 2; 2; 2; 1; 2; 1; 2; 3; 3; 3; 3; // 0xC*
            2; 2; 1; 2; 2; 2; 2; 2; 1; 3; 1; 3; 3; 3; 3; 3; // 0xD*
            2; 2; 2; 2; 2; 2; 2; 2; 1; 2; 1; 2; 3; 3; 3; 3; // 0xE*
            2; 2; 1; 2; 2; 2; 2; 2; 1; 3; 1; 3; 3; 3; 3; 3; // 0xF*
        ]

    /// Flag Updates
    let public UpdateNZ =
        [
            //0      1      2      3      4      5      6      7      8      9      A      B      C      D      E      F
            false;  true; false;  true; false;  true;  true;  true; false;  true;  true;  true; false;  true;  true;  true; // 0x0*
            false;  true; false;  true; false;  true;  true;  true; false;  true; false;  true; false;  true;  true;  true; // 0x1*
            false;  true; false;  true; false;  true;  true;  true;  true;  true;  true;  true; false;  true;  true;  true; // 0x2*
            false;  true; false;  true; false;  true;  true;  true; false;  true; false;  true; false;  true;  true;  true; // 0x3*
             true;  true; false;  true; false;  true;  true;  true; false;  true;  true;  true; false;  true;  true;  true; // 0x4*
            false;  true; false;  true; false;  true;  true;  true; false;  true; false;  true; false;  true;  true;  true; // 0x5*
            false;  true; false;  true; false;  true;  true;  true; false;  true;  true;  true; false;  true;  true;  true; // 0x6*
            false;  true; false;  true; false;  true;  true;  true; false;  true; false;  true; false;  true;  true;  true; // 0x7*
            false; false; false; false; false; false; false; false;  true; false;  true; false; false; false; false; false; // 0x8*
            false; false; false; false; false; false; false; false;  true; false;  true; false; false; false; false; false; // 0x9*
             true;  true;  true;  true;  true;  true;  true;  true;  true;  true;  true;  true;  true;  true;  true;  true; // 0xA*
            false;  true; false;  true;  true;  true;  true;  true; false;  true;  true;  true;  true;  true;  true;  true; // 0xB*
             true;  true; false;  true;  true;  true;  true;  true;  true;  true;  true;  true;  true;  true;  true;  true; // 0xC*
            false;  true; false;  true; false;  true;  true;  true; false;  true; false;  true; false;  true;  true;  true; // 0xD*
             true;  true; false;  true;  true;  true;  true;  true;  true;  true; false;  true;  true;  true;  true;  true; // 0xE*
            false;  true; false;  true; false;  true;  true;  true; false;  true; false;  true; false;  true;  true;  true; // 0xF*
        ]

    /// Page Boundary Crossing
    let public calcCycle cycle address offset =
        let a = ((address &&& 0x00FF) + offset) >>> 8
        cycle + a

    /// Addressing Mode : NOP
    let public nopAM acm = acm
    /// Addressing Mode : Zero Page, X
    let public zpx acm =
        let address = (int)((byte)acm.Oprand.[0] + acm.Register.X)
        let value = [ acm.WRAM.[address] ]
        { acm with Address = address; Value = value; Destination = Destination.Memory; }
    /// Addressing Mode : Zero Page, Y
    let public zpy acm =
        let address = (int)((byte)acm.Oprand.[0] + acm.Register.Y)
        let value = [ acm.WRAM.[address] ]
        { acm with Address = address; Value = value; Destination = Destination.Memory; }
    /// Addressing Mode : Absolute, X
    let public aix acm =
        let old = acm.Oprand.ByteWord()
        let cycle = calcCycle acm.Cycle old ((int)acm.Register.X)
        let address = old + (int)acm.Register.X
        let value = [ acm.WRAM.[address] ]
        { acm with Address = address; Value = value; Cycle = cycle; Destination = Destination.Memory; }
    /// Addressing Mode : Absolute, Y
    let public aiy acm =
        let old = acm.Oprand.ByteWord()
        let cycle = calcCycle acm.Cycle old ((int)acm.Register.Y)
        let address = old + (int)acm.Register.Y
        let value = [ acm.WRAM.[address] ]
        { acm with Address = address; Value = value; Cycle = cycle; Destination = Destination.Memory; }
    /// Addressing Mode : (Indirect, X)
    let public iix acm =
        let address = (int)(acm.Oprand.[0] + acm.Register.X)
        let address2 = (int)acm.WRAM.[address]
        let value = [ acm.WRAM.[address2] ]
        { acm with Address = address2; Value = value; Destination = Destination.Memory; }
    /// Addressing Mode : (Indirect), Y
    let public iiy acm =
        let address = (int)acm.WRAM.[(int)acm.Oprand.[0]]
        let address2 = address + (int)acm.Register.Y
        let cycle = calcCycle acm.Cycle address ((int)acm.Register.Y)
        let value = [ acm.WRAM.[address2] ]
        { acm with Address = address2; Value = value; Cycle = cycle; Destination = Destination.Memory; }
    /// Addressing Mode : Implied
    let public imp acm = acm
    /// Addressing Mode : Accumlator
    let public acm acm = { acm with Value = [ acm.Register.A ]; Destination = Destination.A; }
    /// Addressing Mode : Immediate
    let public imm acm = { acm with Value = acm.Oprand; }
    /// Addressing Mode : Zero Page
    let public zp_ acm =
        let mem = acm.WRAM.[acm.Address]
        let value = [ mem ]
        { acm with Value = value; Destination = Destination.Memory; }
    /// Addressing Mode : Absolute
    let public abs acm =
        let mem = acm.WRAM.[acm.Address]
        let value = [ mem ]
        { acm with Value = value; Destination = Destination.Memory; }
    /// Addressing Mode : Relative
    /// * The cycle calculation when crossing page boundaries is performed by "Instruction".
    let public rel acm =
        let mem = acm.WRAM.[acm.Address]
        let value = [ mem ]
        { acm with Value = value; }
    /// Addressing Mode : Indirect   (JMP ($5597))
    let public ind acm = acm

    let public createConfig () : Config =
        {
            /// CPU Cycle Skip Count
            CpuSkip = 0
            /// PPU Cycle Skip Count
            PpuSkip = 0
            /// Register
            Register = { A = 0uy; X = 0uy; Y = 0uy; PC = 0us; S = 6uy; P = { C = 0uy; Z = 0uy; I = 0uy; D = 0uy; B = 0uy; V = 0uy; N = 0uy; } }
            /// CPU Memory
            WRAM = [| for i in 0..0xFFFF -> 0uy |]
            /// Video Memory
            VRAM = [| for i in 0..0xFFFF -> 0uy |]
            /// Hardware and Software Interrupt
            Interrupt = Interrupt.Empty
        }
        
    /// Read Oprand
    let public read (size:int) (pc:uint16) (wram:byte array) =
        if size = 0 then
            List.Empty, -1
        else
            let pc = (int)pc + 1
            let get pc = wram.[pc]
            let oprand =
                Seq.initInfinite ((+)pc)
                |> Seq.take size
                |> Seq.map get
                |> Seq.toList
            let address =
                seq {
                    yield! oprand
                    yield! [ for i in oprand -> 0uy ]
                }
                |> Seq.map (int)
                |> Seq.reduce ((|||))
            oprand, address

    /// Create CPU Accumulator
    let public createAccumulator opcode (c:Config) : CpuAccumulator =
        let size = Bytes.[opcode] - 1
        let oprand, address = read size c.Register.PC c.WRAM
        {
            Cycle = Cycles.[opcode]
            Opcode = opcode
            Oprand = oprand
            Address = address
            Value = List.empty
            Destination = Destination.None
            /// Register
            Register = c.Register
            /// CPU Memory
            WRAM = c.WRAM
        }

    let public updateNZ acm =
        if UpdateNZ.[acm.Opcode] then
            let value = byte acm.Value.[0]
            let n = value &&& (1uy <<< 7)                           // 負の数なら7bit目を 1
            let z = (if value = 0uy then 1uy else 0uy) <<< 1        // 値が 0 なら2bit目を 1
            { acm with Register = { acm.Register with P = { acm.Register.P with N = n; Z = z } } }
        else acm

    let public addressing acm =
        let opcode = acm.Opcode
        acm

    /// CPU の処理を1ステップ実行する。
    /// One Step Processing.
    let public step acm =
        //acm
        //|> ind
        //opcode
        //|> createAccumulator
        //|> readOpRand c     // Read Oprand
        //|> ind c            // Addressing Mode
        //|> clc c            // Calc opcode
        //|> update c         // Update Register Flags or Memory
        acm

    /// Convert the calculation result "CpuAccumulator" to "Config".
    /// 計算結果の "CpuAccumulator" から "Config" に変換する。
    let public convertConfig (c:Config) (acm:CpuAccumulator) =
        { c with Register = acm.Register; WRAM = acm.WRAM; CpuSkip = acm.Cycle }

    /// Cpu Process Running.
    let public run (c:Config) =
        match c.Interrupt with
        | Interrupt.NMI -> c
        | Interrupt.Reset -> c
        | Interrupt.IRQ
        | Interrupt.BRK ->
            c
        | _ ->
            let opcode = (int)c.WRAM.[(int)c.Register.PC]
            if opcode = 0 then
                { c with Interrupt = Interrupt.BRK }
            else
                let acm = createAccumulator opcode c
                step acm
                |> convertConfig c

