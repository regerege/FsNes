namespace FsNes.Core

open FsNes.Core.Common

module Cpu =
    /// 未定義 実装後に削除予定
    let private err acm =
        failwith "未実装のアドレッシングモード"

    /// 未定義 実装後に削除予定
    let private _err acm =
        failwith "未実装の命令"

    /// Stop command.
    let private kill acm =
        raise <| new NesKillException()

    /// Branch main function.
    let private bitcount (v:byte) =
        let a = (v &&& 0xAAuy >>> 1) + (v &&& 0x55uy)
        let b = (a &&& 0xCCuy >>> 2) + (a &&& 0x33uy)
        let c = (b &&& 0xF0uy >>> 4) + (b &&& 0x0Fuy)
        c

    /// Increment and Decrement Active Pettern.
    let private (|IDC|IDX|IDY|) = function
        | Some v, None, None -> IDC v
        | None, Some v, None -> IDX v
        | None, None, Some v -> IDY v
        | _ -> failwith "存在しないパターンです。"

    /// JMP Instruction Active Pattern
    let private (|JMPABS|JMPIND|) op =
        match op with
        | 0x4C -> JMPABS
        | 0x6C -> JMPIND
        | _ -> failwith "存在しないパターンです。"

    /// Add
    let private calcStatusCA a b = if a > b then 1uy else 0uy
    /// Sub
    let private calcStatusCS a b = if a >= b then 1uy else 0uy
    /// Right Bit shift
    let private calcStatusCR v = v &&& 1uy
    /// Left Bit shift
    let private calcStatusCL v = v >>> 7 &&& 1uy
    let private calcStatusZ v = if v = 0uy then 1uy else 0uy
    let private calcStatusV a b c =
        if a <= 0x7Fuy && b >= 0x80uy && c = true then 1uy
        elif a >= 0x80uy && b <= 0x7Fuy && c = false then 1uy
        else 0uy
    let private calcStatusN v = v >>> 7 &&& 0b10000000uy

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
    let private compare acm a =
        let b = acm.Value.[0]
        let value = a - b
        let c = calcStatusCS a b
        let z = calcStatusZ value
        let n = calcStatusN value
        { acm with Register = { acm.Register with P = { acm.Register.P with N = n; Z = z; C = c } }}

    /// Branch Main Function
    let private branch (digits, comp) acm =
        if acm.Register.P.[digits] = comp then
            let a = acm.Register.PC + 1us
            let b = a + (uint16)acm.Value.[0]
            let cycle = acm.Cycle + 1 + (int)((a ^^^ b) >>> 8)
            { acm with Cycle = cycle; Register = { acm.Register with PC = b } }
        else
            acm

    /// Increment and Decrement Main Function.
    let private incAdec op acm =
        let value = op acm.Value.[0] 1uy
        let z = calcStatusZ value
        let n = calcStatusN value
        if acm.Destination.Is(Destination.Memory) then
            acm.WRAM.[0] <- 0xFFuy
        let x = if acm.Destination.Is(Destination.X) then value else acm.Register.X
        let y = if acm.Destination.Is(Destination.Y) then value else acm.Register.Y
        { acm with
            Value = [value]
            Register =
                { acm.Register with
                    P = { acm.Register.P with N = n; Z = z; }
                    X = x
                    Y = y
                }
        }
    
    /// Plus Instruction.
    let private _adc acm =
        let a = acm.Register.A
        let value = a + acm.Value.[0] + acm.Register.P.C
        let c = calcStatusCA a value
        let v = calcStatusV a value true
        { acm with
            Value = [value]
            Register =
                { acm.Register with
                    P = { acm.Register.P with V = v; C = c; }
                }
        }

    /// Other Illegal Opcode.
    let private _anc acm =
        let value = acm.Register.A &&& acm.Value.[0]
        { acm with
            Value = [value]
            Register = { acm.Register with A = value } }

    /// Logical Conjunction.
    let private _and acm =
        let value = acm.Register.A &&& acm.Value.[0]
        { acm with
            Value = [value]
            Register = { acm.Register with A = value } }

    /// Unoffical Opcode.
    let private _ane acm =
        let value = (acm.Register.A ||| 0xEEuy) &&& acm.Register.X &&& acm.Value.[0]
        { acm with
            Value = [value]
            Register = { acm.Register with A = value } }

    /// Unoffical Opcode.
    let private _arr acm =
        let a = acm.Register.A &&& acm.Value.[0]
        let value = (a >>> 1) ||| (acm.Register.P.C <<< 7)
        let c = calcStatusCR a
        { acm with
            Value = [value]
            Register =
                { acm.Register with
                    A = value
                    P = { acm.Register.P with C = c } } }
        
    /// Left Rotate
    let private _asl acm =
        let v = acm.Value.[0]
        let value = v <<< 1
        let c = calcStatusCL v
        let a =
            if acm.Destination.Is(Destination.A) then
                value
            else acm.Register.A
        if acm.Destination.Is(Destination.Memory) then
            acm.WRAM.[acm.Address] <- value
        { acm with
            Value = [value]
            Register =
                { acm.Register with
                    A = a
                    P = { acm.Register.P with C = c } } }

    /// Other Illegal Opcode.
    let private _asr acm =
        let v = acm.Register.A &&& acm.Value.[0]
        let value = v >>> 1
        let c = calcStatusCR v
        { acm with
            Value = [value]
            Register =
                { acm.Register with
                    A = value
                    P = { acm.Register.P with C = c } } }

    /// Branch : C = 0
    let private _bcc = branch (0, 0uy)
    /// Branch : C = 1
    let private _bcs = branch (0, 1uy)
    /// Branch : Z = 0
    let private _bne = branch (1, 0uy)
    /// Branch : Z = 1
    let private _beq = branch (1, 1uy)
    /// Branch : V = 0
    let private _bvc = branch (6, 0uy)
    /// Branch : V = 1
    let private _bvs = branch (6, 1uy)
    /// Branch : N = 0
    let private _bpl = branch (7, 0uy)
    /// Branch : N = 1
    let private _bmi = branch (7, 1uy)

    /// Clear Flag : C <- 0
    let private _clc acm : CpuAccumulator = { acm with Register = { acm.Register with P = { acm.Register.P with C = 0uy } } }
    /// Clear Flag : I <- 0
    let private _cli acm : CpuAccumulator = { acm with Register = { acm.Register with P = { acm.Register.P with I = 0uy } } }
    /// Clear Flag : V <- 0
    let private _clv acm : CpuAccumulator = { acm with Register = { acm.Register with P = { acm.Register.P with V = 0uy } } }
    /// Clear Flag : D <- 0
    let private _cld acm : CpuAccumulator = { acm with Register = { acm.Register with P = { acm.Register.P with D = 0uy } } }
        
    /// Set Flag : C <- 1
    let private _stc acm : CpuAccumulator = { acm with Register = { acm.Register with P = { acm.Register.P with C = 1uy } } }
    /// Set Flag : I <- 1
    let private _sti acm : CpuAccumulator = { acm with Register = { acm.Register with P = { acm.Register.P with I = 1uy } } }
    /// Set Flag : D <- 1
    let private _std acm : CpuAccumulator = { acm with Register = { acm.Register with P = { acm.Register.P with D = 1uy } } }

    /// Comparison operation. A & {mem}
    let private _bit acm =
        let a = acm.Register.A
        let b = acm.Value.[0]
        let value = a &&& b
        let z = calcStatusZ value
        let v = value >>> 6 &&& 1uy
        let n = calcStatusN value
        { acm with
            Register =
                { acm.Register with
                    P = { acm.Register.P with
                            Z = z
                            V = v
                            N = n } } }

    /// Comparison operation. A - {mem}
    let private _cmp acm = compare acm acm.Register.A
    /// Comparison operation. X - {mem}
    let private _cpx acm = compare acm acm.Register.X
    /// Comparison operation. Y - {mem}
    let private _cpy acm = compare acm acm.Register.Y

    /// Other Illegal Opcode.
    let private _dcp acm =
        let a = acm.Register.A
        let b = acm.Value.[0] - 1uy
        let value = a - b
        let c = calcStatusCS a b
        let z = calcStatusZ value
        let n = calcStatusN value
        acm.WRAM.[acm.Address] <- value
        { acm with
            Value = [value]
            Register =
                { acm.Register with
                    P = { acm.Register.P with
                            N = n
                            Z = z
                            C = c } } }

    /// Xor A, Memory
    let private _eor acm =
        let a = acm.Register.A
        let b = acm.Value.[0]
        let value = a ^^^ b
        let z = calcStatusZ value
        let n = calcStatusN value
        { acm with
            Value = [value]
            Register =
                { acm.Register with
                    A = value
                    P = { acm.Register.P with
                            N = n
                            Z = z} } }

    /// Decrement Memory
    let private _dec acm = incAdec (-) acm
    /// Decrement Index X
    let private _dex acm = incAdec (-) acm
    /// Decrement Index Y
    let private _dey acm = incAdec (-) acm
    /// Increment Memory
    let private _inc acm = incAdec (+) acm
    /// Increment Index X
    let private _inx acm = incAdec (+) acm
    /// Increment Index Y
    let private _iny acm = incAdec (+) acm

    /// Other Illegal Opcode.
    let private _isb acm =
        let a = acm.Value.[0]
        let b = 1uy
        let value = a - b
        let c = calcStatusCS a value
        let z = calcStatusZ value
        let v = calcStatusV a value false
        let n = calcStatusN value
        acm.WRAM.[acm.Address] <- value
        { acm with
            Value = [value]
            Register =
                { acm.Register with
                    P = { acm.Register.P with
                            N = n
                            V = v
                            Z = z
                            C = c } } }

    let private _jmp acm =
        let pc =
            match acm.Opcode with
            | JMPABS -> (uint16)acm.Address
            | JMPIND ->
                let addrL = acm.Address
                let addrH = addrL &&& 0xFF00 ||| (addrL + 1) &&& 0x00FF
                (uint16)acm.WRAM.[addrL] ||| ((uint16)acm.WRAM.[addrH] <<< 8)
        { acm with Register = { acm.Register with PC = pc } }

    let private _jsr acm =
        let value = acm.Register.PC + 2us
        let addrL = (byte)value
        let addrH = (byte)(value >>> 8)
        let acm2 = push acm [| addrH; addrL; |]
        let pc = acm.Address - 1 |> uint16
        { acm2 with Register = { acm2.Register with PC = pc } }

////////////////////////////////////////////////////////////////////////////////////////////////////

    /// Page Boundary Crossing
    let private calcCycle cycle address offset =
        let a = ((address &&& 0x00FF) + offset) >>> 8
        cycle + a

    /// Addressing Mode : NOP
    let private nopAM acm = acm
    /// Addressing Mode : Zero Page, X  ||  PEEK(({addr} + X) % 256)
    let private zpx acm =
        let address = (int)(acm.Oprand.[0] + acm.Register.X)
        let value = [ acm.WRAM.[address] ]
        { acm with Address = address; Value = value; }
    /// Addressing Mode : Zero Page, Y  ||  PEEK(({addr} + Y) % 256)
    let private zpy acm =
        let address = (int)(acm.Oprand.[0] + acm.Register.Y)
        let value = [ acm.WRAM.[address] ]
        { acm with Address = address; Value = value; }
    /// Addressing Mode : Absolute, X   ||  PEEK({addr} + X)
    let private aix acm =
        let address = acm.Address + (int)acm.Register.X
        let cycle = calcCycle acm.Cycle acm.Address ((int)acm.Register.X)
        let value = [ acm.WRAM.[address] ]
        { acm with Address = address; Value = value; Cycle = cycle; }
    /// Addressing Mode : Absolute, Y   ||  PEEK({addr} + Y)
    let private aiy acm =
        let address = acm.Address + (int)acm.Register.Y
        let cycle = calcCycle acm.Cycle acm.Address ((int)acm.Register.Y)
        let value = [ acm.WRAM.[address] ]
        { acm with Address = address; Value = value; Cycle = cycle; }
    /// Addressing Mode : (Indirect, X) ||  PEEK(PEEK(({addr} + X) % 256) + PEEK(({addr} + X + 1) % 256) * 256)
    let private iix acm =
        let f a = acm.WRAM.[(int)a]
        let address =
            [
                acm.Oprand.[0] + acm.Register.X         // 下位アドレス
                acm.Oprand.[0] + acm.Register.X + 1uy   // 上位アドレス
            ]
            |> List.map f
            |> Common.getByteWord
        let value = [ acm.WRAM.[address] ]
        { acm with Address = address; Value = value; }
    /// Addressing Mode : (Indirect), Y ||  PEEK(PEEK({addr}) + PEEK(({addr} + 1) % 256) * 256 + Y)
    let private iiy acm =
        let f a = acm.WRAM.[(int)a]
        let address =
            [
                acm.Oprand.[0]          // 下位アドレス
                acm.Oprand.[0] + 1uy    // 上位アドレス
            ]
            |> List.map f
            |> Common.getByteWord
        let address2 = address + (int)acm.Register.Y
        let value = [ acm.WRAM.[address2] ]
        let cycle = calcCycle acm.Cycle address ((int)acm.Register.Y)
        { acm with Address = address2; Value = value; Cycle = cycle; }
    /// Addressing Mode : Implied
    let private imp acm = acm
    /// Addressing Mode : Accumlator
    let private acm acm = { acm with Value = [ acm.Register.A ]; }
    /// Addressing Mode : Immediate
    let private imm acm = { acm with Value = acm.Oprand; }
    /// Addressing Mode : Zero Page
    let private zp_ acm =
        let mem = acm.WRAM.[acm.Address]
        let value = [ mem ]
        { acm with Value = value; }
    /// Addressing Mode : Absolute
    let private abs acm =
        let mem = acm.WRAM.[acm.Address]
        let value = [ mem ]
        { acm with Value = value; }
    /// Addressing Mode : Relative
    /// * The cycle calculation when crossing page boundaries is performed by "Instruction".
    let private rel acm =
        let mem = acm.WRAM.[acm.Address]
        let value = [ mem ]
        { acm with Value = value; }
    /// Addressing Mode : Indirect   (JMP ($5597))
    let private ind acm = acm

////////////////////////////////////////////////////////////////////////////////////////////////////

    /// CPU Cycle Count
    let private Cycles =
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
    let private Bytes =
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

    /// Instructions.
    let private Instrunctions =
        [
            _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err
            _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err
            _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err
            _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err
            _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err
            _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err
            _err; _adc; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err
            _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err
            _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err
            _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err
            _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err
            _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err
            _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err
            _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err
            _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err
            _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err; _err
        ]

    /// Calculation result destination
    let private Destinations : Destination list =
        [
            //    0            1            2            3            4            5            6            7            8            9            A            B            C            D            E            F
            enum 0x0000; enum 0x0104; enum 0x0000; enum 0x0186; enum 0x0000; enum 0x0104; enum 0x0182; enum 0x0186; enum 0x0000; enum 0x0104; enum 0x0184; enum 0x0184; enum 0x0000; enum 0x0104; enum 0x0182; enum 0x0186;     // 0x0*
            enum 0x0020; enum 0x0104; enum 0x0000; enum 0x0186; enum 0x0000; enum 0x0104; enum 0x0182; enum 0x0186; enum 0x0000; enum 0x0104; enum 0x0000; enum 0x0186; enum 0x0000; enum 0x0104; enum 0x0182; enum 0x0186;     // 0x1*
            enum 0x0020; enum 0x0104; enum 0x0000; enum 0x0186; enum 0x2100; enum 0x0104; enum 0x0182; enum 0x0186; enum 0x3780; enum 0x0104; enum 0x0184; enum 0x0184; enum 0x2100; enum 0x0104; enum 0x0182; enum 0x0186;     // 0x2*
            enum 0x0020; enum 0x0104; enum 0x0000; enum 0x0186; enum 0x0000; enum 0x0104; enum 0x0182; enum 0x0186; enum 0x0000; enum 0x0104; enum 0x0000; enum 0x0186; enum 0x0000; enum 0x0104; enum 0x0182; enum 0x0186;     // 0x3*
            enum 0x37A2; enum 0x0104; enum 0x0000; enum 0x0186; enum 0x0000; enum 0x0104; enum 0x0182; enum 0x0186; enum 0x0000; enum 0x0104; enum 0x0184; enum 0x0184; enum 0x0020; enum 0x0104; enum 0x0182; enum 0x0186;     // 0x4*
            enum 0x0020; enum 0x0104; enum 0x0000; enum 0x0186; enum 0x0000; enum 0x0104; enum 0x0182; enum 0x0186; enum 0x0000; enum 0x0104; enum 0x0000; enum 0x0186; enum 0x0000; enum 0x0104; enum 0x0182; enum 0x0186;     // 0x5*
            enum 0x0022; enum 0x3184; enum 0x0000; enum 0x2186; enum 0x0000; enum 0x2184; enum 0x0182; enum 0x2186; enum 0x0004; enum 0x2184; enum 0x0184; enum 0x2180; enum 0x0020; enum 0x2184; enum 0x0182; enum 0x2186;     // 0x6*
            enum 0x0020; enum 0x2184; enum 0x0000; enum 0x2186; enum 0x0000; enum 0x2184; enum 0x0182; enum 0x2186; enum 0x0000; enum 0x2184; enum 0x0000; enum 0x2186; enum 0x0000; enum 0x2184; enum 0x0182; enum 0x2186;     // 0x7*
            enum 0x0000; enum 0x0002; enum 0x0000; enum 0x0002; enum 0x0002; enum 0x0002; enum 0x0002; enum 0x0002; enum 0x0110; enum 0x0000; enum 0x0104; enum 0x0000; enum 0x0002; enum 0x0002; enum 0x0002; enum 0x0002;     // 0x8*
            enum 0x0020; enum 0x0002; enum 0x0000; enum 0x0002; enum 0x0002; enum 0x0002; enum 0x0002; enum 0x0002; enum 0x0104; enum 0x0002; enum 0x0140; enum 0x0042; enum 0x0002; enum 0x0002; enum 0x0002; enum 0x0002;     // 0x9*
            enum 0x0110; enum 0x0104; enum 0x0108; enum 0x010C; enum 0x0110; enum 0x0104; enum 0x0108; enum 0x010C; enum 0x0110; enum 0x0104; enum 0x0108; enum 0x010C; enum 0x0110; enum 0x0104; enum 0x0108; enum 0x010C;     // 0xA*
            enum 0x0020; enum 0x0104; enum 0x0000; enum 0x010C; enum 0x0110; enum 0x0104; enum 0x0108; enum 0x010C; enum 0x0000; enum 0x0104; enum 0x0148; enum 0x014C; enum 0x0110; enum 0x0104; enum 0x0108; enum 0x010C;     // 0xB*
            enum 0x0180; enum 0x0180; enum 0x0000; enum 0x0182; enum 0x0180; enum 0x0180; enum 0x0102; enum 0x0182; enum 0x0110; enum 0x0180; enum 0x0108; enum 0x0188; enum 0x0180; enum 0x0180; enum 0x0102; enum 0x0182;     // 0xC*
            enum 0x0020; enum 0x0180; enum 0x0000; enum 0x0182; enum 0x0000; enum 0x0180; enum 0x0102; enum 0x0182; enum 0x0000; enum 0x0180; enum 0x0000; enum 0x0182; enum 0x0000; enum 0x0180; enum 0x0102; enum 0x0182;     // 0xD*
            enum 0x0180; enum 0x2184; enum 0x0000; enum 0x2182; enum 0x0180; enum 0x2184; enum 0x0102; enum 0x2182; enum 0x0108; enum 0x2184; enum 0x0000; enum 0x2184; enum 0x0180; enum 0x2184; enum 0x0102; enum 0x2182;     // 0xE*
            enum 0x0020; enum 0x2184; enum 0x0000; enum 0x2182; enum 0x0000; enum 0x2184; enum 0x0102; enum 0x2182; enum 0x0000; enum 0x2184; enum 0x0000; enum 0x2182; enum 0x0000; enum 0x2184; enum 0x0102; enum 0x2182;     // 0xF*
        ]

    /// Addressing Mode List
    let AddressingModes =
        [
            err; err; err; err; err; err; err; err; err; err; err; err; err; err; err; err;
            err; err; err; err; err; err; err; err; err; err; err; err; err; err; err; err;
            err; err; err; err; err; err; err; err; err; err; err; err; err; err; err; err;
            err; err; err; err; err; err; err; err; err; err; err; err; err; err; err; err;
            err; err; err; err; err; err; err; err; err; err; err; err; err; err; err; err;
            err; err; err; err; err; err; err; err; err; err; err; err; err; err; err; err;
            err; iix; err; err; err; err; err; err; err; err; err; err; err; err; err; err;
            err; err; err; err; err; err; err; err; err; err; err; err; err; err; err; err;
            err; err; err; err; err; err; err; err; err; err; err; err; err; err; err; err;
            err; err; err; err; err; err; err; err; err; err; err; err; err; err; err; err;
            err; err; err; err; err; err; err; err; err; err; err; err; err; err; err; err;
            err; err; err; err; err; err; err; err; err; err; err; err; err; err; err; err;
            err; err; err; err; err; err; err; err; err; err; err; err; err; err; err; err;
            err; err; err; err; err; err; err; err; err; err; err; err; err; err; err; err;
            err; err; err; err; err; err; err; err; err; err; err; err; err; err; err; err;
            err; err; err; err; err; err; err; err; err; err; err; err; err; err; err; err;
        ]

    /// Flag Updates
    let private UpdateNZ =
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

////////////////////////////////////////////////////////////////////////////////////////////////////

    /// Read Oprand
    let private read (size:int) (pc:uint16) (wram:byte array) =
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
            oprand, oprand.ByteWord()

    /// Convert from "Config" to "CpuAccumulator".
    /// 「Config」から「CpuAccumulator」に変換する。
    let public createAccumulator (c:Config) : CpuAccumulator =
        let opcode = (int)c.WRAM.[(int)c.Register.PC]
        let size = Bytes.[opcode]
        let oprand, address = read (size-1) c.Register.PC c.WRAM
        {
            Size = (uint16)size
            Cycle = Cycles.[opcode]
            Opcode = opcode
            Oprand = oprand
            Address = address
            Value = List.empty
            Destination = Destinations.[opcode]
            /// Register
            Register = c.Register
            /// CPU Memory
            WRAM = c.WRAM
        }

    /// Read memory from addressing mode.
    /// アドレッシングモードからメモリを読み込む。
    let private addressing acm = AddressingModes.[acm.Opcode] acm

    /// Calculation the instruction.
    let private instruction acm = Instrunctions.[acm.Opcode] acm

    /// Update N and Z Status Flags.
    let private updateNZ acm =
        if UpdateNZ.[acm.Opcode] then
            let value = byte acm.Value.[0]
            let n = value &&& (1uy <<< 7)                           // 負の数なら7bit目を 1
            let z = (if value = 0uy then 1uy else 0uy) <<< 1        // 値が 0 なら2bit目を 1
            { acm with Register = { acm.Register with P = { acm.Register.P with N = n; Z = z } } }
        else acm

    /// Update the calculation result to the register.
    let private update (acm:CpuAccumulator) =
        let a =
            if acm.Destination.Is(Destination.A) then
                acm.Value.[0]
            else
                acm.Register.A
        { acm with
            Register =
                { acm.Register with
                    // Calc result or Unchanged value.
                    A = a
                    // Go to next Instruction.
                    PC = acm.Register.PC + acm.Size
                }
        }

    /// Convert from "CpuAccumulator" to "Config".
    /// 「CpuAccumulator」から「Config」に変換する。
    let private convertConfig (config:Config) (acm:CpuAccumulator) =
        { config with
            CpuSkip = acm.Cycle
            Register = acm.Register
            WRAM = acm.WRAM
            //Interrupt = acm.Interrupt
        }

////////////////////////////////////////////////////////////////////////////////////////////////////

    /// CPU の処理を1ステップ実行する。
    /// One Step Processing.
    let public step (config:Config) =
        createAccumulator config    // 1. Convert from "Config" to "CpuAccumulator".
        |> addressing               // 2. Read memory from addressing mode.
        |> instruction              // 3. Calculation the instruction.
        |> updateNZ                 // 4. Update N and Z Status Flags.
        |> update                   // 5. Update the calculation result to the register.
        |> convertConfig config     // 6. Convert from "CpuAccumulator" to "Config"

////////////////////////////////////////////////////////////////////////////////////////////////////

