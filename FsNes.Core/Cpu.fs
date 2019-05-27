namespace FsNes.Core

module Cpu =
    /// Read Oprand
    let private readOpRand (c:Config) (acm:CpuAccumulator) : CpuAccumulator =
        let pc = (int)c.Register.PC + 1
        let f (index:int) = (int)c.WRAM.[pc + index] <<< (8 * index)
        let oprand =
            match acm.Size with
            | 2 | 3 ->
                seq [2..acm.Size]
                |> Seq.map ((-)2 >> f)
                |> Seq.reduce(|||)
                |> Some
            | _ -> None
        { acm with Oprand = oprand; Address = oprand }

    /// Addressing Mode : NOP
    let private nopAM (c:Config) (acm:CpuAccumulator) : CpuAccumulator =
        acm
    /// Addressing Mode : Zero Page, X
    let private zpx (c:Config) (acm:CpuAccumulator) : CpuAccumulator =
        let address = (int)((byte)acm.Oprand.Value + c.Register.X)
        { acm with Address = Some address; Memory = Some(c.WRAM.[address]) }
    /// Addressing Mode : Zero Page, Y
    let private zpy (c:Config) (acm:CpuAccumulator) : CpuAccumulator =
        let address = (int)((byte)acm.Oprand.Value + c.Register.Y)
        { acm with Address = Some address; Memory = Some(c.WRAM.[address]) }
    /// Addressing Mode : Absolute, X
    let private aix (c:Config) (acm:CpuAccumulator) : CpuAccumulator =
        let address = acm.Oprand.Value + (int)c.Register.X
        { acm with Address = Some address; Memory = Some(c.WRAM.[address]) }
    /// Addressing Mode : Absolute, Y
    let private aiy (c:Config) (acm:CpuAccumulator) : CpuAccumulator =
        let address = acm.Oprand.Value + (int)c.Register.Y
        { acm with Address = Some address; Memory = Some(c.WRAM.[address]) }
    /// Addressing Mode : (Indirect, X)
    let private iix (c:Config) (acm:CpuAccumulator) : CpuAccumulator =
        let address = (int)((byte)acm.Oprand.Value + c.Register.X)
        let address2 = (int)c.WRAM.[address]
        { acm with Address = Some address2; Memory = Some(c.WRAM.[address2]) }
    /// Addressing Mode : (Indirect), Y
    let private iiy (c:Config) (acm:CpuAccumulator) : CpuAccumulator =
        let address = c.WRAM.[acm.Oprand.Value]
        let address2 = (int)(address + c.Register.Y)
        { acm with Address = Some address2; Memory = Some(c.WRAM.[address2]) }
    /// Addressing Mode : Implied
    let private imp (c:Config) (acm:CpuAccumulator) : CpuAccumulator =
        acm
    /// Addressing Mode : Accumlator
    let private acm (c:Config) (acm:CpuAccumulator) : CpuAccumulator =
        acm
    /// Addressing Mode : Immediate
    let private imm (c:Config) (acm:CpuAccumulator) : CpuAccumulator =
        { acm with Memory = Some <| (byte)acm.Oprand.Value }
    /// Addressing Mode : Zero Page
    let private zp_ (c:Config) (acm:CpuAccumulator) : CpuAccumulator =
        { acm with Memory = Some <| c.WRAM.[acm.Oprand.Value] }
    /// Addressing Mode : Absolute
    let private abs (c:Config) (acm:CpuAccumulator) : CpuAccumulator =
        { acm with Memory = Some <| c.WRAM.[acm.Oprand.Value] }
    /// Addressing Mode : Relative
    let private rel (c:Config) (acm:CpuAccumulator) : CpuAccumulator =
        { acm with Memory = Some <| c.WRAM.[acm.Oprand.Value] }
    /// Addressing Mode : Indirect
    let private ind (c:Config) (acm:CpuAccumulator) : CpuAccumulator =
        let address = (int)c.WRAM.[acm.Oprand.Value]
        let value = c.WRAM.[address]
        { acm with Address = Some address; Result = Some value }

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

    //let private adc1 (c:Config) =
    //    // (Indirect, X)
    //    // ▽ フェッチ ▽  メモリの読み込みのみ
    //    let opcode = c.WRAM.[(int)c.Register.PC]
    //    let size = Bytes.[(int)opcode]  // 2 Byte (opcode ($44,X))
    //    let oprand = c.WRAM.[(int)c.Register.PC + 1]
    //    // アドレッシングモードの処理
    //    let address = (int)(oprand + c.Register.X) % 0x100
    //    let memory = c.WRAM.[address]
    //    // ADCの処理
    //    let ret = c.Register.A + memory + (c.Register.S &&& 1uy)
    //    { c with Register = { c.Register with A = ret } }
    //let private adc2 (c:Config) =
    //    // Zero Page
    //    // ▽ フェッチ ▽  メモリの読み込みのみ
    //    let opcode = c.WRAM.[(int)c.Register.PC]
    //    let size = Bytes.[(int)opcode]  // 2 Byte (opcode $44)
    //    let oprand = c.WRAM.[(int)c.Register.PC + 1]
    //    // アドレッシングモードの処理
    //    let address = (int)oprand
    //    let memory = c.WRAM.[address]
    //    // ADCの処理
    //    let ret = c.Register.A + memory + (c.Register.S &&& 1uy)
    //    { c with Register = { c.Register with A = ret } }
    //let private adc3 (c:Config) =
    //    // Immediate
    //    // ▽ フェッチ ▽  メモリの読み込みのみ
    //    let opcode = c.WRAM.[(int)c.Register.PC]
    //    let size = Bytes.[(int)opcode]  // 2 Byte (opcode #$44)
    //    let oprand = c.WRAM.[(int)c.Register.PC + 1]
    //    // アドレッシングモードの処理
    //    let memory = oprand
    //    // ADCの処理
    //    let ret = c.Register.A + memory + (c.Register.S &&& 1uy)
    //    { c with Register = { c.Register with A = ret } }
    //let private adc4 (c:Config) =
    //    // Absolute
    //    // ▽ フェッチ ▽  メモリの読み込みのみ
    //    let opcode = c.WRAM.[(int)c.Register.PC]
    //    let size = Bytes.[(int)opcode]  // 3 Byte (opcode #4400)
    //    let oprand1 = c.WRAM.[(int)c.Register.PC + 1]
    //    let oprand2 = c.WRAM.[(int)c.Register.PC + 2]
    //    let oprand = ((int)oprand2 <<< 8) ||| (int)oprand1
    //    // アドレッシングモードの処理
    //    let address = (int)oprand
    //    let memory = c.WRAM.[address]
    //    // ADCの処理
    //    let ret = c.Register.A + memory + (c.Register.S &&& 1uy)
    //    { c with Register = { c.Register with A = ret } }
    //let private adc5 (c:Config) =
    //    // (Indirect), Y
    //    // ▽ フェッチ ▽  メモリの読み込みのみ
    //    let opcode = c.WRAM.[(int)c.Register.PC]
    //    let size = Bytes.[(int)opcode]  // 2 Byte (opcode ($44),Y)
    //    let oprand = c.WRAM.[(int)c.Register.PC + 1]
    //    // アドレッシングモードの処理
    //    let address = (int)(c.WRAM.[(int)oprand] + c.Register.Y)
    //    let memory = c.WRAM.[address]
    //    // ADCの処理
    //    let ret = c.Register.A + memory + (c.Register.S &&& 1uy)
    //    { c with Register = { c.Register with A = ret } }
    //let private adc6 (c:Config) =
    //    // Zero Page, X
    //    // ▽ フェッチ ▽  メモリの読み込みのみ
    //    let opcode = c.WRAM.[(int)c.Register.PC]
    //    let size = Bytes.[(int)opcode]  // 2 Byte (opcode $44,X)
    //    let oprand = c.WRAM.[(int)c.Register.PC + 1]
    //    // アドレッシングモードの処理
    //    let address = (int)(oprand + c.Register.X) % 0x100
    //    let memory = c.WRAM.[address]
    //    // ADCの処理
    //    let ret = c.Register.A + memory + (c.Register.S &&& 1uy)
    //    { c with Register = { c.Register with A = ret } }
    //let private adc7 (c:Config) =
    //    // Absolute, Y
    //    // ▽ フェッチ ▽  メモリの読み込みのみ
    //    let opcode = c.WRAM.[(int)c.Register.PC]
    //    let size = Bytes.[(int)opcode]  // 3 Byte (opcode $4400,Y)
    //    let oprand1 = c.WRAM.[(int)c.Register.PC + 1]
    //    let oprand2 = c.WRAM.[(int)c.Register.PC + 2]
    //    let oprand = ((int)oprand2 <<< 8) ||| (int)oprand1
    //    // アドレッシングモードの処理
    //    let address = oprand + (int)c.Register.Y
    //    let memory = c.WRAM.[address]
    //    // ADCの処理
    //    let ret = c.Register.A + memory + (c.Register.S &&& 1uy)
    //    { c with Register = { c.Register with A = ret } }


    let private storeNop (c:Config) (acm:CpuAccumulator) : Config =
        c
    let private storeA (c:Config) (acm:CpuAccumulator) : Config =
        { c with Register = { c.Register with A = acm.Result.Value } }
    let private storeP (c:Config) (acm:CpuAccumulator) : Config =
        { c with Register = { c.Register with P = acm.Result.Value } }
    let private storeMem (c:Config) (acm:CpuAccumulator) : Config =
        c.WRAM.[acm.Address.Value] <- acm.Result.Value
        c

    let private adc (c:Config) (acm:CpuAccumulator) : CpuAccumulator =
        let ret = c.Register.A + acm.Memory.Value + (c.Register.S &&& 1uy)
        { acm with Result = Some ret }
    let private clc (c:Config) (acm:CpuAccumulator) : CpuAccumulator =
        let p = c.Register.P &&& 0b11111110uy
        { acm with Result = Some p }
    let private stx (c:Config) (acm:CpuAccumulator) : CpuAccumulator =
        { acm with Result = Some c.Register.X }

    let private asl (c:Config) (address:int option, oprand:byte option) =
        let ret = c.Register.A <<< 1
        address,Some ret

    /// CPU の処理を1ステップ実行する。
    /// One Step Processing.
    let step (c:Config) =
        let opcode = (int)c.WRAM.[(int)c.Register.PC]
        let acm : CpuAccumulator = {
                Opcode = opcode
                Size = Bytes.[opcode]
                Cycle = Cycles.[opcode]
                Oprand = None
                Address = None
                Memory = None
                Result = None
            }

        acm
        |> readOpRand c     // Read Oprand
        |> ind c            // Addressing Mode
        |> clc c            // Calc opcode
        |> storeP c         // Store

