namespace FsNes.Core

module Cpu =
    /// Read Oprand
    let private readOpRand (c:Config) (size:int) =
        let pc = (int)c.Register.PC + 1
        let f (index:int) = (int)c.WRAM.[pc + index] <<< (8 * index)
        match size with
        | 2 | 3 ->
            seq [2..size]
            |> Seq.map ((-)2 >> f)
            |> Seq.reduce(|||)
            |> Some
        | _ -> None

    let private nopA (c:Config) (oprand:int option) =
        None
    /// Addressing Mode : Zero Page, X
    let private zpx (c:Config) (oprand:int option) =
        let address = oprand.Value + (int)c.Register.X
        Some c.WRAM.[address]
    /// Addressing Mode : Zero Page, Y
    let private zpy (c:Config) (oprand:int option) =
        let address = oprand.Value + (int)c.Register.Y
        Some c.WRAM.[address]
    /// Addressing Mode : Absolute, X
    let private aix (c:Config) (oprand:int option) =
        let address = oprand.Value + (int)c.Register.X
        Some c.WRAM.[address]
    /// Addressing Mode : Absolute, Y
    let private aiy (c:Config) (oprand:int option) =
        let address = oprand.Value + (int)c.Register.Y
        Some c.WRAM.[address]
    /// Addressing Mode : (Indirect, X)
    let private iix (c:Config) (oprand:int option) =
        let address = oprand.Value + (int)c.Register.X
        let address2 = (int)c.WRAM.[address]
        Some c.WRAM.[address2]
    /// Addressing Mode : (Indirect), Y
    let private iiy (c:Config) (oprand:int option) =
        let address = c.WRAM.[oprand.Value]
        let address2 = (int)(address + c.Register.Y)
        Some c.WRAM.[address2]
    /// Addressing Mode : Implied
    let private imp (c:Config) =
        None
    /// Addressing Mode : Accumlator
    let private acm (c:Config) =
        None
    /// Addressing Mode : Immediate
    let private imm (c:Config) (oprand:byte) =
        Some oprand
    /// Addressing Mode : Zero Page
    let private zp_ (c:Config) (oprand:byte) =
        Some c.WRAM.[(int)oprand]
    /// Addressing Mode : Absolute
    let private abs (c:Config) (oprand:int) =
        Some c.WRAM.[(int)oprand]
    /// Addressing Mode : Relative
    let private rel (c:Config) (oprand:int) =
        Some oprand
    /// Addressing Mode : Indirect
    let private ind (c:Config) (oprand:byte) =
        let address = (int)c.WRAM.[(int)oprand]
        Some c.WRAM.[address]

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
    //let private adc8 (c:Config) =
    //    // Absolute, X
    //    // ▽ フェッチ ▽  メモリの読み込みのみ
    //    let opcode = c.WRAM.[(int)c.Register.PC]
    //    let size = Bytes.[(int)opcode]  // 3 Byte (opcode $4400,X)
    //    let oprand1 = c.WRAM.[(int)c.Register.PC + 1]
    //    let oprand2 = c.WRAM.[(int)c.Register.PC + 2]
    //    let oprand = ((int)oprand2 <<< 8) ||| (int)oprand1
    //    // アドレッシングモードの処理
    //    let address = oprand + (int)c.Register.X
    //    let memory = c.WRAM.[address]
    //    // ADCの処理
    //    let ret = c.Register.A + memory + (c.Register.S &&& 1uy)
    //    { c with Register = { c.Register with A = ret } }
    //let private clc (c:Config) =
    //    // Implied
    //    // ▽ フェッチ ▽  メモリの読み込みのみ
    //    let opcode = c.WRAM.[(int)c.Register.PC]
    //    // アドレッシングモードの処理

    //    // ADCの処理
    //    let p = c.Register.P &&& 0b11111110uy
    //    { c with Register = { c.Register with P = p } }
    //let private stx (c:Config) =
    //    // Zero Page, Y
    //    // ▽ フェッチ ▽  メモリの読み込みのみ
    //    let opcode = c.WRAM.[(int)c.Register.PC]
    //    let size = Bytes.[(int)opcode]  // 2 Byte (opcode $44,X)
    //    let oprand = c.WRAM.[(int)c.Register.PC + 1]
    //    // アドレッシングモードの処理
    //    let address = (int)(oprand + c.Register.Y) % 0x100
    //    // 処理
    //    c.WRAM.[address] <- c.Register.X
    //let private asl (c:Config) =
    //    // Accumlator
    //    // ▽ フェッチ ▽  メモリの読み込みのみ
    //    let opcode = c.WRAM.[(int)c.Register.PC]
    //    let size = Bytes.[(int)opcode]  // 1 Byte (opcode A) 
    //    // アドレッシングモードの処理

    //    // 処理
    //    let ret = c.Register.A <<< 1
    //    { c with Register = { c.Register with A = ret } }

    let private storeNop (c:Config) (result:byte option) =
        c
    let private storeA (c:Config) (result:byte option) =
        { c with Register = { c.Register with A = result.Value } }

    let private asl (c:Config) (oprand:int option) =
        Some (c.Register.A <<< 1)

    /// CPU の処理を1ステップ実行する。
    /// One Step Processing.
    let step (c:Config) =
        let opcode = (int)c.WRAM.[(int)c.Register.PC]
        //let oprand = readOpRand c <| Bytes.[opcode]

        Bytes.[opcode]
        |> readOpRand c     // Read Oprand
        |> nopA c           // Addressing Mode
        |> asl c            // Calc opcode
        |> storeA c         // Store

