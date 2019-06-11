namespace FsNes.Core

open Microsoft.FSharp.Quotations

module Cpu =
    let bitcount (v:byte) =
        let a = (v &&& 0xAAuy >>> 1) + (v &&& 0x55uy)
        let b = (a &&& 0xCCuy >>> 2) + (a &&& 0x33uy)
        let c = (b &&& 0xF0uy >>> 4) + (b &&& 0x0Fuy)
        c

    // 
    let calcStatusCA a b = Some <| if a > b then Masks.StatusFlag.C else 0uy
    let calcStatusCS a b = Some <| if a >= b then Masks.StatusFlag.C else 0uy
    let calcStatusCR v = v &&& Masks.StatusFlag.C |> Some
    let calcStatusCL v = v >>> 7 &&& Masks.StatusFlag.C |> Some
    //let calcStatusZ v = [0..7] |> List.map ((>>>)v >> (&&&)1uy) |> List.reduce(|||) |> ((^^^)1uy)
    let calcStatusZ v = Some <| if v = 0uy then Masks.StatusFlag.Z else 0uy
    let calcStatusV a b = (a &&& 0x7Fuy) ^^^ (b &&& 0x7Fuy) >>> 1 |> Some
    let calcStatusN v = v &&& Masks.StatusFlag.N |> Some
    //let calcStatusP c v = c.Register.P &&& (0xFFuy ^^^ v) ||| v
    //let getC c = c.Register.P.C
    //let getZ c = c.Register.P.Z
    //let getI c = c.Register.P.I
    //let getD c = c.Register.P.D
    //let getV c = c.Register.P.V
    //let getN c = c.Register.P.N

    /// Plus Instruction.
    let public _adc c acm =
        let a = c.Register.A
        let ret = c.Register.A + acm.Value.Value + (c.Register.P.C &&& 1uy)
        let c = calcStatusCA a ret
        let v = calcStatusV a ret
        { acm with ResultMemory = Some ret; ResultC = c; ResultV = v }

    /// Other Illegal Opcode.
    let public _anc c acm =
        let ret = c.Register.A &&& acm.Value.Value
        { acm with ResultA = Some ret }

    /// Logical Conjunction.
    let public _and c acm =
        let ret = c.Register.A &&& acm.Value.Value
        { acm with ResultA = Some ret }

    /// Unoffical Opcode.
    let public _ane c acm =
        let ret = (c.Register.A ||| 0xEEuy) &&& c.Register.X &&& acm.Value.Value
        { acm with ResultA = Some ret }

    /// Unoffical Opcode.
    let public _arr c acm =
        let a = c.Register.A &&& acm.Value.Value
        let ret = (a >>> 1) ||| (c.Register.P.C <<< 7)
        let c = calcStatusCR a
        { acm with ResultA = Some ret; ResultC = c }
        
    /// Left Rotate
    let public _asl c acm =
        let v = acm.Value.Value
        let ret = v <<< 1
        let c = calcStatusCL v
        if acm.Address.IsNone then
            { acm with ResultA = Some ret; ResultC = c }
        else
            { acm with ResultMemory = Some ret; ResultC = c }

    /// Other Illegal Opcode.
    let public _asr c acm =
        let v = c.Register.A &&& acm.Value.Value
        let ret = v >>> 1
        let c = calcStatusCR v
        { acm with ResultA = Some ret; ResultC = c }

    /// Branch Main Function
    let public branch (digits, comp) c acm =
        if c.Register.P.[digits] = comp then
            let a = c.Register.PC + 1s
            let b = a + (int16)acm.Value.Value
            let cycle = acm.Cycle + 1 + (int)((a ^^^ b) >>> 8)
            { acm with ResultPC = Some b; Cycle = cycle }
        else
            acm

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
    let public _clc c acm = { acm with ResultC = Some 0uy }
    /// Clear Flag : I <- 0
    let public _cli c acm = { acm with ResultI = Some 0uy }
    /// Clear Flag : V <- 0
    let public _clv c acm = { acm with ResultV = Some 0uy }
    /// Clear Flag : D <- 0
    let public _cld c acm = { acm with ResultD = Some 0uy }
        
    /// Set Flag : C <- 1
    let public _stc c acm = { acm with ResultC = Some 1uy }
    /// Set Flag : I <- 1
    let public _sti c acm = { acm with ResultI = Some 1uy }
    /// Set Flag : D <- 1
    let public _std c acm = { acm with ResultD = Some 1uy }

    /// Test Instruction
    let public _bit c acm =
        let value = c.Register.A &&& acm.Value.Value
        let n = (value >>> 7) &&& 1uy |> Some
        let v = (value >>> 6) &&& 1uy |> Some
        let z = calcStatusZ value
        { acm with ResultN = n; ResultV = v; ResultZ = z; }

    /// Test Instruction
    let public _cmp c acm =
        let value = c.Register.A - acm.Value.Value
        let c = (value >>> 6) &&& 1uy |> Some
        let n = (value >>> 7) &&& 1uy |> Some
        let z = calcStatusZ value
        { acm with ResultN = n; ResultZ = z; ResultC = c; }

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

    /// Read Oprand
    let public readOpRand c acm =
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

    /// Page Boundary Crossing
    let public calcCycle cycle address offset =
        let a = ((address &&& 0x00FF) + offset) >>> 8
        cycle + a

    /// Addressing Mode : NOP
    let public nopAM c acm =
        acm
    /// Addressing Mode : Zero Page, X
    let public zpx c acm =
        let address = (int)((byte)acm.Oprand.Value + c.Register.X)
        { acm with Address = Some address; Value = Some(c.WRAM.[address]) }
    /// Addressing Mode : Zero Page, Y
    let public zpy c acm =
        let address = (int)((byte)acm.Oprand.Value + c.Register.Y)
        { acm with Address = Some address; Value = Some(c.WRAM.[address]) }
    /// Addressing Mode : Absolute, X
    let public aix c acm =
        let address = acm.Oprand.Value + (int)c.Register.X
        let cycle = calcCycle acm.Cycle acm.Oprand.Value ((int)c.Register.X)
        { acm with Address = Some address; Value = Some(c.WRAM.[address]); Cycle = cycle }
    /// Addressing Mode : Absolute, Y
    let public aiy c acm =
        let address = acm.Oprand.Value + (int)c.Register.Y
        let cycle = calcCycle acm.Cycle acm.Oprand.Value ((int)c.Register.Y)
        { acm with Address = Some address; Value = Some(c.WRAM.[address]); Cycle = cycle }
    /// Addressing Mode : (Indirect, X)
    let public iix c acm =
        let address = (int)((byte)acm.Oprand.Value + c.Register.X)
        let address2 = (int)c.WRAM.[address]
        { acm with Address = Some address2; Value = Some(c.WRAM.[address2]) }
    /// Addressing Mode : (Indirect), Y
    let public iiy c acm =
        let address = (int)c.WRAM.[acm.Oprand.Value]
        let address2 = address + (int)c.Register.Y
        let cycle = calcCycle acm.Cycle address ((int)c.Register.Y)
        { acm with Address = Some address2; Value = Some(c.WRAM.[address2]); Cycle = cycle }
    /// Addressing Mode : Implied
    let public imp c acm =
        acm
    /// Addressing Mode : Accumlator
    let public acm c acm =
        { acm with Value = Some c.Register.A }
    /// Addressing Mode : Immediate
    let public imm c acm =
        { acm with Value = Some <| (byte)acm.Oprand.Value }
    /// Addressing Mode : Zero Page
    let public zp_ c acm =
        let addr = acm.Oprand.Value
        let mem = c.WRAM.[addr]
        { acm with Address = Some addr; Value = Some mem }
    /// Addressing Mode : Absolute
    let public abs c acm =
        let addr = acm.Oprand.Value
        let mem = c.WRAM.[addr]
        { acm with Address = Some addr; Value = Some mem }
    /// Addressing Mode : Relative
    /// * The cycle calculation when crossing page boundaries is performed by "Instruction".
    let public rel c acm =
        let addr = acm.Oprand.Value
        let mem = c.WRAM.[addr]
        { acm with Address = Some addr; Value = Some mem }
    /// Addressing Mode : Indirect   (JMP ($5597))
    let public ind c acm =
        let addr = (int)c.WRAM.[acm.Oprand.Value]
        { acm with Address = Some addr; }

    /// Reflect the calculation results to 'Config'.
    /// 計算結果を config に反映させる。
    let public storeResult acm c =
        let set o f c =
            match o with
            | Some x -> f x c
            | None -> c
        let setP =
            set acm.ResultC (fun v p -> { p with C = v })
            >> set acm.ResultZ (fun v p -> { p with Z = v })
            >> set acm.ResultI (fun v p -> { p with I = v })
            >> set acm.ResultD (fun v p -> { p with D = v })
            >> set acm.ResultV (fun v p -> { p with V = v })
            >> set acm.ResultN (fun v p -> { p with N = v })
        set acm.ResultMemory (fun v c -> c.WRAM.[acm.Address.Value] <- v; c) c
        |> set acm.ResultPC (fun v c -> { c with Register = { c.Register with PC = v } })
        |> set acm.ResultA (fun v c -> { c with Register = { c.Register with A = v } })
        |> set acm.ResultX (fun v c -> { c with Register = { c.Register with X = v } })
        |> set acm.ResultY (fun v c -> { c with Register = { c.Register with Y = v } })
        |> set acm.ResultS (fun v c -> { c with Register = { c.Register with S = v } })
        |> fun c -> { c with Register = { c.Register with P = setP c.Register.P } }
    /// 計算結果をもとにステータスフラグ N Z の更新を行う。
    let public updateNZ acm c =
        if acm.UpdateNZ then
            match acm.ResultMemory, acm.ResultA with
            | Some value, None
            | None, Some value ->
                let n = value &&& (1uy <<< 7)                           // 負の数なら7bit目を 1
                let z = (if value = 0uy then 1uy else 0uy) <<< 1        // 値が 0 なら2bit目を 1
                { c with Register = { c.Register with P = { c.Register.P with N = n; Z = z } } }
            | _ -> c
        else c
    /// CPUアキュムレータの結果を Config に反映させる。
    let public update c acm =
        storeResult acm c
        |> updateNZ acm

    /// Create CPU Accumulator
    let createAccumulator opcode : CpuAccumulator =
        {
            Opcode = opcode
            Size = Bytes.[opcode]
            Cycle = Cycles.[opcode]
            Oprand = None
            Address = None
            Value = None
            ResultMemory = None
            ResultA = None
            ResultX = None
            ResultY = None
            ResultPC = None
            ResultS = None
            ResultC = None
            ResultZ = None
            ResultI = None
            ResultD = None
            ResultV = None
            ResultN = None
            UpdateNZ = UpdateNZ.[opcode]
        }

    /// CPU の処理を1ステップ実行する。
    /// One Step Processing.
    let public step (opcode:int) (c:Config) =
        c
        //opcode
        //|> createAccumulator
        //|> readOpRand c     // Read Oprand
        //|> ind c            // Addressing Mode
        //|> clc c            // Calc opcode
        //|> update c         // Update Register Flags or Memory
            
    /// Cpu Process Running.
    let run (c:Config) =
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
                step opcode c

