namespace FsNes.Core

open Microsoft.FSharp.Quotations

module Cpu =
    let bitcount (v:byte) =
        let a = (v &&& 0xAAuy >>> 1) + (v &&& 0x55uy)
        let b = (a &&& 0xCCuy >>> 2) + (a &&& 0x33uy)
        let c = (b &&& 0xF0uy >>> 4) + (b &&& 0x0Fuy)
        c

    let calcStatusC a b = if a > b then Masks.StatusFlag.C else 0uy
    let calcStatusZ v = if v = 0uy then Masks.StatusFlag.Z else 0uy
    let calcStatusV a b = (a &&& 0x7Fuy) ^^^ (b &&& 0x7Fuy) >>> 1
    let calcStatusN v = v &&& Masks.StatusFlag.N
    let getC c = c.Register.P &&& Masks.StatusFlag.C
    let getZ c = c.Register.P &&& Masks.StatusFlag.Z
    let getI c = c.Register.P &&& Masks.StatusFlag.I
    let getD c = c.Register.P &&& Masks.StatusFlag.D
    let getV c = c.Register.P &&& Masks.StatusFlag.V
    let getN c = c.Register.P &&& Masks.StatusFlag.N

    /// Plus Instruction.
    let private _adc c acm =
        let a = c.Register.A
        let ret = c.Register.A + acm.Value.Value + (c.Register.P &&& 1uy)
        let p = calcStatusC a ret ||| calcStatusV a ret
        { acm with ResultMemory = Some ret; ResultP = Some p }

    /// Other Illegal Opcode.
    let private _anc c acm =
        let ret = c.Register.A &&& acm.Value.Value
        { acm with ResultA = Some ret }

    /// Logical Conjunction.
    let private _and c acm =
        let ret = c.Register.A &&& acm.Value.Value
        { acm with ResultA = Some ret }

    /// Unoffical Opcode.
    let private _ane c acm =
        let ret = (c.Register.A ||| 0xEEuy) &&& c.Register.X &&& acm.Value.Value
        { acm with ResultA = Some ret }

    /// Unoffical Opcode.
    let private _arr c acm =
        let a = c.Register.A &&& acm.Value.Value
        let ret = (a >>> 1) ||| (getC c <<< 7)
        let c = (c.Register.A &&& acm.Value.Value) &&& 1uy
        { acm with ResultA = Some ret; UpdateC = Some c }
        
    /// Left Rotate
    let private _asl c acm =
        let value =
            if acm.Memory.IsSome then
                acm.Memory.Value
            else
                c.Register.A
        let f v =
            let ret = v <<< 1
            let c = v >>> 7
            { acm with ResultA = Some ret; UpdateC = Some c }
        match acm.Memory with
        | Some v -> v
        | None -> c.Register.A
        |> f

    /// Other Illegal Opcode.
    let private _asr c acm =
        let v = c.Register.A &&& acm.Memory.Value
        let ret = v >>> 1
        let c = v &&& 1uy
        { acm with ResultA = Some ret; UpdateC = Some c }

    /// Branch Main Function
    let private branch (digits, comp) c acm =
        if (c.Register.P >>> digits &&& 1uy) = comp then
            let a = c.Register.PC + 1s
            let b = a + (int16)acm.Memory.Value
            let cycle = acm.Cycle + 1 + (int)((a ^^^ b) >>> 8)
            { acm with ResultPC = Some b; Cycle = cycle }
        else
            acm

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
    let private _clc c acm = { acm with UpdateC = Some 0uy }
    /// Clear Flag : I <- 0
    let private _cli c acm = { acm with UpdateI = Some 0uy }
    /// Clear Flag : V <- 0
    let private _clv c acm = { acm with UpdateV = Some 0uy }
    /// Clear Flag : D <- 0
    let private _cld c acm = { acm with UpdateD = Some 0uy }
        
    /// Set Flag : C <- 1
    let private _stc c acm = { acm with UpdateC = Some 1uy }
    /// Set Flag : I <- 1
    let private _sti c acm = { acm with UpdateI = Some 1uy }
    /// Set Flag : D <- 1
    let private _std c acm = { acm with UpdateD = Some 1uy }

    /// Test Instruction
    let private _bit c acm =
        let value = c.Register.A &&& acm.Memory.Value
        let n = (value >>> 7) &&& 1uy
        let v = (value >>> 6) &&& 1uy
        let z = if value = 0uy then 1uy else 0uy
        { acm with UpdateN = Some n; UpdateV = Some v; UpdateZ = Some z; }

    /// Test Instruction
    let private _cmp c acm =
        let value = c.Register.A - acm.Memory.Value
        let c = (value >>> 6) &&& 1uy
        let n = (value >>> 7) &&& 1uy
        let z = if value = 0uy then 1uy else 0uy
        { acm with UpdateN = Some n; UpdateV = Some v; UpdateZ = Some z; }

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

    /// Read Oprand
    let private readOpRand c acm =
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
    let private calcCycle cycle address offset =
        let a = ((address &&& 0x00FF) + offset) >>> 8
        cycle + a

    /// Addressing Mode : NOP
    let private nopAM c acm =
        acm
    /// Addressing Mode : Zero Page, X
    let private zpx c acm =
        let address = (int)((byte)acm.Oprand.Value + c.Register.X)
        { acm with Address = Some address; Memory = Some(c.WRAM.[address]) }
    /// Addressing Mode : Zero Page, Y
    let private zpy c acm =
        let address = (int)((byte)acm.Oprand.Value + c.Register.Y)
        { acm with Address = Some address; Memory = Some(c.WRAM.[address]) }
    /// Addressing Mode : Absolute, X
    let private aix c acm =
        let address = acm.Oprand.Value + (int)c.Register.X
        let cycle = calcCycle acm.Cycle acm.Oprand.Value ((int)c.Register.X)
        { acm with Address = Some address; Memory = Some(c.WRAM.[address]); Cycle = cycle }
    /// Addressing Mode : Absolute, Y
    let private aiy c acm =
        let address = acm.Oprand.Value + (int)c.Register.Y
        let cycle = calcCycle acm.Cycle acm.Oprand.Value ((int)c.Register.Y)
        { acm with Address = Some address; Memory = Some(c.WRAM.[address]); Cycle = cycle }
    /// Addressing Mode : (Indirect, X)
    let private iix c acm =
        let address = (int)((byte)acm.Oprand.Value + c.Register.X)
        let address2 = (int)c.WRAM.[address]
        { acm with Address = Some address2; Memory = Some(c.WRAM.[address2]) }
    /// Addressing Mode : (Indirect), Y
    let private iiy c acm =
        let address = (int)c.WRAM.[acm.Oprand.Value]
        let address2 = address + (int)c.Register.Y
        let cycle = calcCycle acm.Cycle address ((int)c.Register.Y)
        { acm with Address = Some address2; Memory = Some(c.WRAM.[address2]); Cycle = cycle }
    /// Addressing Mode : Implied
    let private imp c acm =
        acm
    /// Addressing Mode : Accumlator
    let private acm c acm =
        { acm with Memory = Some c.Register.A }
    /// Addressing Mode : Immediate
    let private imm c acm =
        { acm with Memory = Some <| (byte)acm.Oprand.Value }
    /// Addressing Mode : Zero Page
    let private zp_ c acm =
        let addr = acm.Oprand.Value
        let mem = c.WRAM.[addr]
        { acm with Address = Some addr; Memory = Some mem }
    /// Addressing Mode : Absolute
    let private abs c acm =
        let addr = acm.Oprand.Value
        let mem = c.WRAM.[addr]
        { acm with Address = Some addr; Memory = Some mem }
    /// Addressing Mode : Relative
    /// * The cycle calculation when crossing page boundaries is performed by "Instruction".
    let private rel c acm =
        let addr = acm.Oprand.Value
        let mem = c.WRAM.[addr]
        { acm with Address = Some addr; Memory = Some mem }
    /// Addressing Mode : Indirect   (JMP ($5597))
    let private ind c acm =
        let addr = (int)c.WRAM.[acm.Oprand.Value]
        { acm with Address = Some addr; }

    /// Reflect the calculation results to 'Config'.
    /// 計算結果を config に反映させる。
    let private storeResult acm c =
        let set o f c =
            match o with
            | Some x -> f x c
            | None -> c
        set acm.ResultMemory (fun v c -> c.WRAM.[acm.Address.Value] <- v; c) c
        |> set acm.ResultPC (fun v c -> { c with Register = { c.Register with PC = v } })
        |> set acm.ResultA (fun v c -> { c with Register = { c.Register with A = v } })
        |> set acm.ResultX (fun v c -> { c with Register = { c.Register with X = v } })
        |> set acm.ResultY (fun v c -> { c with Register = { c.Register with Y = v } })
        |> set acm.ResultS (fun v c -> { c with Register = { c.Register with S = v } })
        |> set acm.ResultP (fun v c -> { c with Register = { c.Register with P = v } })
    /// 計算結果をもとにステータスフラグ N Z の更新を行う。
    let private updateNZ acm c =
        if acm.UpdateNZ then
            match acm.ResultMemory, acm.ResultA with
            | Some value, None
            | None, Some value ->
                let n = value &&& (1uy <<< 7)                           // 負の数なら7bit目を 1
                let z = (if value = 0uy then 1uy else 0uy) <<< 1        // 値が 0 なら2bit目を 1
                let p = c.Register.P ||| n ||| z
                { c with Register = { c.Register with P = p } }
            | _ -> c
        else c
    ///// ステータスフラグの更新
    //let private updateP acm c =
    //    let mask,value =
    //        seq[
    //            0, acm.UpdateC
    //            1, acm.UpdateZ
    //            2, acm.UpdateI
    //            3, acm.UpdateD
    //            6, acm.UpdateV
    //            7, acm.UpdateN
    //        ]
    //        |> Seq.filter (snd >> Option.isSome)
    //        |> Seq.fold (fun (m,v) (a,b) -> (m ||| (1uy <<< a)),(v ||| b.Value)) (0uy, 0uy)
    //    let p = c.Register.P &&& (mask ^^^ 0xFFuy) ||| value
    //    { c with Register = { c.Register with P = p } }
    /// CPUアキュムレータの結果を Config に反映させる。
    let private update c acm =
        storeResult acm c
        //|> updateP acm
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
            ResultP = None
            //UpdateN = None
            //UpdateV = None
            //UpdateI = None
            //UpdateD = None
            //UpdateZ = None
            //UpdateC = None
            UpdateNZ = UpdateNZ.[opcode]
        }

    /// CPU の処理を1ステップ実行する。
    /// One Step Processing.
    let private step (opcode:int) (c:Config) =
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

