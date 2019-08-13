namespace FsNes.Core

open System
open System.IO
open System.Text.RegularExpressions
open FsNes.Core

module TestReader =
    let private initConfig =
        {
            CpuSkip = 0
            PpuSkip = 0
            Register =
                {
                    A = 0uy
                    X = 0uy
                    Y = 0uy
                    PC = 0us
                    S = 0uy
                    P =
                        {
                            N = 0uy
                            V = 0uy
                            B = 0uy
                            D = 0uy
                            I = 0uy
                            Z = 0uy
                            C = 0uy
                        }
                }
            WRAM = [| for i in 0..0xFFFF -> 0uy |]
            VRAM = [| for i in 0..0xFFFF -> 0uy |]
            Interrupt = Interrupt.Empty
        }

    let private split text = Regex.Split(text, "\s+")
    let private toInt index (arr:string[]) = Convert.ToInt32(arr.[index], 16)
    let private toByte index (arr:string[]) = Convert.ToByte(arr.[index], 16)
    let private toUint16 index (arr:string[]) = Convert.ToUInt16(arr.[index], 16)
    let private writeRAM (ram:byte[]) (text:string) index =
        let arr = text.Substring(2, 767).Split(" ")
        let b = index * 0x100
        for i in 0..0xFF do
            ram.[i+b] <- toByte i arr
        if index = 0xFF then -1
        else index + 1

    let private startsWith (exp:string) (text:string) = text.StartsWith(exp)
    let private (|Interrupt|_|) config text =
        if startsWith "Interrupt" text then
            let arr = split text
            let interrupt = Enum.Parse<Interrupt>(arr.[1])
            Some { config with Interrupt = interrupt }
        else
            None
    let private (|CpuSkip|_|) config text =
        if startsWith "CpuSkip" text then
            let v = split text |> toInt 1
            Some { config with CpuSkip = v }
        else
            None
    let private (|PpuSkip|_|) config text =
        if startsWith "PpuSkip" text then
            let v = split text |> toInt 1
            Some { config with PpuSkip = v }
        else
            None
    let private (|Register|_|) (config:Config) text =
        if startsWith "Register" text then
            let arr = split text
            let a = toByte 1 arr
            let x = toByte 2 arr
            let y = toByte 3 arr
            let pc = toUint16 4 arr
            let s = toByte 5 arr
            { config with
                Register =
                    { config.Register with
                        A = a
                        X = x
                        Y = y
                        PC = pc
                        S = s
                    }
            } |> Some
        else
            None
    let private (|StatusFlag|_|) (config:Config) text =
        if startsWith "StatusFlag" text then
            let arr = split text
            let n = toByte 1 arr
            let v = toByte 2 arr
            let b = toByte 3 arr
            let d = toByte 4 arr
            let i = toByte 5 arr
            let z = toByte 6 arr
            let c = toByte 7 arr
            { config with
                Register =
                    { config.Register with
                        P =
                            {
                                N = n
                                V = v
                                B = b
                                D = d
                                I = i
                                Z = z
                                C = c
                            }
                    }
            } |> Some
        else
            None
    let private (|WRAM|_|) (config:Config) index text =
        if text = "WRAM" then Some 0
        elif index < 0 then None
        elif text.Length = 777 then
            writeRAM config.WRAM text index
            |> Some
        else None
    let private (|VRAM|_|) (config:Config) index text =
        if text = "VRAM" then Some 0
        elif index < 0 then None
        elif text.Length = 777 then
            writeRAM config.VRAM text index
            |> Some
        else None

    let private skipComment (text:string) = not <| text.StartsWith("#")
    let private foldConfig (config,(wram,vram)) text =
        match text with
        | Interrupt config c -> c,(wram,vram)
        | CpuSkip config c -> c,(wram,vram)
        | PpuSkip config c -> c,(wram,vram)
        | Register config c -> c,(wram,vram)
        | StatusFlag config c -> c,(wram,vram)
        | WRAM config wram index -> config,(index,vram)
        | VRAM config vram index -> config,(wram,index)
        | _ -> config,(wram,vram)

    let Read (path:string) : Config =
        let config = initConfig
        File.ReadLines path
        |> Seq.filter skipComment
        |> Seq.fold foldConfig (config,(-1,-1))
        |> fst

