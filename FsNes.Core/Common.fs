namespace FsNes.Core

module Common =
    let getInitConfig () : Config =
        {
            Register = {
                A = 0uy
                X = 0uy
                Y = 0uy
                PC = 0s
                S = 0uy
                P = 0uy
            }
            WRAM = Array.zeroCreate 0xFFFF
        }

