namespace FsNes.Core

/// マジックナンバー
module Masks =
    /// ステータスフラグ用
    module StatusFlag =
        /// Mask value of status flag C [0]
        let C = 0b00000001uy
        /// Mask value of status flag Z [1]
        let Z = 0b00000010uy
        /// Mask value of status flag I [2]
        let I = 0b00000100uy
        /// Mask value of status flag D [3]
        let D = 0b00001000uy
        /// Mask value of status flag V [6]
        let V = 0b01000000uy
        /// Mask value of status flag N [7]
        let N = 0b10000000uy

