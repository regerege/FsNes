namespace FsNes.Core

/// <summary>Status flags</summary>
/// <remarks>
/// The flags register, also called processor status or just P, is one of the six architectural registers on the 6502 family CPU.
/// It is composed of six one-bit registers; instructions modify one or more bits and leave others unchanged.
///
/// Instructions that save or restore the flags map them to bits in the architectural 'P' register as follows:
/// 7654 3210 (bit)
/// NVss DIZC
/// |||| ||||
/// |||| |||+- Carry
/// |||| ||+-- Zero
/// |||| |+--- Interrupt Disable
/// |||| +---- Decimal
/// ||++------ No CPU effect, see: the B flag
/// |+-------- Overflow
/// +--------- Negative
/// ･The PHP (Push Processor Status) and PLP (Pull Processor Status) instructions can be used to retrieve or set this register directly via the stack.
/// ･Interrupts, including the NMI and also the pseudo-interrupt BRK instruction, implicitly push the status register to the stack.
/// ･Interrupts returning with RTI will implicitly pull the saved status register from the stack.
///
/// flagsレジスタは、プロセッサステータスまたは単にPとも呼ばれ、6502ファミリCPUの6つのアーキテクチャレジスタのうちの1つです。
/// 6個の1ビットレジスタで構成されています。 命令は1つ以上のビットを変更し、他のものは変更しません。
/// 
/// フラグを保存または復元する命令は、それらをアーキテクチャの 'P'レジスタ内のビットに次のようにマッピングします:
/// 7654 3210 (bit)
/// NVss DIZC
/// |||| ||||
/// |||| |||+- キャリー
/// |||| ||+-- ゼロ
/// |||| |+--- 割り込み不可
/// |||| +---- Decimal
/// ||++------ CPUへの影響はありません。参照：Bフラグ
/// |+-------- オーバーフロー
/// +--------- ネガティブ
/// ･PHP（プッシュプロセッサステータス）およびPLP（プルプロセッサステータス）命令は、スタックを介して直接このレジスタを検索または設定するために使用することができる。
/// ･NMIや疑似割り込みBRK命令を含む割り込みは、暗黙的にステータスレジスタをスタックにプッシュします。
/// ･RTIで戻る割り込みは、保存されたステータスレジスタを暗黙的にスタックから取得します。
/// </remarks>
type StatusFlag = {
    /// <summary>Carry</summary>
    /// <remarks>
    /// ･After ADC, this is the carry result of the addition.
    /// ･After SBC or CMP, this flag will be set if no borrow was the result, or alternatively a "greater than or equal" result.
    /// ･After a shift instruction (ASL, LSR, ROL, ROR), this contains the bit that was shifted out.
    /// ･Increment and decrement instructions do not affect the carry flag.
    /// ･Can be set or cleared directly with SEC, CLC.
    /// 
    /// ･ADCの後、これは加算のキャリー結果です。
    /// ･SBCまたはCMPの後、借入が結果に含まれていない場合、あるいは「以上」の結果である場合は、このフラグが設定されます。
    /// ･シフト命令（ASL、LSR、ROL、ROR）の後、これにはシフトアウトされたビットが含まれます。
    /// ･増減命令はキャリーフラグに影響を与えません。
    /// ･SEC、CLCで直接設定またはクリアできます。
    /// </remarks>
    C : int
    /// <summary>Zero</summary>
    /// <remarks>
    /// After most instructions that have a value result, if that value is zero, this flag will be set.
    /// 
    /// このフラグは直前の計算結果(比較命令は実際には結果が保存されていない減算を行っています)がゼロになった場合にセットされます。
    /// </remarks>
    Z : int
    /// <summary>割り込み不可</summary>
    /// <remarks>
    /// ･When set, all interrupts except the NMI are inhibited.
    /// ･Can be set or cleared directly with SEI, CLI.
    /// ･Automatically set by the CPU when an IRQ is triggered, and restored to its previous state by RTI.
    /// ･If the /IRQ line is low (IRQ pending) when this flag is cleared, an interrupt will immediately be triggered.
    /// 
    /// ･設定すると、NMIを除くすべての割り込みが禁止されます。
    /// ･SEI、CLIを使用して直接設定または消去できます。
    /// ･IRQがトリガーされたときにCPUによって自動的に設定され、RTIによって前の状態に復元されます。
    /// ･このフラグがクリアされているときに/ IRQラインがロー（IRQペンディング）になっていると、直ちに割り込みがトリガされます。
    /// </remarks>
    I : int
    /// <summary>Decimal</summary>
    /// <remarks>
    /// ･On the NES, this flag has no effect.
    /// ･On the original 6502, this flag causes some arithmetic instructions to use binary-coded decimal representation to make base 10 calculations easier.
    /// ･Can be set or cleared directly with SED, CLD.
    /// 
    /// ･NESでは、このフラグは無効です。
    /// ･オリジナルの6502では、このフラグはいくつかの算術命令に10進法の2進数表現を使い、10進法の計算をより簡単にします。
    /// ･SED、CLDを使用して直接設定または消去できます。
    /// </remarks>
    D : int
    /// <summary>The B flag</summary>
    /// <remarks>
    /// 
    /// </remarks>
    B : int
    /// <summary>オーバーフロー</summary>
    /// <remarks>
    /// ･ADC, SBC, and CMP will set this flag if the signed result would be invalid, necessary for making signed comparisons.
    /// ･BIT will load bit 6 of the addressed value directly into the V flag.
    /// ･Can be cleared directly with CLV. There is no corresponding set instruction.
    /// 
    /// ･署名された結果が無効である場合、ADC、SBC、およびCMPはこのフラグを設定します。
    /// ･BITはアドレス指定された値のビット6を直接Vフラグにロードします。
    /// ･CLVで直接クリアできます。 対応する設定命令はありません。
    /// </remarks>
    V : int
    /// <summary>Nagtive</summary>
    /// <remarks>
    /// ･After most instructions that have a value result, this flag will contain bit 7 of that result.
    /// ･BIT will load bit 7 of the addressed value directly into the N flag.
    /// 
    /// ･値が結果となるほとんどの命令の後、このフラグはその結果のビット7を含みます。
    /// ･BITはアドレス指定された値のビット7を直接Nフラグにロードします。
    /// </remarks>
    N : int
}

/// <summary>Registers</summary>
/// <remarks>
/// The registers on the NES CPU are just like on the 6502. There is the accumulator, 2 indexes, a program counter, the stack pointer, and the status register. Unlike many CPU families, members do not have generic groups of registers like say, R0 through R7.
/// NES CPUのレジスタは、6502とまったく同じです。アキュムレータ、2つのインデックス、プログラムカウンタ、スタックポインタ、およびステータスレジスタがあります。 多くのCPUファミリとは異なり、メンバには、たとえばR0からR7のような汎用レジスタグループはありません。
/// </remarks>
type Register = {
    /// <summary>Acumulator</summary>
    /// <remarks>
    /// A is byte-wide and along with the arithmetic logic unit (ALU), supports using the status register for carrying, overflow detection, and so on.
    /// Aはバイト幅で、算術論理装置（ALU）とともに、運搬、オーバーフロー検出などのためのステータスレジスタの使用をサポートします。
    /// </remarks>
    A : int
    /// <summary>Indexes</summary>
    /// <remarks>
    /// X and Y are byte-wide and used for several addressing modes. They can be used as loop counters easily, using INC/DEC and branch instructions. Not being the accumulator, they have limited addresing modes themselves when loading and saving.
    /// XとYはバイト幅で、いくつかのアドレッシングモードに使用されます。 これらはINC / DECと分岐命令を使用して、ループカウンタとして簡単に使用できます。 アキュムレータではないため、ロード時および保存時のアドレス指定モード自体が制限されています。
    /// </remarks>
    X : int
    /// <summary>Indexes</summary>
    /// <remarks>
    /// X and Y are byte-wide and used for several addressing modes. They can be used as loop counters easily, using INC/DEC and branch instructions. Not being the accumulator, they have limited addresing modes themselves when loading and saving.
    /// XとYはバイト幅で、いくつかのアドレッシングモードに使用されます。 これらはINC / DECと分岐命令を使用して、ループカウンタとして簡単に使用できます。 アキュムレータではないため、ロード時および保存時のアドレス指定モード自体が制限されています。
    /// </remarks>
    Y : int
    /// <summary>Program Counter</summary>
    /// <remarks>
    /// The 2-byte program counter PC suports 65536 direct (unbanked) memory locations, however not all values are sent to the cartridge, It can be accessed either by allowing CPU's internal fetch logic increment the address bus, an interrupt(NMI, Rest, IRQ/BRQ), and unsing the RTS/JMP/JSR/Branch instructions.
    /// 2バイトプログラムカウンタPCは65536のダイレクト（非マッパー or バンク0の場合）メモリ位置をサポートしていますが、すべての値がカートリッジに送信されるわけではありません。CPUの内部フェッチロジックがアドレスバスをインクリメントさせることによってアクセスできます。 RTS / JMP / JSR / Branch 命令をアンシングします。
    /// </remarks>
    PC : int
    /// <summary>Stack Pointer</summary>
    /// <remarks>
    /// S is byte-wide and can be accessed using interrupts, pulls, pushes, and transfers.
    /// Sはバイト幅で、割り込み、プル、プッシュ、転送を使ってアクセスできます。
    /// </remarks>
    S : int
    /// <summary>Status Register</summary>
    /// <remarks>
    /// P has 6 bits used by the ALU but is byte-wide. PHP, PLP, arithmetic, testing, and branch instructions can access this register.
    /// PにはALUで使用される6ビットがありますが、バイト幅です。 PHP、PLP、算術演算、テスト、分岐命令はこのレジスタにアクセスできます。
    /// </remarks>
    P : StatusFlag
}

type Config = {
    Register : Register
}
