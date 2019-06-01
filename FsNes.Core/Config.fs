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
    C : bool
    /// <summary>Zero</summary>
    /// <remarks>
    /// After most instructions that have a value result, if that value is zero, this flag will be set.
    /// 
    /// このフラグは直前の計算結果(比較命令は実際には結果が保存されていない減算を行っています)がゼロになった場合にセットされます。
    /// </remarks>
    Z : bool
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
    I : bool
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
    D : bool
    /// <summary>The B flag</summary>
    /// <remarks>
    /// While there are only six flags in the processor status register within the CPU, when transferred to the stack, there are two additional bits. These do not represent a register that cna hold a value but can be used to distinguish how ther flag s were pushed.
    /// Some 6502 references call this the "B flag", though it does not represent a n actual CPU register.
    /// Two interrupts (/IRQ and /NMI) and two instructions (PHP and BRK) push the flags to the stack. IN the byte pushed, bit 5 is always set to 1, and bit 4 is 1 if from an instruction (PHP or BRK) or 0 if from an interrupt line being pulled low (/IRQ or /NMI). This is the only time and place where the B flag actually exists: not in the status register it self, but in bit 4 of the copy that is written to the stack.
    /// ┏━━━━━━┳━━━━━━┳━━━━━━━━━━━━━┓
    /// ┃Instruction ┃Bit 5 and 4 ┃Side effects after pushing┃
    /// ┣━━━━━━╋━━━━━━╋━━━━━━━━━━━━━┫
    /// ┃PHP         ┃11          ┃None                      ┃
    /// ┣━━━━━━╋━━━━━━╋━━━━━━━━━━━━━┫
    /// ┃BRK         ┃11          ┃I is set to 1             ┃
    /// ┣━━━━━━╋━━━━━━╋━━━━━━━━━━━━━┫
    /// ┃/IRQ        ┃10          ┃I is set to 1             ┃
    /// ┣━━━━━━╋━━━━━━╋━━━━━━━━━━━━━┫
    /// ┃/NMI        ┃10          ┃I is set to 1             ┃
    /// ┗━━━━━━┻━━━━━━┻━━━━━━━━━━━━━┛
    /// Two instructions (PLP and RIT) pull a byte from the stack and set all the flags. They ignore bits 5 and 4.
    /// The only way for an IRQ handler to distingush /IRQ from BRK is to read the flags byte from the stack and test bit 4. The slowness of this is one reason why BRK wasn't used as a syscall mechanism. Instead, it was more often used to trigger a patching mechanism that hung off the /IRQ vector: a single byte in PROM, UVEPROM, flash, etc. would be forced to 0, and the IRQ handler would pick something to do instead based on the program counter.
    /// Unlike bit 5 and 4, bit 3 actually exists in P, even though it doesn't affect the ALU operation on the 2A03 or 2A07 CPU the way it does in MOS Technology's own chips.
    /// 
    /// CPU内のプロセッサステータスレジスタには6つのフラグしかありませんが、スタックに転送されると2つの追加ビットがあります。 これらは値を保持できるレジスタを表すものではありませんが、フラグがどのようにプッシュされたかを区別するために使用することができます。
    /// 実際のCPUレジスタを表すものではありませんが、6502の参照ではこれを「Bフラグ」と呼びます。
    /// 2つの割り込み（/IRQと/NMI）と2つの命令（PHPとBRK）がフラグをスタックにプッシュします。 プッシュされたバイトでは、ビット5は常に1に設定され、命令（PHPまたはBRK）からの場合はビット4、割り込みライン（/ IRQまたは/ NMI）がLowに設定されている場合は0になります。 これはBフラグが実際に存在する唯一の時間と場所です。ステータスレジスタ自体ではなく、スタックに書き込まれるコピーのビット4です。
    /// ┏━━━━━━┳━━━━━━┳━━━━━━━━━━━━━┓
    /// ┃Instruction ┃Bit 5 and 4 ┃Side effects after pushing┃
    /// ┣━━━━━━╋━━━━━━╋━━━━━━━━━━━━━┫
    /// ┃PHP         ┃11          ┃None                      ┃
    /// ┣━━━━━━╋━━━━━━╋━━━━━━━━━━━━━┫
    /// ┃BRK         ┃11          ┃I is set to 1             ┃
    /// ┣━━━━━━╋━━━━━━╋━━━━━━━━━━━━━┫
    /// ┃/IRQ        ┃10          ┃I is set to 1             ┃
    /// ┣━━━━━━╋━━━━━━╋━━━━━━━━━━━━━┫
    /// ┃/NMI        ┃10          ┃I is set to 1             ┃
    /// ┗━━━━━━┻━━━━━━┻━━━━━━━━━━━━━┛
    /// 2つの命令（PLPとRTI）がスタックから1バイトを取り出してすべてのフラグを設定します。 それらはビット5と4を無視します。
    /// IRQハンドラが/ IRQとBRKを区別する唯一の方法は、スタックからフラグバイトを読み取ってビット4をテストすることです。これが遅いことが、BRKがシステムコールメカニズムとして使用されなかった理由の1つです。 代わりに、/ IRQベクトルをハングアップさせるパッチメカニズムを起動するために使用されることが多くなりました。PROM、UVEPROM、フラッシュなどの1バイトは0に強制され、IRQハンドラは代わりに何かを実行します。 プログラムカウンタ
    /// ビット5および4とは異なり、ビット3は実際にはPに存在します。ただし、MOSテクノロジー独自のチップのように2A03または2A07 CPUのALU操作には影響しません。
    /// </remarks>
    B : bool
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
    V : bool
    /// <summary>Nagtive</summary>
    /// <remarks>
    /// ･After most instructions that have a value result, this flag will contain bit 7 of that result.
    /// ･BIT will load bit 7 of the addressed value directly into the N flag.
    /// 
    /// ･値が結果となるほとんどの命令の後、このフラグはその結果のビット7を含みます。
    /// ･BITはアドレス指定された値のビット7を直接Nフラグにロードします。
    /// </remarks>
    N : bool
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
    A : byte
    /// <summary>Indexes</summary>
    /// <remarks>
    /// X and Y are byte-wide and used for several addressing modes. They can be used as loop counters easily, using INC/DEC and branch instructions. Not being the accumulator, they have limited addresing modes themselves when loading and saving.
    /// XとYはバイト幅で、いくつかのアドレッシングモードに使用されます。 これらはINC / DECと分岐命令を使用して、ループカウンタとして簡単に使用できます。 アキュムレータではないため、ロード時および保存時のアドレス指定モード自体が制限されています。
    /// </remarks>
    X : byte
    /// <summary>Indexes</summary>
    /// <remarks>
    /// X and Y are byte-wide and used for several addressing modes. They can be used as loop counters easily, using INC/DEC and branch instructions. Not being the accumulator, they have limited addresing modes themselves when loading and saving.
    /// XとYはバイト幅で、いくつかのアドレッシングモードに使用されます。 これらはINC / DECと分岐命令を使用して、ループカウンタとして簡単に使用できます。 アキュムレータではないため、ロード時および保存時のアドレス指定モード自体が制限されています。
    /// </remarks>
    Y : byte
    /// <summary>Program Counter</summary>
    /// <remarks>
    /// The 2-byte program counter PC suports 65536 direct (unbanked) memory locations, however not all values are sent to the cartridge, It can be accessed either by allowing CPU's internal fetch logic increment the address bus, an interrupt(NMI, Rest, IRQ/BRQ), and unsing the RTS/JMP/JSR/Branch instructions.
    /// 2バイトプログラムカウンタPCは65536のダイレクト（非マッパー or バンク0の場合）メモリ位置をサポートしていますが、すべての値がカートリッジに送信されるわけではありません。CPUの内部フェッチロジックがアドレスバスをインクリメントさせることによってアクセスできます。 RTS / JMP / JSR / Branch 命令をアンシングします。
    /// </remarks>
    PC : int16
    /// <summary>Stack Pointer</summary>
    /// <remarks>
    /// S is byte-wide and can be accessed using interrupts, pulls, pushes, and transfers.
    /// Sはバイト幅で、割り込み、プル、プッシュ、転送を使ってアクセスできます。
    /// </remarks>
    S : byte
    /// <summary>Status Register</summary>
    /// <remarks>
    /// P has 6 bits used by the ALU but is byte-wide. PHP, PLP, arithmetic, testing, and branch instructions can access this register.
    /// PにはALUで使用される6ビットがありますが、バイト幅です。 PHP、PLP、算術演算、テスト、分岐命令はこのレジスタにアクセスできます。
    /// </remarks>
    P : byte
    //P : StatusFlag
}

/// 割り込み処理
type Interrupt =
    | Empty
    | Reset
    | NMI
    | IRQ
    | BRK

/// NES Config
type Config = {
    /// CPU Cycle Skip Count
    CpuSkip : int
    /// PPU Cycle Skip Count
    PpuSkip : int
    /// Register
    Register : Register
    /// CPU Memory
    WRAM : byte array
    /// Video Memory
    VRAM : byte array
    /// Hardware and Software Interrupt
    Interrupt: Interrupt
}

/// CPU処理の計算途中結果を保持する構造体
type CpuAccumulator = {
    Opcode: int
    Size: int
    Cycle: int
    Oprand: int option
    Address: int option
    Memory: byte option
    ResultMemory: byte option
    ResultA: byte option
    ResultX: byte option
    ResultY: byte option
    ResultPC: int16 option
    ResultS: byte option
    ResultP: byte option
    UpdateN: byte option
    UpdateV: byte option
    UpdateZ: byte option
    UpdateC: byte option
    UpdateNZ: bool
}