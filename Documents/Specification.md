--------------------------------------------------
# NESの仕様
--------------------------------------------------
大きく分けて３つに分けて記載していきます。（気が向けば、だけど！）
基本的には NES で使われている 6502 をベースとした独自のCPUについての仕様をメインで記載するもとします。

気が向いた時に CPU 以外の APU と PPU についても記載して行ければと考えていますが、あまり余力が無いので期待はしないようにお願いします。


1. [CPU 6502 の仕様](#CPU6502)
  1. [クロック数](#クロック数)
  2. [命令](#命令)
  3. [命令の仕様](#命令の仕様)


<a name="CPU6502"></a>
--------------------------------------------------
## CPU 6502 の仕様
--------------------------------------------------
NES の CPU は 6502 をベースに作られています。
CPU の命令もかなり近しいものとなっていますが同じものではないことをご認識下さい。


<a name="クロック数"></a>
### クロック数
CPU のクロック数は NES に取り付けられている水晶振動子の12分周となっています。
水晶振動子のクロック数をベースとした場合のクロック数は以下の通りとなります。
水晶振動子のクロック数をマスタークロックと名付けます。
※12分周 とはメインとなるクロック数から12回に1回のクロックを得ること、電子工作の基礎っぽいので分からない人はそちらも勉強してみると楽しいかと思います。

|       名称       |  クロック数   | 分周 |
|:-----------------|--------------:|-----:|
| マスタークロック | 21477272 Hz   |      |
| CPU              | 約 1789772 Hz |  12  |
| PPU              | 5369318 Hz    |   4  |


<a name="命令"></a>
### 命令
非公式命令を含めると全部で 72 個 存在します。

|命令 | 名称                                         |
|-----|----------------------------------------------|
| ADC | Add M to A with Carry                        |
| ANC | ※1                                          |
| AND | And Memory With Accumulator                  |
| ANE | ※1                                          |
| ARR | ※1                                          |
| ASL | Shift Left One Bit (M or A)                  |
| ASR | ※1                                          |
| BCC | Branch on Carry Clear                        |
| BCS | Branch on Carry Set                          |
| BEQ | Branch on Equal (Zero Set)                   |
| BIT | Test Bits in Memory with Accumulator         |
| BMI | Branch on Result Minus                       |
| BNE | Branch on Result Not Zero                    |
| BPL | Branch on Result Plus                        |
| BRK | Force Break                                  |
| BVC | Branch on Overflow Clear                     |
| BVS | Branch on Overflow Set                       |
| CLC | Clear Carry Flag                             |
| CLD | Clear Decimal Flag                           |
| CLI | Clear Interrupt Disable Flag                 |
| CLV | Clear Overflow Flag                          |
| CMP | Compare Memory with Accumulator              |
| CPX | Compare Memory with X Register               |
| CPY | Compare Memory with Y Register               |
| DCP | ※1                                          |
| DEC | Decrement Memory by One                      |
| DEX | Decrement Index X Register by One            |
| DEY | Decrement Index Y Register by One            |
| EOR | Exclusive Or Memory With Accumulator         |
| INC | Increment Memory by One                      |
| INX | Increment Index X Register by One            |
| INY | Increment Index Y Register by One            |
| ISB | ※1                                          |
| JMP | Jump to New Location                         |
| JSR | Jump to New Location Saving Return Address   |
| KIL | ※2                                          |
| LAS | ※1                                          |
| LAX | ※1                                          |
| LDA | Load Accumulator With Memory                 |
| LDX | Load X Index With Memory                     |
| LDY | Load Y Index With Memory                     |
| LSR | Shift One Bit Right (Memory or Accumulator)  |
| NOP | No Operation                                 |
| ORA | Or Memory With Accumulator                   |
| PHA | Push Accumulator on Stack                    |
| PHP | Push Processer Status on Stack               |
| PLA | Pull Accumulator from Stack                  |
| PLP | Pull Processer Status from Stack             |
| RLA | ※1                                          |
| ROL | Rotate One Bit Left (Memory or Accumulator)  |
| ROR | Rotate One Bit Right (Memory or Accumulator) |
| RRA | ※1                                          |
| RTI | Retrun from Interrupt                        |
| RTS | Retrun from Subroutine                       |
| SAX | ※1                                          |
| SBC | Subtract with Carry                          |
| SBX | ※1                                          |
| SEC | Set Carry Flag                               |
| SED | Set Decimal Flag                             |
| SEI | Set Interrupt Disable Flag                   |
| SHA | ※1                                          |
| SHS | ※1                                          |
| SHX | ※1                                          |
| SHY | ※1                                          |
| SLO | ※1                                          |
| SRE | ※1                                          |
| STA | Store Accumulator In Memory                  |
| STX | Store X Index In Memory                      |
| STY | Store Y Index In Memory                      |
| TAX | Transfer Accumulator to X Index              |
| TAY | Transfer Accumulator to Y Index              |
| TSX | Transfer Stack Pointer to X Index            |
| TXA | Transfer X Index to Accumulator              |
| TXS | Transfer X Index to Stack Pointer            |
| TYA | Transfer Y Index to Accumulator              |

※1. 非公式命令のため不明
※2. 未定義命令？ 処理の強制停止



<a name="命令の仕様"></a>
### 命令の仕様
NES の CPU は数多くのアドレッシングモードと各命令を組み合わせることで多くの命令を定義されています。
アドレッシングモードについては後述します。
ひとまずメインとなる命令の仕様を記載するとします。

