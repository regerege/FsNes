--------------------------------------------------
# NESの仕様
--------------------------------------------------
大きく分けて３つに分けて記載していきます。（気が向けば、だけど！）  
基本的には NES で使われている 6502 をベースとした独自のCPUについての仕様をメインで記載するもとします。  

気が向いた時に CPU 以外の APU と PPU についても記載して行ければと考えていますが、あまり余力が無いので期待はしないようにお願いします。


1. [CPU 6502 の仕様](#CPU6502)
    1. [クロック数](#クロック数)
    2. [命令一覧](#命令一覧)
    3. [アドレッシングモード](#アドレッシングモード)
    4. [命令の仕様](#命令の仕様)
    5. [命令のマトリックス](#命令のマトリックス)
    6. [すべての命令の一覧表](#すべての命令の一覧表)



--------------------------------------------------
## CPU 6502 の仕様
--------------------------------------------------
NES の CPU は 6502 をベースに作られています。  
CPU の命令もかなり近しいものとなっていますが同じものではないことをご認識下さい。  


### クロック数
CPU のクロック数は NES に取り付けられている水晶振動子の12分周となっています。  
水晶振動子のクロック数をベースとした場合のクロック数は以下の通りとなります。  
水晶振動子のクロック数をマスタークロックと名付けます。  
※12分周 とはメインとなるクロック数から12回に1回のクロックを得ること、電子工作の基礎っぽいので分からない人はそちらも勉強してみると楽しいかと思います。

|       名称       |  クロック数           | 分周 |
|:-----------------|-----------------------|-----:|
| マスタークロック | 21477272           Hz |      |
| CPU              |  1789772.666666667 Hz |  12  |
| PPU              |  5369318           Hz |   4  |


### 命令一覧
非公式命令を含めると全部で 75 個 存在します。

|命令 | 名称                                         | 内容 |
|-----|----------------------------------------------|------|
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


### アドレッシングモード
メモリの読み書き先アドレスを決めるための方式のことを指します。  
  
Address: アドレスの計算方法  
Data: 読み込み または 書き込み方法  
角括弧はメモリ読み込みの意味  

| NO | Addressing Mode | Address                 | Data                      |
|---:|:----------------|:------------------------|:--------------------------|
| 01 | Absolute        | [PC+1]<<8&#x7C;[PC]     | [([PC+1]<<8&#x7C;[PC])]   |
| 02 | Absolute, X     | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] |
| 03 | Absolute, Y     | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] |
| 04 | Immediate       | PC                      | [PC]                      |
| 05 | Accumlator      | None                    | None                      |
| 06 | Implied         | None                    | None                      |
| 07 | Indirect        | None                    | None                      |
| 08 | Relative        | [PC]                    | None                      |
| 09 | Zero Page       | [PC]                    | [[PC]]                    |
| 10 | Zero Page, X    | [PC]+X                  | [[PC]+X]                  |
| 11 | Zero Page, Y    | [PC]+Y                  | [[PC]+Y]                  |
| 12 | (Indirect, X)   | [[PC]+X]                |  [[[PC]+X]]               |
| 13 | (Indirect), Y   | [[PC]]+Y                | [[[PC]]+Y]                |


### 命令の仕様
各命令事の仕様を一覧化  
フラグがどのように変化するかも記載  
細かなフラグ変化は Proccess に含めないものとする。  
{ } は上記アドレッシングモードで記載している Address または Data 列の取得方法に基づいて取得した値またはアドレスを指す。  

| Instraction | C | Z | I | D | B | B | V | N | Proccess                                                           |
|:-----------:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-------------------------------------------------------------------|
|     ADC     | C | C |   |   | 1 |   | C | C | A <- A + {data} + C                                                |
|     ANC     | C | C |   |   | 1 |   |   | C | A <- A & {data}                                                    |
|     AND     |   | C |   |   | 1 |   |   | C | A <- A & {data}                                                    |
|     ANE     |   |   |   |   | 1 |   |   |   | A <- (A &#x7C; 0xEE) & X & {data}                                  |
|     ARR     | C | C |   |   | 1 |   | C | C | A <- (A & {data}) >> 1 &#x7C; (C << 7)                             |
|     ASL     | C | C |   |   | 1 |   |   | C | [addr] <- {data} << 1                                              |
|     ASR     | C | C |   |   | 1 |   |   | C | A <- (A & {data}) >> 1                                             |
|     BCC     | * |   |   |   | 1 |   |   |   | PC <- if C = 0 then [PC] else PC+1                                 |
|     BCS     | * |   |   |   | 1 |   |   |   | PC <- if C = 1 then [PC] else PC+1                                 |
|     BEQ     |   | * |   |   | 1 |   |   |   | PC <- if Z = 1 then [PC] else PC+1                                 |
|     BIT     |   | C |   |   | 1 |   | C | C | N <- {data} >> 7 & 1, V <- {data} >> 6 & 1, Z <- ({data} & A) == 0 |
|     BMI     |   |   |   |   | 1 |   |   | * | PC <- if N = 1 then [PC] else PC+1                                 |
|     BNE     |   | * |   |   | 1 |   |   |   | PC <- if Z = 0 then [PC] else PC+1                                 |
|     BPL     |   |   |   |   | 1 |   |   | * | PC <- if N = 0 then [PC] else PC+1                                 |
|     BRK     | * | * | * | * | 1 | 1 | * | * | Force Break B=1,[PUSH]=[P],PC=[$FFFE]                              |
|     BVC     |   |   |   |   | 1 |   | * |   | PC <- if V = 0 then [PC] else PC+1                                 |
|     BVS     |   |   |   |   | 1 |   | * |   | PC <- if V = 1 then [PC] else PC+1                                 |
|     CLC     | 0 |   |   |   | 1 |   |   |   | C <- 0                                                             |
|     CLD     |   |   |   | 0 | 1 |   |   |   | D <- 0                                                             |
|     CLI     |   |   | 0 |   | 1 |   |   |   | I <- 0                                                             |
|     CLV     |   |   |   |   | 1 |   | 0 |   | V <- 0                                                             |
|     CMP     | C | C |   |   | 1 |   |   | C | A - {data}                                                         |
|     CPX     | C | C |   |   | 1 |   |   | C | X - {data}                                                         |
|     CPY     | C | C |   |   | 1 |   |   | C | Y - {data}                                                         |
|     DCP     | C | C |   |   | 1 |   |   | C | [addr] <- A - {data} - 1                                           |
|     DEC     |   | C |   |   | 1 |   |   | C | [addr] <- {data}+1                                                 |
|     DEX     |   | C |   |   | 1 |   |   | C | X <- X - 1                                                         |
|     DEY     |   | C |   |   | 1 |   |   | C | Y <- Y - 1                                                         |
|     EOR     |   | C |   |   | 1 |   |   | C | A <- A ^ {data}                                                    |
|     INC     |   | C |   |   | 1 |   |   | C | [addr] <- {data} + 1                                               |
|     INX     |   | C |   |   | 1 |   |   | C | X <- X + 1                                                         |
|     INY     |   | C |   |   | 1 |   |   | C | Y <- Y - 1                                                         |
|     ISB     | C | C |   |   | 1 |   | C | C | [addr] <- {data} + 1, SBC([addr])                                  |
|     JMP     |   |   |   |   | 1 |   |   |   | PC <- {data}                                                       |
|     JSR     |   |   |   |   | 1 |   |   |   | PC <- {data}                                                       |
|     KIL     |   |   |   |   | 1 |   |   |   |                                                                    |
|     LAS     |   | C |   |   | 1 |   |   | C | S <- S & {data}, X <- S, A <- S                                    |
|     LAX     |   | C |   |   | 1 |   |   | C | A <- {data}, X <- {data}                                           |
|     LDA     |   | C |   |   | 1 |   |   | C | A <- A &#x7C; {data}                                               |
|     LDX     |   | C |   |   | 1 |   |   | C | X <- {data}                                                        |
|     LDY     |   | C |   |   | 1 |   |   | C | Y <- {data}                                                        |
|     LSR     | C | C |   |   | 1 |   |   | C | C <- A & 1, A <- A >> 1                                            |
|     NOP     |   |   |   |   | 1 |   |   |   |                                                                    |
|     ORA     |   | C |   |   | 1 |   |   | C | A <- A &#x7C; {data}                                               |
|     PHA     |   |   |   |   | 1 |   |   |   | PUSH A                                                             |
|     PHP     | * | * | * | * | 1 | * | * | * | PUSH P                                                             |
|     PLA     |   |   |   |   | 1 |   |   |   | A <- POP                                                           |
|     PLP     | C | C | C | C | 1 | C | C | C | P <- POP                                                           |
|     RLA     | C | C |   |   | 1 |   |   | C | A <- A & ({data} << 1 &#x7C; C)                                    |
|     ROL     | C | C |   |   | 1 |   |   | C | C <- {data}>>7, [addr] <- {data}<<1 &#x7C; C                       |
|     ROR     | C | C |   |   | 1 |   |   | C | C <- {data}&1, [addr] <- {data}>>1 &#x7C; C<<7                     |
|     RRA     | C | C |   |   | 1 |   | C | C | [addr] <- ({data} >> 1 &#x7C; C << 7), ADC([addr])                 |
|     RTI     | C | C | C | C | 1 | C | C | C | P <- POP, PC <- POP &#x7C; POP << 8                                |
|     RTS     |   |   |   |   | 1 |   |   |   | PC <- (POP &#x7C; POP << 8) + 1                                    |
|     SAX     |   |   |   |   | 1 |   |   |   | [addr] <- A & X                                                    |
|     SBC     | C | C |   |   | 1 |   | C | C | ADC({data})                                                        |
|     SBX     | C | C |   |   | 1 |   |   | C | X <- (A & X) - {data}                                              |
|     SEC     | 1 |   |   |   | 1 |   |   |   | C <- 1                                                             |
|     SED     |   |   |   | 1 | 1 |   |   |   | D <- 1                                                             |
|     SEI     |   |   | 1 |   | 1 |   |   |   | I <- 1                                                             |
|     SHA     |   |   |   |   | 1 |   |   |   | [addr] <- A & X & ((addr>>8)+1)                                    |
|     SHS     |   |   |   |   | 1 |   |   |   | S <- A & X, [addr] <- S & ((addr>>8)+1)                            |
|     SHX     |   |   |   |   | 1 |   |   |   | [addr] <- X & ((addr>>8)+1)                                        |
|     SHY     |   |   |   |   | 1 |   |   |   | [addr] <- Y & ((addr>>8)+1)                                        |
|     SLO     | C | C |   |   | 1 |   |   | C | [addr] <- {data} << 1, A <- A & {data}                             |
|     SRE     | C | C |   |   | 1 |   |   | C | [addr] <- {data} >> 1, A <- A ^ {data}                             |
|     STA     |   |   |   |   | 1 |   |   |   | [addr] <- A                                                        |
|     STX     |   |   |   |   | 1 |   |   |   | [addr] <- X                                                        |
|     STY     |   |   |   |   | 1 |   |   |   | [addr] <- Y                                                        |
|     TAX     |   | C |   |   | 1 |   |   | C | X <- A                                                             |
|     TAY     |   | C |   |   | 1 |   |   | C | Y <- A                                                             |
|     TSX     |   | C |   |   | 1 |   |   | C | X <- S                                                             |
|     TXA     |   | C |   |   | 1 |   |   | C | A <- X                                                             |
|     TXS     |   | C |   |   | 1 |   |   | C | S <- X                                                             |
|     TYA     |   | C |   |   | 1 |   |   | C | A <- Y                                                             |


### 命令のマトリックス
命令とアドレッシングモードの組み合わせを表に Read/Write を記載する。  
 C: Const Value (固定値)  
 M: Memory  
 R: Register  
 X -> Y: X は読み込み元、Y は書き込み先  
 Reset: リセット処理  
 Branch: 分岐処理、実態はレジスタの PC に分岐先アドレスを書き込む (M -> R と同義)
 PUSH: スタックに積む。実態はメモリとレジスタを書き換える。(メモリに追加後、スタックポインタを進ませる。)
 POP: スタックから卸す。実態はPUSHの逆
 NOP: 処理は行わない。サイクル処理のみ行う。(指定サイクル数分CPUの処理をスキップさせるだけ)

|     | Implied | Accumlator | Immediate | Zero Page | Zero Page, X | Zero Page, Y | Absolute | Absolute, X | Absolute, Y | (Indirect), Y | (Indirect, X) | Relative | Indirect |
|:---:|:-------:|:----------:|:---------:|:---------:|:------------:|:------------:|:--------:|:-----------:|:-----------:|:-------------:|:-------------:|:--------:|:--------:|
|     |    1    |     1      |     2     |     2     |      2       |      2       |    3     |      3      |      3      |       2       |       2       |    2     |    3     |
| ADC |         |            |  M -> R   |  M -> R   |    M -> R    |              |  M -> R  |   M -> R    |   M -> R    |    M -> R     |    M -> R     |          |          |
| ANC |         |            |  M -> R   |           |              |              |          |             |             |               |               |          |          |
| AND |         |            |  M -> R   |  M -> R   |    M -> R    |              |  M -> R  |   M -> R    |   M -> R    |    M -> R     |    M -> R     |          |          |
| ANE |         |            |  M -> R   |           |              |              |          |             |             |               |               |          |          |
| ARR |         |            |  M -> R   |           |              |              |          |             |             |               |               |          |          |
| ASL |         |   R -> R   |           |  M -> M   |    M -> M    |              |  M -> M  |   M -> M    |             |               |               |          |          |
| ASR |         |            |  M -> R   |           |              |              |          |             |             |               |               |          |          |
| BCC |         |            |           |           |              |              |          |             |             |               |               |  Branch  |          |
| BCS |         |            |           |           |              |              |          |             |             |               |               |  Branch  |          |
| BEQ |         |            |           |           |              |              |          |             |             |               |               |  Branch  |          |
| BIT |         |            |           |  M -> R   |              |              |  M -> R  |             |             |               |               |          |          |
| BMI |         |            |           |           |              |              |          |             |             |               |               |  Branch  |          |
| BNE |         |            |           |           |              |              |          |             |             |               |               |  Branch  |          |
| BPL |         |            |           |           |              |              |          |             |             |               |               |  Branch  |          |
| BRK |  Reset  |            |           |           |              |              |          |             |             |               |               |          |          |
| BVC |         |            |           |           |              |              |          |             |             |               |               |  Branch  |          |
| BVS |         |            |           |           |              |              |          |             |             |               |               |  Branch  |          |
| CLC | C -> R  |            |           |           |              |              |          |             |             |               |               |          |          |
| CLD | C -> R  |            |           |           |              |              |          |             |             |               |               |          |          |
| CLI | C -> R  |            |           |           |              |              |          |             |             |               |               |          |          |
| CLV | C -> R  |            |           |           |              |              |          |             |             |               |               |          |          |
| CMP |         |            |  M -> R   |  M -> R   |    M -> R    |              |  M -> R  |   M -> R    |   M -> R    |    M -> R     |    M -> R     |          |          |
| CPX |         |            |  M -> R   |  M -> R   |              |              |  M -> R  |             |             |               |               |          |          |
| CPY |         |            |  M -> R   |  M -> R   |              |              |  M -> R  |             |             |               |               |          |          |
| DCP |         |            |           |  M -> M   |    M -> M    |              |  M -> M  |   M -> M    |   M -> M    |    M -> M     |    M -> M     |          |          |
| DEC |         |            |           |  M -> M   |    M -> M    |              |  M -> M  |   M -> M    |             |               |               |          |          |
| DEX | R -> R  |            |           |           |              |              |          |             |             |               |               |          |          |
| DEY | R -> R  |            |           |           |              |              |          |             |             |               |               |          |          |
| EOR |         |            |  M -> R   |  M -> R   |    M -> R    |              |  M -> R  |   M -> R    |   M -> R    |    M -> R     |    M -> R     |          |          |
| INC |         |            |           |  M -> M   |    M -> M    |              |  M -> M  |   M -> M    |             |               |               |          |          |
| INX | R -> R  |            |           |           |              |              |          |             |             |               |               |          |          |
| INY | R -> R  |            |           |           |              |              |          |             |             |               |               |          |          |
| ISB |         |            |           |  M -> MR  |    M -> MR   |              |  M -> MR |   M -> MR   |   M -> MR   |    M -> MR    |    M -> MR    |          |          |
| JMP |         |            |           |           |              |              |  M -> R  |             |             |               |               |          |   JMP    |
| JSR |         |            |           |           |              |              |  M -> R  |             |             |               |               |          |          |
| KIL |         |            |           |           |              |              |          |             |             |               |               |          |          |
| LAS |         |            |           |           |              |              |          |             |   M -> MR   |               |               |          |          |
| LAX |         |            |  M -> R   |  M -> R   |              |    M -> R    |  M -> R  |             |   M -> R    |    M -> R     |    M -> R     |          |          |
| LDA |         |            |  M -> R   |  M -> R   |    M -> R    |              |  M -> R  |   M -> R    |   M -> R    |    M -> R     |    M -> R     |          |          |
| LDX |         |            |  M -> R   |  M -> R   |              |    M -> R    |  M -> R  |             |   M -> R    |               |               |          |          |
| LDY |         |            |  M -> R   |  M -> R   |    M -> R    |              |  M -> R  |   M -> R    |             |               |               |          |          |
| LSR |         |   R -> R   |           |  M -> M   |    M -> M    |              |  M -> M  |   M -> M    |             |               |               |          |          |
| NOP |  NOP    |            |    NOP    |    NOP    |     NOP      |              |   NOP    |     NOP     |             |               |               |          |          |
| ORA |         |            |  M -> R   |  M -> R   |    M -> R    |              |  M -> R  |   M -> R    |   M -> R    |    M -> R     |    M -> R     |          |          |
| PHA |  PUSH   |            |           |           |              |              |          |             |             |               |               |          |          |
| PHP |  PUSH   |            |           |           |              |              |          |             |             |               |               |          |          |
| PLA |  POP    |            |           |           |              |              |          |             |             |               |               |          |          |
| PLP |  POP    |            |           |           |              |              |          |             |             |               |               |          |          |
| RLA |         |            |           |  M -> R   |    M -> R    |              |  M -> R  |   M -> R    |   M -> R    |    M -> R     |    M -> R     |          |          |
| ROL |         |   R -> R   |           |  M -> M   |    M -> M    |              |  M -> M  |   M -> M    |             |               |               |          |          |
| ROR |         |   R -> R   |           |  M -> M   |    M -> M    |              |  M -> M  |   M -> M    |             |               |               |          |          |
| RRA |         |            |           |  M -> M   |    M -> M    |              |  M -> M  |   M -> M    |   M -> M    |    M -> M     |    M -> M     |          |          |
| RTI |  POP    |            |           |           |              |              |          |             |             |               |               |          |          |
| RTS |  POP    |            |           |           |              |              |          |             |             |               |               |          |          |
| SAX |         |            |           |  R -> M   |              |    R -> M    |  R -> M  |             |             |               |    R -> M     |          |          |
| SBC |         |            |  M -> R   |  M -> R   |    M -> R    |              |  M -> R  |   M -> R    |   M -> R    |    M -> R     |    M -> R     |          |          |
| SBX |         |            |  M -> R   |           |              |              |          |             |             |               |               |          |          |
| SEC | C -> R  |            |           |           |              |              |          |             |             |               |               |          |          |
| SED | C -> R  |            |           |           |              |              |          |             |             |               |               |          |          |
| SEI | C -> R  |            |           |           |              |              |          |             |             |               |               |          |          |
| SHA |         |            |           |           |              |              |          |             |   A -> M    |    A -> M     |               |          |          |
| SHS |         |            |           |           |              |              |          |             |   M -> M    |               |               |          |          |
| SHX |         |            |           |           |              |              |          |             |   A -> M    |               |               |          |          |
| SHY |         |            |           |           |              |              |          |   A -> M    |             |               |               |          |          |
| SLO |         |            |           |  M -> MR  |    M -> MR   |              |  M -> MR |   M -> MR   |   M -> MR   |    M -> MR    |    M -> MR    |          |          |
| SRE |         |            |           |  M -> MR  |    M -> MR   |              |  M -> MR |   M -> MR   |   M -> MR   |    M -> MR    |    M -> MR    |          |          |
| STA |         |            |           |  R -> M   |    R -> M    |              |  R -> M  |   R -> M    |   R -> M    |    R -> M     |    R -> M     |          |          |
| STX |         |            |           |  R -> M   |              |    R -> M    |  R -> M  |             |             |               |               |          |          |
| STY |         |            |           |  R -> M   |    R -> M    |              |  R -> M  |             |             |               |               |          |          |
| TAX | R -> R  |            |           |           |              |              |          |             |             |               |               |          |          |
| TAY | R -> R  |            |           |           |              |              |          |             |             |               |               |          |          |
| TSX | R -> R  |            |           |           |              |              |          |             |             |               |               |          |          |
| TXA | R -> R  |            |           |           |              |              |          |             |             |               |               |          |          |
| TXS | R -> R  |            |           |           |              |              |          |             |             |               |               |          |          |
| TYA | R -> R  |            |           |           |              |              |          |             |             |               |               |          |          |


### すべての命令の一覧表
CPU の命令定義の一覧  
かなりの数があるので見づらいです。

| Offset | Instraction | Addressing Mode | Bytes | Cycles    | Offical | Unoffical | Illegal | Memory  | A | X | Y | PC | S  | C | Z | I | D | B | B | V | N | Example     | Address                 | Data                      | Proccess                                                           |
|:------:|:-----------:|:----------------|------:|----------:|:-------:|:---------:|:-------:|:-------:|:-:|:-:|:-:|:--:|:--:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:------------|:------------------------|:--------------------------|:-------------------------------------------------------------------|
| 00     | BRK         | Implied         |   1   |     7     |    *    |           |         |    *    |   |   |   | *  | ++ | * | * | * | * | 1 | 1 | * | * | BRK         | None                    | None                      | Force Break B=1,[PUSH]=[P],PC=[$FFFE]                              |
| 01     | ORA         | (Indirect, X)   |   2   |     6     |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | ORA ($44,X) | [[PC]+X]                | [[[PC]+X]]                | A <- A &#x7C; {data}                                               |
| 02     | KIL         |                 |   1   | Exception |         |           |    *    |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   |             | None                    | None                      |                                                                    |
| 03     | SLO         | (Indirect, X)   |   2   |     8     |         |     *     |         |    C    | C | * |   | *  |    | C | C |   |   | 1 |   |   | C | SLO ($44,X) | [[PC]+X]                | [[[PC]+X]]                | [addr] <- {data} << 1, A <- A & {data}                             |
| 04     | NOP         | Zero Page       |   2   |     3     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP $44     | [PC]                    | [[PC]]                    |                                                                    |
| 05     | ORA         | Zero Page       |   2   |     3     |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | ORA $44     | [PC]                    | [[PC]]                    | A <- A &#x7C; {data}                                               |
| 06     | ASL         | Zero Page       |   2   |     5     |    *    |           |         |    C    |   |   |   |    |    | C | C |   |   | 1 |   |   | C | ASL $44     | [PC]                    | [[PC]]                    | [addr] <- [addr] << 1                                              |
| 07     | SLO         | Zero Page       |   2   |     5     |         |     *     |         |    C    | C |   |   | *  |    | C | C |   |   | 1 |   |   | C | SLO $44     | [PC]                    | [[PC]]                    | [addr] <- {data} << 1, A <- A & {data}                             |
| 08     | PHP         | Implied         |   1   |     3     |    *    |           |         |    *    |   |   |   |    | +  | * | * | * | * | 1 | * | * | * | PHP         | None                    | None                      | PUSH P                                                             |
| 09     | ORA         | Immediate       |   2   |     2     |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | ORA #$44    | PC                      | [PC]                      | A <- A &#x7C; {data}                                               |
| 0A     | ASL         | Accumlator      |   1   |     2     |    *    |           |         |         | C |   |   |    |    | C | C |   |   | 1 |   |   | C | ASL A       | None                    | None                      | A <- A << 1                                                        |
| 0B     | ANC         | Immediate       |   2   |     2     |         |     *     |         |    *    | C |   |   | *  |    | C | C |   |   | 1 |   |   | C | ANC #$44    | PC                      | [PC]                      | A <- A & {data}                                                    |
| 0C     | NOP         | Absolute        |   3   |     4     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   |                                                                    |
| 0D     | ORA         | Absolute        |   3   |     4     |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | ORA $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | A <- A &#x7C; {data}                                               |
| 0E     | ASL         | Absolute        |   3   |     6     |    *    |           |         |    C    |   |   |   |    |    | C | C |   |   | 1 |   |   | C | ASL $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | [addr] <- [addr] << 1                                              |
| 0F     | SLO         | Absolute        |   3   |     6     |         |     *     |         |    C    | C |   |   | *  |    | C | C |   |   | 1 |   |   | C | SLO $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | [addr] <- {data} << 1, A <- A & {data}                             |
| 10     | BPL         | Relative        |   2   |     2++   |    *    |           |         |    *    |   |   |   | C  |    |   |   |   |   | 1 |   |   | * | BPL $44     | [PC]                    | None                      | PC <- if N = 0 then [PC] else PC+1                                 |
| 11     | ORA         | (Indirect), Y   |   2   |     5+    |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | ORA ($44),Y | [[PC]]+Y                | [[[PC]]+Y]                | A <- A &#x7C; {data}                                               |
| 12     | KIL         |                 |   1   | Exception |         |           |    *    |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   |             | None                    | None                      |                                                                    |
| 13     | SLO         | (Indirect), Y   |   2   |     8     |         |     *     |         |    C    | C |   | * | *  |    | C | C |   |   | 1 |   |   | C | SLO ($44),Y | [[PC]]+Y                | [[[PC]]+Y]                | [addr] <- {data} << 1, A <- A & {data}                             |
| 14     | NOP         | Zero Page, X    |   2   |     4     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP $44,X   | [PC]+X                  | [[[PC]+X]]                |                                                                    |
| 15     | ORA         | Zero Page, X    |   2   |     4     |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | ORA $44,X   | [PC]+X                  | [[[PC]+X]]                | A <- A &#x7C; {data}                                               |
| 16     | ASL         | Zero Page, X    |   2   |     6     |    *    |           |         |    C    |   | * |   |    |    | C | C |   |   | 1 |   |   | C | ASL $44,X   | [PC]+X                  | [[[PC]+X]]                | [addr] <- [addr] << 1                                              |
| 17     | SLO         | Zero Page, X    |   2   |     6     |         |     *     |         |    C    | C | * |   | *  |    | C | C |   |   | 1 |   |   | C | SLO $44,X   | [PC]+X                  | [[[PC]+X]]                | [addr] <- {data} << 1, A <- A & {data}                             |
| 18     | CLC         | Implied         |   1   |     2     |    *    |           |         |         |   |   |   |    |    | 0 |   |   |   | 1 |   |   |   | CLC         | None                    | None                      | C <- 0                                                             |
| 19     | ORA         | Absolute, Y     |   3   |     4+    |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | ORA $4400,Y | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] | A <- A &#x7C; {data}                                               |
| 1A     | NOP         | Implied         |   1   |     2     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   |             | None                    | None                      |                                                                    |
| 1B     | SLO         | Absolute, Y     |   3   |     7     |         |     *     |         |    C    | C |   | * | *  |    | C | C |   |   | 1 |   |   | C | SLO $4400,Y | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] | [addr] <- {data} << 1, A <- A & {data}                             |
| 1C     | NOP         | Absolute, X     |   3   |     4+    |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] |                                                                    |
| 1D     | ORA         | Absolute, X     |   3   |     4+    |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | ORA $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | A <- A &#x7C; {data}                                               |
| 1E     | ASL         | Absolute, X     |   3   |     6+    |    *    |           |         |    C    |   | * |   |    |    | C | C |   |   | 1 |   |   | C | ASL $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | [addr] <- [addr] << 1                                              |
| 1F     | SLO         | Absolute, X     |   3   |     7     |         |     *     |         |    C    | C | * |   | *  |    | C | C |   |   | 1 |   |   | C | SLO $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | [addr] <- {data} << 1, A <- A & {data}                             |
| 20     | JSR         | Absolute        |   3   |     6     |    *    |           |         |    *    |   |   |   | C  | +  |   |   |   |   | 1 |   |   |   | JSR $5597   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | PC <- {data}                                                       |
| 21     | AND         | (Indirect, X)   |   2   |     6     |    *    |           |         |    *    | C | * |   | *  |    |   | C |   |   | 1 |   |   | C | AND ($44,X) | [[PC]+X]                | [[[PC]+X]]                | A <- A & [[[PC]+X]]                                                |
| 22     | KIL         |                 |   1   | Exception |         |           |    *    |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   |             | None                    | None                      |                                                                    |
| 23     | RLA         | (Indirect, X)   |   2   |     8     |         |     *     |         |    C    | C | * |   | *  |    | C | C |   |   | 1 |   |   | C | RLA ($44,X) | [[PC]+X]                | [[[PC]+X]]                | A <- A & ({data} << 1 &#x7C; C)                                    |
| 24     | BIT         | Zero Page       |   2   |     3     |    *    |           |         |    *    | * |   |   | *  |    |   | C |   |   | 1 |   | C | C | BIT $44     | [PC]                    | [[PC]]                    | N <- {data} >> 7 & 1, V <- {data} >> 6 & 1, Z <- ({data} & A) == 0 |
| 25     | AND         | Zero Page       |   2   |     3     |    *    |           |         |    *    | C |   |   | *  |    |   | C |   |   | 1 |   |   | C | AND $44     | [PC]                    | [[PC]]                    | A <- A & [[PC]]                                                    |
| 26     | ROL         | Zero Page       |   2   |     5     |    *    |           |         |    C    |   |   |   |    |    | C | C |   |   | 1 |   |   | C | ROL $44     | [PC]                    | [[PC]]                    | C <- {data}>>7, [addr] <- {data}<<1 &#x7C; C                       |
| 27     | RLA         | Zero Page       |   2   |     5     |         |     *     |         |    C    | C |   |   | *  |    | C | C |   |   | 1 |   |   | C | RLA $44     | [PC]                    | [[PC]]                    | A <- A & ({data} << 1 &#x7C; C)                                    |
| 28     | PLP         | Implied         |   1   |     4     |    *    |           |         |    *    |   |   |   |    | -  | C | C | C | C | 1 | C | C | C | PLP         | None                    | None                      | P <- POP                                                           |
| 29     | AND         | Immediate       |   2   |     2     |    *    |           |         |    *    | C |   |   | *  |    |   | C |   |   | 1 |   |   | C | AND #$44    | PC                      | [PC]                      | A <- A & [PC]                                                      |
| 2A     | ROL         | Accumlator      |   1   |     2     |    *    |           |         |         | C |   |   |    |    | C | C |   |   | 1 |   |   | C | ROL A       | None                    | None                      | C <- A>>7, A <- A<<1 &#x7C; C                                      |
| 2B     | ANC         | Immediate       |   2   |     2     |         |     *     |         |    *    | C |   |   | *  |    | C | C |   |   | 1 |   |   | C | ANC #$44    | PC                      | [PC]                      | A <- A & {data}                                                    |
| 2C     | BIT         | Absolute        |   3   |     4     |    *    |           |         |    *    | * |   |   | *  |    |   | C |   |   | 1 |   | C | C | BIT $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | N <- {data} >> 7 & 1, V <- {data} >> 6 & 1, Z <- ({data} & A) == 0 |
| 2D     | AND         | Absolute        |   3   |     4     |    *    |           |         |    *    | C |   |   | *  |    |   | C |   |   | 1 |   |   | C | AND $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | A <- A & {data}                                                    |
| 2E     | ROL         | Absolute        |   3   |     6     |    *    |           |         |    C    |   |   |   |    |    | C | C |   |   | 1 |   |   | C | ROL $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | C <- {data}>>7, [addr] <- {data}<<1 &#x7C; C                       |
| 2F     | RLA         | Absolute        |   3   |     6     |         |     *     |         |    C    | C |   |   | *  |    | C | C |   |   | 1 |   |   | C | RLA $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | A <- A & ({data} << 1 &#x7C; C)                                    |
| 30     | BMI         | Relative        |   2   |     2++   |    *    |           |         |    *    |   |   |   | C  |    |   |   |   |   | 1 |   |   | * | BMI $44     | [PC]                    | None                      | PC <- if N = 1 then [PC] else PC+1                                 |
| 31     | AND         | (Indirect), Y   |   2   |     5+    |    *    |           |         |    *    | C |   | * | *  |    |   | C |   |   | 1 |   |   | C | AND ($44),Y | [[PC]]+Y                | [[[PC]]+Y]                | A <- A & [[[PC]]+Y]                                                |
| 32     | KIL         |                 |   1   | Exception |         |           |    *    |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   |             | None                    | None                      |                                                                    |
| 33     | RLA         | (Indirect), Y   |   2   |     8     |         |     *     |         |    C    | C |   | * | *  |    | C | C |   |   | 1 |   |   | C | RLA ($44),Y | [[PC]]+Y                | [[[PC]]+Y]                | A <- A & ({data} << 1 &#x7C; C)                                    |
| 34     | NOP         | Zero Page, X    |   2   |     4     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP $44,X   | [PC]+X                  | [[[PC]+X]]                |                                                                    |
| 35     | AND         | Zero Page, X    |   2   |     4     |    *    |           |         |    *    | C | * |   | *  |    |   | C |   |   | 1 |   |   | C | AND $44,X   | [PC]+X                  | [[[PC]+X]]                | A <- A & [[PC]+X]                                                  |
| 36     | ROL         | Zero Page, X    |   2   |     6     |    *    |           |         |    C    |   | * |   |    |    | C | C |   |   | 1 |   |   | C | ROL $44,X   | [PC]+X                  | [[[PC]+X]]                | C <- {data}>>7, [addr] <- {data}<<1 &#x7C; C                       |
| 37     | RLA         | Zero Page, X    |   2   |     6     |         |     *     |         |    C    | C | * |   | *  |    | C | C |   |   | 1 |   |   | C | RLA $44,X   | [PC]+X                  | [[[PC]+X]]                | A <- A & ({data} << 1 &#x7C; C)                                    |
| 38     | SEC         | Implied         |   1   |     2     |    *    |           |         |         |   |   |   |    |    | 1 |   |   |   | 1 |   |   |   | SEC         | None                    | None                      | C <- 1                                                             |
| 39     | AND         | Absolute, Y     |   3   |     4+    |    *    |           |         |    *    | C |   | * | *  |    |   | C |   |   | 1 |   |   | C | AND $4400,Y | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] | A <- A & [([PC+1]<<8&#x7C;[PC])+Y]                                 |
| 3A     | NOP         | Implied         |   1   |     2     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP         | None                    | None                      |                                                                    |
| 3B     | RLA         | Absolute, Y     |   3   |     7     |         |     *     |         |    C    | C |   | * | *  |    | C | C |   |   | 1 |   |   | C | RLA $4400,Y | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] | A <- A & ({data} << 1 &#x7C; C)                                    |
| 3C     | NOP         | Absolute, X     |   3   |     4+    |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] |                                                                    |
| 3D     | AND         | Absolute, X     |   3   |     4+    |    *    |           |         |    *    | C | * |   | *  |    |   | C |   |   | 1 |   |   | C | AND $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | A <- A & [([PC+1]<<8&#x7C;[PC])+X]                                 |
| 3E     | ROL         | Absolute, X     |   3   |     7     |    *    |           |         |    C    |   | * |   |    |    | C | C |   |   | 1 |   |   | C | ROL $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | C <- {data}>>7, [addr] <- {data}<<1 &#x7C; C                       |
| 3F     | RLA         | Absolute, X     |   3   |     7     |         |     *     |         |    C    | C | * |   | *  |    | C | C |   |   | 1 |   |   | C | RLA $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | A <- A & ({data} << 1 &#x7C; C)                                    |
| 40     | RTI         | Implied         |   1   |     6     |    *    |           |         |    C    |   |   |   | C  | -  | C | C | C | C | 1 | C | C | C | RTI         | None                    | None                      | P <- POP, PC <- POP &#x7C; POP << 8                                |
| 41     | EOR         | (Indirect, X)   |   2   |     6     |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | EOR ($44,X) | [[PC]+X]                | [[[PC]+X]]                | A <- A ^ {data}                                                    |
| 42     | KIL         |                 |   1   | Exception |         |           |    *    |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   |             | None                    | None                      |                                                                    |
| 43     | SRE         | (Indirect, X)   |   2   |     8     |         |     *     |         |    C    | C | * |   | *  |    | C | C |   |   | 1 |   |   | C | SRE ($44,X) | [[PC]+X]                | [[[PC]+X]]                | [addr] <- {data} >> 1, A <- A ^ {data}                             |
| 44     | NOP         | Zero Page       |   2   |     3     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP $44     | [PC]                    | [[PC]]                    |                                                                    |
| 45     | EOR         | Zero Page       |   2   |     3     |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | EOR $44     | [PC]                    | [[PC]]                    | A <- A ^ {data}                                                    |
| 46     | LSR         | Zero Page       |   2   |     5     |    *    |           |         |    C    |   |   |   |    |    | C | C |   |   | 1 |   |   | C | LSR $44     | [PC]                    | [[PC]]                    | C <- A & 1, A <- A >> 1                                            |
| 47     | SRE         | Zero Page       |   2   |     5     |         |     *     |         |    C    | C |   |   | *  |    | C | C |   |   | 1 |   |   | C | SRE $44     | [PC]                    | [[PC]]                    | [addr] <- {data} >> 1, A <- A ^ {data}                             |
| 48     | PHA         | Implied         |   1   |     3     |    *    |           |         |    *    | * |   |   |    | +  |   |   |   |   | 1 |   |   |   | PHA         | None                    | None                      | PUSH A                                                             |
| 49     | EOR         | Immediate       |   2   |     2     |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | EOR #$44    | PC                      | [PC]                      | A <- A ^ {data}                                                    |
| 4A     | LSR         | Accumlator      |   1   |     2     |    *    |           |         |         | C |   |   |    |    | C | C |   |   | 1 |   |   | C | LSR A       | None                    | None                      | C <- A & 1, A <- A >> 1                                            |
| 4B     | ASR         | Immediate       |   2   |     2     |         |     *     |         |    *    | C |   |   |    |    | C | C |   |   | 1 |   |   | C | ASR #$44    | PC                      | [PC]                      | A <- (A & {data}) >> 1                                             |
| 4C     | JMP         | Absolute        |   3   |     3     |    *    |           |         |    *    |   |   |   | C  |    |   |   |   |   | 1 |   |   |   | JMP $5597   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | PC <- {data}                                                       |
| 4D     | EOR         | Absolute        |   3   |     4     |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | EOR $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | A <- A ^ {data}                                                    |
| 4E     | LSR         | Absolute        |   3   |     6     |    *    |           |         |    C    |   |   |   |    |    | C | C |   |   | 1 |   |   | C | LSR $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | C <- A & 1, A <- A >> 1                                            |
| 4F     | SRE         | Absolute        |   3   |     6     |         |     *     |         |    C    | C |   |   | *  |    | C | C |   |   | 1 |   |   | C | SRE $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | [addr] <- {data} >> 1, A <- A ^ {data}                             |
| 50     | BVC         | Relative        |   2   |     2++   |    *    |           |         |    *    |   |   |   | C  |    |   |   |   |   | 1 |   | * |   | BVC $44     | [PC]                    | None                      | PC <- if V = 0 then [PC] else PC+1                                 |
| 51     | EOR         | (Indirect), Y   |   2   |     5+    |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | EOR ($44),Y | [[PC]]+Y                | [[[PC]]+Y]                | A <- A ^ {data}                                                    |
| 52     | KIL         |                 |   1   | Exception |         |           |    *    |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   |             | None                    | None                      |                                                                    |
| 53     | SRE         | (Indirect), Y   |   2   |     8     |         |     *     |         |    C    | C |   | * | *  |    | C | C |   |   | 1 |   |   | C | SRE ($44),Y | [[PC]]+Y                | [[[PC]]+Y]                | [addr] <- {data} >> 1, A <- A ^ {data}                             |
| 54     | NOP         | Zero Page, X    |   2   |     4     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP $44,X   | [PC]+X                  | [[[PC]+X]]                |                                                                    |
| 55     | EOR         | Zero Page, X    |   2   |     4     |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | EOR $44,X   | [PC]+X                  | [[[PC]+X]]                | A <- A ^ {data}                                                    |
| 56     | LSR         | Zero Page, X    |   2   |     6     |    *    |           |         |    C    |   |   |   |    |    | C | C |   |   | 1 |   |   | C | LSR $44,X   | [PC]+X                  | [[[PC]+X]]                | C <- A & 1, A <- A >> 1                                            |
| 57     | SRE         | Zero Page, X    |   2   |     6     |         |     *     |         |    C    | C | * |   | *  |    | C | C |   |   | 1 |   |   | C | SRE $44,X   | [PC]+X                  | [[[PC]+X]]                | [addr] <- {data} >> 1, A <- A ^ {data}                             |
| 58     | CLI         | Implied         |   1   |     2     |    *    |           |         |         |   |   |   |    |    |   |   | 0 |   | 1 |   |   |   | CLI         | None                    | None                      | I <- 0                                                             |
| 59     | EOR         | Absolute, Y     |   3   |     4+    |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | EOR $4400,Y | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] | A <- A ^ {data}                                                    |
| 5A     | NOP         | Implied         |   1   |     2     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP         | None                    | None                      |                                                                    |
| 5B     | SRE         | Absolute, Y     |   3   |     7     |         |     *     |         |    C    | C |   | * | *  |    | C | C |   |   | 1 |   |   | C | SRE $4400,Y | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] | [addr] <- {data} >> 1, A <- A ^ {data}                             |
| 5C     | NOP         | Absolute, X     |   3   |     4+    |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] |                                                                    |
| 5D     | EOR         | Absolute, X     |   3   |     4+    |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | EOR $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | A <- A ^ {data}                                                    |
| 5E     | LSR         | Absolute, X     |   3   |     7     |    *    |           |         |    C    |   |   |   |    |    | C | C |   |   | 1 |   |   | C | LSR $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | C <- A & 1, A <- A >> 1                                            |
| 5F     | SRE         | Absolute, X     |   3   |     7     |         |     *     |         |    C    | C | * |   | *  |    | C | C |   |   | 1 |   |   | C | SRE $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | [addr] <- {data} >> 1, A <- A ^ {data}                             |
| 60     | RTS         | Implied         |   1   |     6     |    *    |           |         |    C    |   |   |   | C  | -  |   |   |   |   | 1 |   |   |   | RTS         | None                    | None                      | PC <- (POP &#x7C; POP << 8) + 1                                    |
| 61     | ADC         | (Indirect, X)   |   2   |     6     |    *    |           |         |    *    | C | * |   | *  |    | C | C |   |   | 1 |   | C | C | ADC ($44,X) | [[PC]+X]                | [[[PC]+X]]                | A <- A + {data} + C                                                |
| 62     | KIL         |                 |   1   | Exception |         |           |    *    |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   |             | None                    | None                      |                                                                    |
| 63     | RRA         | (Indirect, X)   |   2   |     8     |         |     *     |         |    C    | C | * |   | *  |    | C | C |   |   | 1 |   | C | C | RRA ($44,X) | [[PC]+X]                | [[[PC]+X]]                | [addr] <- ({data} >> 1 &#x7C; C << 7), ADC([addr])                 |
| 64     | NOP         | Zero Page       |   2   |     3     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP $44     | [PC]                    | [[PC]]                    |                                                                    |
| 65     | ADC         | Zero Page       |   2   |     3     |    *    |           |         |    *    | C |   |   | *  |    | C | C |   |   | 1 |   | C | C | ADC $44     | [PC]                    | [[PC]]                    | A <- A + {data} + C                                                |
| 66     | ROR         | Zero Page       |   2   |     5     |    *    |           |         |    C    |   |   |   |    |    | C | C |   |   | 1 |   |   | C | ROR $44     | [PC]                    | [[PC]]                    | C <- {data}&1, [addr] <- {data}>>1 &#x7C; C<<7                     |
| 67     | RRA         | Zero Page       |   2   |     5     |         |     *     |         |    C    | C |   |   | *  |    | C | C |   |   | 1 |   | C | C | RRA $44     | [PC]                    | [[PC]]                    | [addr] <- ({data} >> 1 &#x7C; C << 7), ADC([addr])                 |
| 68     | PLA         | Implied         |   1   |     4     |    *    |           |         |    *    | C |   |   |    | -  |   |   |   |   | 1 |   |   |   | PLA         | None                    | None                      | A <- POP                                                           |
| 69     | ADC         | Immediate       |   2   |     2     |    *    |           |         |    *    | C |   |   | *  |    | C | C |   |   | 1 |   | C | C | ADC #$44    | PC                      | [PC]                      | A <- A + {data} + C                                                |
| 6A     | ROR         | Accumlator      |   1   |     2     |    *    |           |         |         | C |   |   |    |    | C | C |   |   | 1 |   |   | C | ROR A       | None                    | None                      | C <- A&1, A <- A>>1 &#x7C; C<<7                                    |
| 6B     | ARR         | Immediate       |   2   |     2     |         |     *     |         |    *    |   |   |   |    |    | C | C |   |   | 1 |   | C | C | ARR #$44    | PC                      | [PC]                      | A <- (A & {data}) >> 1 &#x7C; (C << 7)                             |
| 6C     | JMP         | Indirect        |   3   |     5     |    *    |           |         |    *    |   |   |   | C  |    |   |   |   |   | 1 |   |   |   | JMP ($5597) | None                    | None                      | PC <- [{data}]                                                     |
| 6D     | ADC         | Absolute        |   3   |     4     |    *    |           |         |    *    | C |   |   | *  |    | C | C |   |   | 1 |   | C | C | ADC $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | A <- A + {data} + C                                                |
| 6E     | ROR         | Absolute        |   3   |     6     |    *    |           |         |    C    |   |   |   |    |    | C | C |   |   | 1 |   |   | C | ROR $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | C <- {data}&1, [addr] <- {data}>>1 &#x7C; C<<7                     |
| 6F     | RRA         | Absolute        |   3   |     6     |         |     *     |         |    C    | C |   |   | *  |    | C | C |   |   | 1 |   | C | C | RRA $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | [addr] <- ({data} >> 1 &#x7C; C << 7), ADC([addr])                 |
| 70     | BVS         | Relative        |   2   |     2++   |    *    |           |         |    *    |   |   |   | C  |    |   |   |   |   | 1 |   | * |   | BVS $44     | [PC]                    | None                      | PC <- if V = 1 then [PC] else PC+1                                 |
| 71     | ADC         | (Indirect), Y   |   2   |     5+    |    *    |           |         |    *    | C |   | * | *  |    | C | C |   |   | 1 |   | C | C | ADC ($44),Y | [[PC]]+Y                | [[[PC]]+Y]                | A <- A + {data} + C                                                |
| 72     | KIL         |                 |   1   | Exception |         |           |    *    |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   |             | None                    | None                      |                                                                    |
| 73     | RRA         | (Indirect), Y   |   2   |     8     |         |     *     |         |    C    | C |   | * | *  |    | C | C |   |   | 1 |   | C | C | RRA ($44),Y | [[PC]]+Y                | [[[PC]]+Y]                | [addr] <- ({data} >> 1 &#x7C; C << 7), ADC([addr])                 |
| 74     | NOP         | Zero Page, X    |   2   |     4     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP $44,X   | [PC]+X                  | [[[PC]+X]]                |                                                                    |
| 75     | ADC         | Zero Page, X    |   2   |     4     |    *    |           |         |    *    | C | * |   | *  |    | C | C |   |   | 1 |   | C | C | ADC $44,X   | [PC]+X                  | [[[PC]+X]]                | A <- A + {data} + C                                                |
| 76     | ROR         | Zero Page, X    |   2   |     6     |    *    |           |         |    C    |   |   |   |    |    | C | C |   |   | 1 |   |   | C | ROR $44,X   | [PC]+X                  | [[[PC]+X]]                | C <- {data}&1, [addr] <- {data}>>1 &#x7C; C<<7                     |
| 77     | RRA         | Zero Page, X    |   2   |     6     |         |     *     |         |    C    | C | * |   | *  |    | C | C |   |   | 1 |   | C | C | RRA $44,X   | [PC]+X                  | [[[PC]+X]]                | [addr] <- ({data} >> 1 &#x7C; C << 7), ADC([addr])                 |
| 78     | SEI         | Implied         |   1   |     2     |    *    |           |         |         |   |   |   |    |    |   |   | 1 |   | 1 |   |   |   | SEI         | None                    | None                      | I <- 1                                                             |
| 79     | ADC         | Absolute, Y     |   3   |     4+    |    *    |           |         |    *    | C |   | * | *  |    | C | C |   |   | 1 |   | C | C | ADC $4400,Y | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] | A <- A + {data} + C                                                |
| 7A     | NOP         | Implied         |   1   |     2     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP         | None                    | None                      | A <- A + {data} + C                                                |
| 7B     | RRA         | Absolute, Y     |   3   |     7     |         |     *     |         |    C    | C |   | * | *  |    | C | C |   |   | 1 |   | C | C | RRA $4400,Y | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] | [addr] <- ({data} >> 1 &#x7C; C << 7), ADC([addr])                 |
| 7C     | NOP         | Absolute, X     |   3   |     4+    |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | A <- A + {data} + C                                                |
| 7D     | ADC         | Absolute, X     |   3   |     4+    |    *    |           |         |    *    | C | * |   | *  |    | C | C |   |   | 1 |   | C | C | ADC $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | A <- A + {data} + C                                                |
| 7E     | ROR         | Absolute, X     |   3   |     7     |    *    |           |         |    C    |   |   |   |    |    | C | C |   |   | 1 |   |   | C | ROR $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | C <- {data}&1, [addr] <- {data}>>1 &#x7C; C<<7                     |
| 7F     | RRA         | Absolute, X     |   3   |     7     |         |     *     |         |    C    | C | * |   | *  |    | C | C |   |   | 1 |   | C | C | RRA $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | [addr] <- ({data} >> 1 &#x7C; C << 7), ADC([addr])                 |
| 80     | NOP         | Immediate       |   2   |     2     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP #$44    | PC                      | [PC]                      |                                                                    |
| 81     | STA         | (Indirect, X)   |   2   |     6     |    *    |           |         |    C    | * |   |   |    |    |   |   |   |   | 1 |   |   |   | STA ($44,X) | [[PC]+X]                | [[[PC]+X]]                | [addr] <- A                                                        |
| 82     | NOP         | Immediate       |   2   |     2     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP #$44    | PC                      | [PC]                      |                                                                    |
| 83     | SAX         | (Indirect, X)   |   2   |     6     |         |     *     |         |    C    | * | * |   | *  |    |   |   |   |   | 1 |   |   |   | SAX ($44,X) | [[PC]+X]                | [[[PC]+X]]                | [addr] <- A & X                                                    |
| 84     | STY         | Zero Page       |   2   |     3     |    *    |           |         |    C    |   |   | * |    |    |   |   |   |   | 1 |   |   |   | STY $44     | [PC]                    | [[PC]]                    | [addr] <- Y                                                        |
| 85     | STA         | Zero Page       |   2   |     3     |    *    |           |         |    C    | * |   |   |    |    |   |   |   |   | 1 |   |   |   | STA $44     | [PC]                    | [[PC]]                    | [addr] <- A                                                        |
| 86     | STX         | Zero Page       |   2   |     3     |    *    |           |         |    C    |   | * |   |    |    |   |   |   |   | 1 |   |   |   | STX $44     | [PC]                    | [[PC]]                    | [addr] <- X                                                        |
| 87     | SAX         | Zero Page       |   2   |     3     |         |     *     |         |    C    | * | * |   | *  |    |   |   |   |   | 1 |   |   |   | SAX $44     | [PC]                    | [[PC]]                    | [addr] <- A & X                                                    |
| 88     | DEY         | Implied         |   1   |     2     |    *    |           |         |         |   |   | C |    |    |   | C |   |   | 1 |   |   | C | DEY         | None                    | None                      | Y <- Y - 1                                                         |
| 89     | NOP         | Immediate       |   2   |     2     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP #$44    | PC                      | [PC]                      |                                                                    |
| 8A     | TXA         | Implied         |   1   |     2     |    *    |           |         |         | C | * |   |    |    |   | C |   |   | 1 |   |   | C | TXA         | None                    | None                      | A <- X                                                             |
| 8B     | ANE         | Immediate       |   2   |     2     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | ANE #$44    | PC                      | [PC]                      | A <- (A &#x7C; 0xEE) & X & {data}                                  |
| 8C     | STY         | Absolute        |   3   |     4     |    *    |           |         |    C    |   |   | * |    |    |   |   |   |   | 1 |   |   |   | STY $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | [addr] <- Y                                                        |
| 8D     | STA         | Absolute        |   3   |     4     |    *    |           |         |    C    | * |   |   |    |    |   |   |   |   | 1 |   |   |   | STA $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | [addr] <- A                                                        |
| 8E     | STX         | Absolute        |   3   |     4     |    *    |           |         |    C    |   | * |   |    |    |   |   |   |   | 1 |   |   |   | STX $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | [addr] <- X                                                        |
| 8F     | SAX         | Absolute        |   3   |     4     |         |     *     |         |    C    | * | * |   | *  |    |   |   |   |   | 1 |   |   |   | SAX $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | [addr] <- A & X                                                    |
| 90     | BCC         | Relative        |   2   |     2++   |    *    |           |         |    *    |   |   |   | C  |    | * |   |   |   | 1 |   |   |   | BCC $44     | [PC]                    | None                      | PC <- if C = 0 then [PC] else PC+1                                 |
| 91     | STA         | (Indirect), Y   |   2   |     5+    |    *    |           |         |    C    | * |   |   |    |    |   |   |   |   | 1 |   |   |   | STA ($44),Y | [[PC]]+Y                | [[[PC]]+Y]                | [addr] <- A                                                        |
| 92     | KIL         |                 |   1   | Exception |         |           |    *    |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   |             | None                    | None                      |                                                                    |
| 93     | SHA         | (Indirect), Y   |   2   |     6     |         |     *     |         |    C    | * | * | * | *  |    |   |   |   |   | 1 |   |   |   | SHA ($44),Y | [[PC]]+Y                | [[[PC]]+Y]                | [addr] <- A & X & ((addr>>8)+1)                                    |
| 94     | STY         | Zero Page, X    |   2   |     4     |    *    |           |         |    C    |   |   | * |    |    |   |   |   |   | 1 |   |   |   | STY $44,X   | [PC]+X                  | [[[PC]+X]]                | [addr] <- Y                                                        |
| 95     | STA         | Zero Page, X    |   2   |     4     |    *    |           |         |    C    | * |   |   |    |    |   |   |   |   | 1 |   |   |   | STA $44,X   | [PC]+X                  | [[[PC]+X]]                | [addr] <- A                                                        |
| 96     | STX         | Zero Page, Y    |   2   |     4     |    *    |           |         |    C    |   | * |   |    |    |   |   |   |   | 1 |   |   |   | STX $44,Y   | [PC]+Y                  | [[PC]+Y]                  | [addr] <- X                                                        |
| 97     | SAX         | Zero Page, Y    |   2   |     4     |         |     *     |         |    C    | * | * | * | *  |    |   |   |   |   | 1 |   |   |   | SAX $44,Y   | [PC]+Y                  | [[PC]+Y]                  | [addr] <- A & X                                                    |
| 98     | TYA         | Implied         |   1   |     2     |    *    |           |         |         | C |   | * |    |    |   | C |   |   | 1 |   |   | C | TYA         | None                    | None                      | A <- Y                                                             |
| 99     | STA         | Absolute, Y     |   3   |     4+    |    *    |           |         |    C    | * |   |   |    |    |   |   |   |   | 1 |   |   |   | STA $4400,Y | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] | [addr] <- A                                                        |
| 9A     | TXS         | Implied         |   1   |     2     |    *    |           |         |         |   | * |   |    | C  |   | C |   |   | 1 |   |   | C | TXS         | None                    | None                      | S <- X                                                             |
| 9B     | SHS         | Absolute, Y     |   3   |     5     |         |     *     |         |    C    | * | * | * | *  | C  |   |   |   |   | 1 |   |   |   | TAS $4400,Y | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] | S <- A & X, [addr] <- S & ((addr>>8)+1)                            |
| 9C     | SHY         | Absolute, X     |   3   |     5     |         |     *     |         |    C    |   | * | * | *  |    |   |   |   |   | 1 |   |   |   | SHY $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | [addr] <- Y & ((addr>>8)+1)                                        |
| 9D     | STA         | Absolute, X     |   3   |     4+    |    *    |           |         |    C    | * |   |   |    |    |   |   |   |   | 1 |   |   |   | STA $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | [addr] <- A                                                        |
| 9E     | SHX         | Absolute, Y     |   3   |     5     |         |     *     |         |    C    |   | * | * | *  |    |   |   |   |   | 1 |   |   |   | SHX $4400,Y | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] | [addr] <- X & ((addr>>8)+1)                                        |
| 9F     | SHA         | Absolute, Y     |   3   |     5     |         |     *     |         |    C    | * | * | * | *  |    |   |   |   |   | 1 |   |   |   | SHA $4400,Y | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] | [addr] <- A & X & ((addr>>8)+1)                                    |
| A0     | LDY         | Immediate       |   2   |     2     |    *    |           |         |    *    |   |   | C |    |    |   | C |   |   | 1 |   |   | C | LDY #$44    | PC                      | [PC]                      | Y <- {data}                                                        |
| A1     | LDA         | (Indirect, X)   |   2   |     6     |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | LDA ($44,X) | [[PC]+X]                | [[[PC]+X]]                | A <- A &#x7C; {data}                                               |
| A2     | LDX         | Immediate       |   2   |     2     |    *    |           |         |    *    |   | C |   |    |    |   | C |   |   | 1 |   |   | C | LDX #$44    | PC                      | [PC]                      | X <- {data}                                                        |
| A3     | LAX         | (Indirect, X)   |   2   |     6     |         |     *     |         |    *    | C | C |   | *  |    |   | C |   |   | 1 |   |   | C | LAX ($44,X) | [[PC]+X]                | [[[PC]+X]]                | A <- {data}, X <- {data}                                           |
| A4     | LDY         | Zero Page       |   2   |     3     |    *    |           |         |    *    |   |   | C |    |    |   | C |   |   | 1 |   |   | C | LDY $44     | [PC]                    | [[PC]]                    | Y <- {data}                                                        |
| A5     | LDA         | Zero Page       |   2   |     3     |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | LDA $44     | [PC]                    | [[PC]]                    | A <- A &#x7C; {data}                                               |
| A6     | LDX         | Zero Page       |   2   |     3     |    *    |           |         |    *    |   | C |   |    |    |   | C |   |   | 1 |   |   | C | LDX $44     | [PC]                    | [[PC]]                    | X <- {data}                                                        |
| A7     | LAX         | Zero Page       |   2   |     3     |         |     *     |         |    *    | C | C |   | *  |    |   | C |   |   | 1 |   |   | C | LAX $44     | [PC]                    | [[PC]]                    | A <- {data}, X <- {data}                                           |
| A8     | TAY         | Implied         |   1   |     2     |    *    |           |         |         | * |   | C |    |    |   | C |   |   | 1 |   |   | C | TAY         | None                    | None                      | Y <- A                                                             |
| A9     | LDA         | Immediate       |   2   |     2     |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | LDA #$44    | PC                      | [PC]                      | A <- A &#x7C; {data}                                               |
| AA     | TAX         | Implied         |   1   |     2     |    *    |           |         |         | * | C |   |    |    |   | C |   |   | 1 |   |   | C | TAX         | None                    | None                      | X <- A                                                             |
| AB     | LAX         | Immediate       |   2   |     2     |         |     *     |         |    *    | C | C |   | *  |    |   | C |   |   | 1 |   |   | C | LAX #$44    | PC                      | [PC]                      | A <- {data}, X <- {data}                                           |
| AC     | LDY         | Absolute        |   3   |     4     |    *    |           |         |    *    |   |   | C |    |    |   | C |   |   | 1 |   |   | C | LDY $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | Y <- {data}                                                        |
| AD     | LDA         | Absolute        |   3   |     4     |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | LDA $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | A <- A &#x7C; {data}                                               |
| AE     | LDX         | Absolute        |   3   |     4     |    *    |           |         |    *    |   | C |   |    |    |   | C |   |   | 1 |   |   | C | LDX $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | X <- {data}                                                        |
| AF     | LAX         | Absolute        |   3   |     4     |         |     *     |         |    *    | C | C |   | *  |    |   | C |   |   | 1 |   |   | C | LAX $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | A <- {data}, X <- {data}                                           |
| B0     | BCS         | Relative        |   2   |     2++   |    *    |           |         |    *    |   |   |   | C  |    | * |   |   |   | 1 |   |   |   | BCS $44     | [PC]                    | None                      | PC <- if C = 1 then [PC] else PC+1                                 |
| B1     | LDA         | (Indirect), Y   |   2   |     5+    |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | LDA ($44),Y | [[PC]]+Y                | [[[PC]]+Y]                | A <- A &#x7C; {data}                                               |
| B2     | KIL         |                 |   1   | Exception |         |           |    *    |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   |             | None                    | None                      |                                                                    |
| B3     | LAX         | (Indirect), Y   |   2   |     5+    |         |     *     |         |    *    | C | C | * | *  |    |   | C |   |   | 1 |   |   | C | LAX ($44),Y | [[PC]]+Y                | [[[PC]]+Y]                | A <- {data}, X <- {data}                                           |
| B4     | LDY         | Zero Page, X    |   2   |     4     |    *    |           |         |    *    |   |   | C |    |    |   | C |   |   | 1 |   |   | C | LDY $44,X   | [PC]+X                  | [[[PC]+X]]                | Y <- {data}                                                        |
| B5     | LDA         | Zero Page, X    |   2   |     4     |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | LDA $44,X   | [PC]+X                  | [[[PC]+X]]                | A <- A &#x7C; {data}                                               |
| B6     | LDX         | Zero Page, Y    |   2   |     4     |    *    |           |         |    *    |   | C |   |    |    |   | C |   |   | 1 |   |   | C | LDX $44,Y   | [PC]+Y                  | [[PC]+Y]                  | X <- {data}                                                        |
| B7     | LAX         | Zero Page, Y    |   2   |     4     |         |     *     |         |    *    | C | C | * | *  |    |   | C |   |   | 1 |   |   | C | LAX $44,Y   | [PC]+Y                  | [[PC]+Y]                  | A <- {data}, X <- {data}                                           |
| B8     | CLV         | Implied         |   1   |     2     |    *    |           |         |         |   |   |   |    |    |   |   |   |   | 1 |   | 0 |   | CLV         | None                    | None                      | V <- 0                                                             |
| B9     | LDA         | Absolute, Y     |   3   |     4+    |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | LDA $4400,Y | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] | A <- A &#x7C; {data}                                               |
| BA     | TSX         | Implied         |   1   |     2     |    *    |           |         |         |   | C |   |    | C  |   | C |   |   | 1 |   |   | C | TSX         | None                    | None                      | X <- S                                                             |
| BB     | LAS         | Absolute, Y     |   3   |     4+    |         |     *     |         |    *    | C | C | * | *  | C  |   | C |   |   | 1 |   |   | C | LAS $4400,Y | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] | S <- S & {data}, X <- S, A <- S                                    |
| BC     | LDY         | Absolute, X     |   3   |     4+    |    *    |           |         |    *    |   |   | C |    |    |   | C |   |   | 1 |   |   | C | LDY $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | Y <- {data}                                                        |
| BD     | LDA         | Absolute, X     |   3   |     4+    |    *    |           |         |    *    | C |   |   |    |    |   | C |   |   | 1 |   |   | C | LDA $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | A <- A &#x7C; {data}                                               |
| BE     | LDX         | Absolute, Y     |   3   |     4+    |    *    |           |         |    *    |   | C |   |    |    |   | C |   |   | 1 |   |   | C | LDX $4400,Y | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] | X <- {data}                                                        |
| BF     | LAX         | Absolute, Y     |   3   |     4+    |         |     *     |         |    *    | C | C | * | *  |    |   | C |   |   | 1 |   |   | C | LAX $4400,Y | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] | A <- {data}, X <- {data}                                           |
| C0     | CPY         | Immediate       |   2   |     2     |    *    |           |         |         |   |   | * |    |    | C | C |   |   | 1 |   |   | C | CPY #$44    | PC                      | [PC]                      | Y - {data}                                                         |
| C1     | CMP         | (Indirect, X)   |   2   |     6     |    *    |           |         |         | * |   |   |    |    | C | C |   |   | 1 |   |   | C | CMP ($44,X) | [[PC]+X]                | [[[PC]+X]]                | A - {data}                                                         |
| C2     | NOP         | Immediate       |   2   |     2     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP #$44    | PC                      | [PC]                      |                                                                    |
| C3     | DCP         | (Indirect, X)   |   2   |     8     |         |     *     |         |    C    | * | * |   | *  |    | C | C |   |   | 1 |   |   | C | DCP ($44,X) | [[PC]+X]                | [[[PC]+X]]                | [addr] <- A - {data} - 1                                           |
| C4     | CPY         | Zero Page       |   2   |     3     |    *    |           |         |         |   |   | * |    |    | C | C |   |   | 1 |   |   | C | CPY $44     | [PC]                    | [[PC]]                    | Y - {data}                                                         |
| C5     | CMP         | Zero Page       |   2   |     3     |    *    |           |         |         | * |   |   |    |    | C | C |   |   | 1 |   |   | C | CMP $44     | [PC]                    | [[PC]]                    | A - {data}                                                         |
| C6     | DEC         | Zero Page       |   2   |     5     |    *    |           |         |    C    |   |   |   |    |    |   | C |   |   | 1 |   |   | C | DEC $44     | [PC]                    | [[PC]]                    | [addr] <- {data}+1                                                 |
| C7     | DCP         | Zero Page       |   2   |     5     |         |     *     |         |    C    | * |   |   | *  |    | C | C |   |   | 1 |   |   | C | DCP $44     | [PC]                    | [[PC]]                    | [addr] <- A - {data} - 1                                           |
| C8     | INY         | Implied         |   1   |     2     |    *    |           |         |         |   |   | C |    |    |   | C |   |   | 1 |   |   | C | INY         | None                    | None                      | Y <- Y - 1                                                         |
| C9     | CMP         | Immediate       |   2   |     2     |    *    |           |         |         | * |   |   |    |    | C | C |   |   | 1 |   |   | C | CMP #$44    | PC                      | [PC]                      | A - {data}                                                         |
| CA     | DEX         | Implied         |   1   |     2     |    *    |           |         |         |   | C |   |    |    |   | C |   |   | 1 |   |   | C | DEX         | None                    | None                      | X <- X - 1                                                         |
| CB     | SBX         | Immediate       |   2   |     2     |         |     *     |         |    *    | * | C |   | *  |    | C | C |   |   | 1 |   |   | C | AXS #$44    | PC                      | [PC]                      | X <- (A & X) - {data}                                              |
| CC     | CPY         | Absolute        |   3   |     4     |    *    |           |         |         |   |   | * |    |    | C | C |   |   | 1 |   |   | C | CPY $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | Y - {data}                                                         |
| CD     | CMP         | Absolute        |   3   |     4     |    *    |           |         |         | * |   |   |    |    | C | C |   |   | 1 |   |   | C | CMP $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | A - {data}                                                         |
| CE     | DEC         | Absolute        |   3   |     6     |    *    |           |         |    C    |   |   |   |    |    |   | C |   |   | 1 |   |   | C | DEC $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | [addr] <- {data}+1                                                 |
| CF     | DCP         | Absolute        |   3   |     6     |         |     *     |         |    C    | * |   |   | *  |    | C | C |   |   | 1 |   |   | C | DCP $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | [addr] <- A - {data} - 1                                           |
| D0     | BNE         | Relative        |   2   |     2++   |    *    |           |         |    *    |   |   |   | C  |    |   | * |   |   | 1 |   |   |   | BNE $44     | [PC]                    | None                      | PC <- if Z = 0 then [PC] else PC+1                                 |
| D1     | CMP         | (Indirect), Y   |   2   |     5+    |    *    |           |         |         | * |   |   |    |    | C | C |   |   | 1 |   |   | C | CMP ($44),Y | [[PC]]+Y                | [[[PC]]+Y]                | A - {data}                                                         |
| D2     | KIL         |                 |   1   | Exception |         |           |    *    |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   |             | None                    | None                      |                                                                    |
| D3     | DCP         | (Indirect), Y   |   2   |     8     |         |     *     |         |    C    | * |   | * | *  |    | C | C |   |   | 1 |   |   | C | DCP ($44),Y | [[PC]]+Y                | [[[PC]]+Y]                | [addr] <- A - {data} - 1                                           |
| D4     | NOP         | Zero Page, X    |   2   |     4     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP $44,X   | [PC]+X                  | [[[PC]+X]]                |                                                                    |
| D5     | CMP         | Zero Page, X    |   2   |     4     |    *    |           |         |         | * |   |   |    |    | C | C |   |   | 1 |   |   | C | CMP $44,X   | [PC]+X                  | [[[PC]+X]]                | A - {data}                                                         |
| D6     | DEC         | Zero Page, X    |   2   |     6     |    *    |           |         |    C    |   |   |   |    |    |   | C |   |   | 1 |   |   | C | DEC $44,X   | [PC]+X                  | [[[PC]+X]]                | [addr] <- {data}+1                                                 |
| D7     | DCP         | Zero Page, X    |   2   |     6     |         |     *     |         |    C    | * | * |   | *  |    | C | C |   |   | 1 |   |   | C | DCP $44,X   | [PC]+X                  | [[[PC]+X]]                | [addr] <- A - {data} - 1                                           |
| D8     | CLD         | Implied         |   1   |     2     |    *    |           |         |         |   |   |   |    |    |   |   |   | 0 | 1 |   |   |   | CLD         | None                    | None                      | D <- 0                                                             |
| D9     | CMP         | Absolute, Y     |   3   |     4+    |    *    |           |         |         | * |   |   |    |    | C | C |   |   | 1 |   |   | C | CMP $4400,Y | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] | A - {data}                                                         |
| DA     | NOP         | Implied         |   1   |     2     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP         | None                    | None                      |                                                                    |
| DB     | DCP         | Absolute, Y     |   3   |     7     |         |     *     |         |    C    | * |   | * | *  |    | C | C |   |   | 1 |   |   | C | DCP $4400,Y | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] | [addr] <- A - {data} - 1                                           |
| DC     | NOP         | Absolute, X     |   3   |     4+    |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] |                                                                    |
| DD     | CMP         | Absolute, X     |   3   |     4+    |    *    |           |         |         | * |   |   |    |    | C | C |   |   | 1 |   |   | C | CMP $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | A - {data}                                                         |
| DE     | DEC         | Absolute, X     |   3   |     6+    |    *    |           |         |    C    |   |   |   |    |    |   | C |   |   | 1 |   |   | C | DEC $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | [addr] <- {data}+1                                                 |
| DF     | DCP         | Absolute, X     |   3   |     7     |         |     *     |         |    C    | * | * |   | *  |    | C | C |   |   | 1 |   |   | C | DCP $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | [addr] <- A - {data} - 1                                           |
| E0     | CPX         | Immediate       |   2   |     2     |    *    |           |         |         |   | * |   |    |    | C | C |   |   | 1 |   |   | C | CPX #$44    | PC                      | [PC]                      | X - {data}                                                         |
| E1     | SBC         | (Indirect, X)   |   2   |     6     |    *    |           |         |    *    | C |   |   | *  |    | C | C |   |   | 1 |   | C | C | SBC ($44,X) | [[PC]+X]                | [[[PC]+X]]                | ADC({data})                                                        |
| E2     | NOP         | Immediate       |   2   |     2     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP #$44    | PC                      | [PC]                      |                                                                    |
| E3     | ISB         | (Indirect, X)   |   2   |     8     |         |     *     |         |    C    | * | * |   |    |    | C | C |   |   | 1 |   | C | C | ISC ($44,X) | [[PC]+X]                | [[[PC]+X]]                | [addr] <- {data} + 1, SBC([addr])                                  |
| E4     | CPX         | Zero Page       |   2   |     3     |    *    |           |         |         |   | * |   |    |    | C | C |   |   | 1 |   |   | C | CPX $44     | [PC]                    | [[PC]]                    | X - {data}                                                         |
| E5     | SBC         | Zero Page       |   2   |     3     |    *    |           |         |    *    | C |   |   | *  |    | C | C |   |   | 1 |   | C | C | SBC $44     | [PC]                    | [[PC]]                    | ADC({data})                                                        |
| E6     | INC         | Zero Page       |   2   |     5     |    *    |           |         |    C    |   |   |   |    |    |   | C |   |   | 1 |   |   | C | INC $44     | [PC]                    | [[PC]]                    | [addr] <- {data} + 1                                               |
| E7     | ISB         | Zero Page       |   2   |     5     |         |     *     |         |    C    | * |   |   |    |    | C | C |   |   | 1 |   | C | C | ISC $44     | [PC]                    | [[PC]]                    | [addr] <- {data} + 1, SBC([addr])                                  |
| E8     | INX         | Implied         |   1   |     2     |    *    |           |         |         |   | C |   |    |    |   | C |   |   | 1 |   |   | C | INX         | None                    | None                      | X <- X + 1                                                         |
| E9     | SBC         | Immediate       |   2   |     2     |    *    |           |         |    *    | C |   |   | *  |    | C | C |   |   | 1 |   | C | C | SBC #$44    | PC                      | [PC]                      | ADC({data})                                                        |
| EA     | NOP         | Implied         |   1   |     2     |    *    |           |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP         | None                    | None                      |                                                                    |
| EB     | SBC         | Immediate       |   2   |     2     |         |     *     |         |    *    | C |   |   | *  |    | C | C |   |   | 1 |   | C | C | SBC #$44    | PC                      | [PC]                      | ADC({data})                                                        |
| EC     | CPX         | Absolute        |   3   |     4     |    *    |           |         |         |   | * |   |    |    | C | C |   |   | 1 |   |   | C | CPX $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | X - {data}                                                         |
| ED     | SBC         | Absolute        |   3   |     4     |    *    |           |         |    *    | C |   |   | *  |    | C | C |   |   | 1 |   | C | C | SBC $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | ADC({data})                                                        |
| EE     | INC         | Absolute        |   3   |     6     |    *    |           |         |    C    |   |   |   |    |    |   | C |   |   | 1 |   |   | C | INC $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | [addr] <- {data} + 1                                               |
| EF     | ISB         | Absolute        |   3   |     6     |         |     *     |         |    C    | * |   |   |    |    | C | C |   |   | 1 |   | C | C | ISC $4400   | ([PC+1]<<8&#x7C;[PC])   | [([PC+1]<<8&#x7C;[PC])]   | [addr] <- {data} + 1, SBC([addr])                                  |
| F0     | BEQ         | Relative        |   2   |     2++   |    *    |           |         |    *    |   |   |   | C  |    |   | * |   |   | 1 |   |   |   | BEQ $44     | [PC]                    | None                      | PC <- if Z = 1 then [PC] else PC+1                                 |
| F1     | SBC         | (Indirect), Y   |   2   |     5+    |    *    |           |         |    *    | C |   |   | *  |    | C | C |   |   | 1 |   | C | C | SBC ($44),Y | [[PC]]+Y                | [[[PC]]+Y]                | ADC({data})                                                        |
| F2     | KIL         |                 |   1   | Exception |         |           |    *    |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   |             | None                    | None                      |                                                                    |
| F3     | ISB         | (Indirect), Y   |   2   |     8     |         |     *     |         |    C    | * |   | * |    |    | C | C |   |   | 1 |   | C | C | ISC ($44),Y | [[PC]]+Y                | [[[PC]]+Y]                | [addr] <- {data} + 1, SBC([addr])                                  |
| F4     | NOP         | Zero Page, X    |   2   |     4     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP $44,X   | [PC]+X                  | [[[PC]+X]]                |                                                                    |
| F5     | SBC         | Zero Page, X    |   2   |     4     |    *    |           |         |    *    | C |   |   | *  |    | C | C |   |   | 1 |   | C | C | SBC $44,X   | [PC]+X                  | [[[PC]+X]]                | ADC({data})                                                        |
| F6     | INC         | Zero Page, X    |   2   |     6     |    *    |           |         |    C    |   |   |   |    |    |   | C |   |   | 1 |   |   | C | INC $44,X   | [PC]+X                  | [[[PC]+X]]                | [addr] <- {data} + 1                                               |
| F7     | ISB         | Zero Page, X    |   2   |     6     |         |     *     |         |    C    | * | * |   |    |    | C | C |   |   | 1 |   | C | C | ISC $44,X   | [PC]+X                  | [[[PC]+X]]                | [addr] <- {data} + 1, SBC([addr])                                  |
| F8     | SED         | Implied         |   1   |     2     |    *    |           |         |         |   |   |   |    |    |   |   |   | 1 | 1 |   |   |   | SED         | None                    | None                      | D <- 1                                                             |
| F9     | SBC         | Absolute, Y     |   3   |     4+    |    *    |           |         |    *    | C |   |   | *  |    | C | C |   |   | 1 |   | C | C | SBC $4400,Y | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] | ADC({data})                                                        |
| FA     | NOP         | Implied         |   1   |     2     |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP         | None                    | None                      |                                                                    |
| FB     | ISB         | Absolute, Y     |   3   |     7     |         |     *     |         |    C    | * |   | * |    |    | C | C |   |   | 1 |   | C | C | ISC $4400,Y | ([PC+1]<<8&#x7C;[PC])+Y | [([PC+1]<<8&#x7C;[PC])+Y] | [addr] <- {data} + 1, SBC([addr])                                  |
| FC     | NOP         | Absolute, X     |   3   |     4+    |         |     *     |         |         |   |   |   |    |    |   |   |   |   | 1 |   |   |   | NOP $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] |                                                                    |
| FD     | SBC         | Absolute, X     |   3   |     4+    |    *    |           |         |    *    | C |   |   | *  |    | C | C |   |   | 1 |   | C | C | SBC $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | ADC({data})                                                        |
| FE     | INC         | Absolute, X     |   3   |     7     |    *    |           |         |    C    |   |   |   |    |    |   | C |   |   | 1 |   |   | C | INC $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | [addr] <- {data} + 1                                               |
| FF     | ISB         | Absolute, X     |   3   |     7     |         |     *     |         |    C    | * | * |   |    |    | C | C |   |   | 1 |   | C | C | ISC $4400,X | ([PC+1]<<8&#x7C;[PC])+X | [([PC+1]<<8&#x7C;[PC])+X] | [addr] <- {data} + 1, SBC([addr])                                  |
