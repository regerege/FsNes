--------------------------------------------------
# NES�̎d�l
--------------------------------------------------
�傫�������ĂR�ɕ����ċL�ڂ��Ă����܂��B�i�C�������΁A�����ǁI�j
��{�I�ɂ� NES �Ŏg���Ă��� 6502 ���x�[�X�Ƃ����Ǝ���CPU�ɂ��Ă̎d�l�����C���ŋL�ڂ�����Ƃ��܂��B

�C������������ CPU �ȊO�� APU �� PPU �ɂ��Ă��L�ڂ��čs����΂ƍl���Ă��܂����A���܂�]�͂������̂Ŋ��҂͂��Ȃ��悤�ɂ��肢���܂��B


1. [CPU 6502 �̎d�l](#CPU6502)
  1. [�N���b�N��](#�N���b�N��)
  2. [����](#����)
  3. [���߂̎d�l](#���߂̎d�l)


<a name="CPU6502"></a>
--------------------------------------------------
## CPU 6502 �̎d�l
--------------------------------------------------
NES �� CPU �� 6502 ���x�[�X�ɍ���Ă��܂��B
CPU �̖��߂����Ȃ�߂������̂ƂȂ��Ă��܂����������̂ł͂Ȃ����Ƃ����F���������B


<a name="�N���b�N��"></a>
### �N���b�N��
CPU �̃N���b�N���� NES �Ɏ��t�����Ă��鐅���U���q��12�����ƂȂ��Ă��܂��B
�����U���q�̃N���b�N�����x�[�X�Ƃ����ꍇ�̃N���b�N���͈ȉ��̒ʂ�ƂȂ�܂��B
�����U���q�̃N���b�N�����}�X�^�[�N���b�N�Ɩ��t���܂��B
��12���� �Ƃ̓��C���ƂȂ�N���b�N������12���1��̃N���b�N�𓾂邱�ƁA�d�q�H��̊�b���ۂ��̂ŕ�����Ȃ��l�͂�������׋����Ă݂�Ɗy�������Ǝv���܂��B

|       ����       |  �N���b�N��   | ���� |
|:-----------------|--------------:|-----:|
| �}�X�^�[�N���b�N | 21477272 Hz   |      |
| CPU              | �� 1789772 Hz |  12  |
| PPU              | 5369318 Hz    |   4  |


<a name="����"></a>
### ����
��������߂��܂߂�ƑS���� 72 �� ���݂��܂��B

|���� | ����                                         |
|-----|----------------------------------------------|
| ADC | Add M to A with Carry                        |
| ANC | ��1                                          |
| AND | And Memory With Accumulator                  |
| ANE | ��1                                          |
| ARR | ��1                                          |
| ASL | Shift Left One Bit (M or A)                  |
| ASR | ��1                                          |
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
| DCP | ��1                                          |
| DEC | Decrement Memory by One                      |
| DEX | Decrement Index X Register by One            |
| DEY | Decrement Index Y Register by One            |
| EOR | Exclusive Or Memory With Accumulator         |
| INC | Increment Memory by One                      |
| INX | Increment Index X Register by One            |
| INY | Increment Index Y Register by One            |
| ISB | ��1                                          |
| JMP | Jump to New Location                         |
| JSR | Jump to New Location Saving Return Address   |
| KIL | ��2                                          |
| LAS | ��1                                          |
| LAX | ��1                                          |
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
| RLA | ��1                                          |
| ROL | Rotate One Bit Left (Memory or Accumulator)  |
| ROR | Rotate One Bit Right (Memory or Accumulator) |
| RRA | ��1                                          |
| RTI | Retrun from Interrupt                        |
| RTS | Retrun from Subroutine                       |
| SAX | ��1                                          |
| SBC | Subtract with Carry                          |
| SBX | ��1                                          |
| SEC | Set Carry Flag                               |
| SED | Set Decimal Flag                             |
| SEI | Set Interrupt Disable Flag                   |
| SHA | ��1                                          |
| SHS | ��1                                          |
| SHX | ��1                                          |
| SHY | ��1                                          |
| SLO | ��1                                          |
| SRE | ��1                                          |
| STA | Store Accumulator In Memory                  |
| STX | Store X Index In Memory                      |
| STY | Store Y Index In Memory                      |
| TAX | Transfer Accumulator to X Index              |
| TAY | Transfer Accumulator to Y Index              |
| TSX | Transfer Stack Pointer to X Index            |
| TXA | Transfer X Index to Accumulator              |
| TXS | Transfer X Index to Stack Pointer            |
| TYA | Transfer Y Index to Accumulator              |

��1. ��������߂̂��ߕs��
��2. ����`���߁H �����̋�����~



<a name="���߂̎d�l"></a>
### ���߂̎d�l
NES �� CPU �͐������̃A�h���b�V���O���[�h�Ɗe���߂�g�ݍ��킹�邱�Ƃő����̖��߂��`����Ă��܂��B
�A�h���b�V���O���[�h�ɂ��Ă͌�q���܂��B
�ЂƂ܂����C���ƂȂ閽�߂̎d�l���L�ڂ���Ƃ��܂��B

