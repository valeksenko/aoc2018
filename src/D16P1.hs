module D16P1 (
    samplecount
) where

import Data.List
import qualified Data.Sequence as S
import Data.Foldable (toList)
import Data.Bits ((.&.), (.|.))

type Register = Int
type Address = (Int, Int, Int)

data InstructionSet
    = ADDr Address
    | ADDi Address
    | MULr Address
    | MULi Address
    | BANr Address
    | BANi Address
    | BORr Address
    | BORi Address
    | SETr Address
    | SETi Address
    | GTIr Address
    | GTRr Address
    | GTRi Address
    | EQIr Address
    | EQRr Address
    | EQRi Address
    deriving(Show, Eq)

registerABmount = 4
opAmount = 16

allInstructions = [ADDr, ADDi, MULr, MULi, BANr, BANi, BORr, BORi, SETr, SETi, GTIr, GTRr, GTRi, EQIr, EQRr, EQRi]

samplecount :: [([Register], Address, [Register])] -> Int
samplecount = length . filter ((>2) . length) . map matching
    where
        matching (iReg, addr, tReg) = filter (== tReg) $ runSample addr iReg

runSample :: Address -> [Register] -> [[Register]]
runSample addr registers = map (\op -> toList $ execOp (S.fromList registers) (op addr)) allInstructions

runProgram :: [InstructionSet] -> [Register]
runProgram = toList . foldl' execOp (S.replicate registerABmount 0)

setC :: Int -> Register -> S.Seq Register -> S.Seq Register
setC = S.update

registerAB :: (Int, Int, Int) -> S.Seq Register -> (Int, Int, Int)
registerAB (a, b, c) registers = (registers `S.index` a, registers `S.index` b, c)

immediateA :: (Int, Int, Int) -> S.Seq Register -> (Int, Int, Int)
immediateA (a, b, c) registers = (a, registers `S.index`  b, c)

immediateB :: (Int, Int, Int) -> S.Seq Register -> (Int, Int, Int)
immediateB (a, b, c) registers = (registers `S.index` a, b, c)

execOp :: S.Seq Register -> InstructionSet -> S.Seq Register

execOp registers (ADDr addr) = op $ registerAB addr registers
    where
        op (a, b, c) = setC c (a + b) registers

execOp registers (ADDi addr) = op $ immediateB addr registers
    where
        op (a, b, c) = setC c (a + b) registers

execOp registers (MULr addr) = op $ registerAB addr registers
    where
        op (a, b, c) = setC c (a * b) registers

execOp registers (MULi addr) = op $ immediateB addr registers
    where
        op (a, b, c) = setC c (a * b) registers

execOp registers (BANr addr) = op $ registerAB addr registers
    where
        op (a, b, c) = setC c (a .&. b) registers

execOp registers (BANi addr) = op $ immediateB addr registers
    where
        op (a, b, c) = setC c (a .&. b) registers

execOp registers (BORr addr) = op $ registerAB addr registers
    where
        op (a, b, c) = setC c (a .|. b) registers

execOp registers (BORi addr) = op $ immediateB addr registers
    where
        op (a, b, c) = setC c (a .|. b) registers

execOp registers (SETr addr) = op $ registerAB addr registers
    where
        op (a, b, c) = setC c a registers

execOp registers (SETi addr) = op $ immediateA addr registers
    where
        op (a, b, c) = setC c a registers

execOp registers (GTRr addr) = op $ registerAB addr registers
    where
        op (a, b, c) = setC c (if a > b then 1 else 0) registers

execOp registers (GTIr addr) = op $ immediateA addr registers
    where
        op (a, b, c) = setC c (if a > b then 1 else 0) registers

execOp registers (GTRi addr) = op $ immediateB addr registers
    where
        op (a, b, c) = setC c (if a > b then 1 else 0) registers

execOp registers (EQRr addr) = op $ registerAB addr registers
    where
        op (a, b, c) = setC c (if a == b then 1 else 0) registers

execOp registers (EQIr addr) = op $ immediateA addr registers
    where
        op (a, b, c) = setC c (if a == b then 1 else 0) registers

execOp registers (EQRi addr) = op $ immediateB addr registers
    where
        op (a, b, c) = setC c (if a == b then 1 else 0) registers



{-
https://adventofcode.com/2018/day/16

According to the manual, the device has four registers (numbered 0 through 3) that can be manipulated by instructions containing one of 16 opcodes. The registers start with the value 0.

Every instruction consists of four values: an opcode, two inputs (named A and B), and an output (named C), in that order. The opcode specifies the behavior of the instruction and how the inputs are interpreted. The output, C, is always treated as a register.

In the opcode descriptions below, if something says "value A", it means to take the number given as A literally. (This is also called an "immediate" value.) If something says "register A", it means to use the number given as A to read from (or write to) the register with that number. So, if the opcode addi adds register A and value B, storing the result in register C, and the instruction addi 0 7 3 is encountered, it would add 7 to the value contained by register 0 and store the sum in register 3, never modifying registers 0, 1, or 2 in the process.

Many opcodes are similar except for how they interpret their arguments. The opcodes fall into seven general categories:

Addition:

addr (add register) stores into register C the result of adding register A and register B.
addi (add immediate) stores into register C the result of adding register A and value B.
Multiplication:

mulr (multiply register) stores into register C the result of multiplying register A and register B.
muli (multiply immediate) stores into register C the result of multiplying register A and value B.
Bitwise AND:

banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
Bitwise OR:

borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
Assignment:

setr (set register) copies the contents of register A into register C. (Input B is ignored.)
seti (set immediate) stores value A into register C. (Input B is ignored.)
Greater-than testing:

gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
Equality testing:

eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.
Unfortunately, while the manual gives the name of each opcode, it doesn't seem to indicate the number. However, you can monitor the CPU to see the contents of the registers before and after instructions are executed to try to work them out. Each opcode has a number from 0 through 15, but the manual doesn't say which is which. For example, suppose you capture the following sample:

Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]
This sample shows the effect of the instruction 9 2 1 2 on the registers. Before the instruction is executed, register 0 has value 3, register 1 has value 2, and registers 2 and 3 have value 1. After the instruction is executed, register 2's value becomes 2.

The instruction itself, 9 2 1 2, means that opcode 9 was executed with A=2, B=1, and C=2. Opcode 9 could be any of the 16 opcodes listed above, but only three of them behave in a way that would cause the result shown in the sample:

Opcode 9 could be mulr: register 2 (which has a value of 1) times register 1 (which has a value of 2) produces 2, which matches the value stored in the output register, register 2.
Opcode 9 could be addi: register 2 (which has a value of 1) plus value 1 produces 2, which matches the value stored in the output register, register 2.
Opcode 9 could be seti: value 2 matches the value stored in the output register, register 2; the number given for B is irrelevant.
None of the other opcodes produce the result captured in the sample. Because of this, the sample above behaves like three opcodes.

You collect many of these samples (the first section of your puzzle input). The manual also includes a small test program (the second section of your puzzle input) - you can ignore it for now.

Ignoring the opcode numbers, how many samples in your puzzle input behave like three or more opcodes?
-}