module D16 (
    runSample
  , runProgram
  , allInstructions
  , InstructionSet(..)
  , Register
  , Address
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

runSample :: Address -> [Register] -> [(Int, [Register])]
runSample addr registers = map (\(op, i) -> (i, toList $ execOp (S.fromList registers) (op addr))) $ zip allInstructions [0..]

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
