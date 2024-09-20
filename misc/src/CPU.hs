{-# LANGUAGE ImportQualifiedPost #-}
module CPU where
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List (foldl')

type Memory = IntMap Int

type MemAddr = Int

data Instr
  = STORE MemAddr
  | LOAD MemAddr
  | HALT
  | ADDLit Int
  | ADD MemAddr

type Prog = [Instr]

ex1 = [HALT]

ex2 = [LOAD 0, HALT]

ex3 = [LOAD 0, ADDLit 1, STORE 1, HALT]

ex4 = [ADDLit 1, STORE 1, HALT]


execInstr (acc, mem) instr = case instr of
  STORE addr -> (acc, IM.insert addr acc mem)
  LOAD addr -> (mem IM.! addr, mem)
  HALT -> (acc, mem)
  ADDLit x -> (acc + x, mem)
  ADD addr -> (acc + mem IM.! addr, mem)

exec :: Prog -> (Int, Memory)
exec instrs = foldl' f k instrs
  where
    k = (0, IM.empty)

    f = execInstr
