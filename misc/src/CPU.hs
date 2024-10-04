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
  | JUMP
  | RELJUMP Int

type Prog = [Instr]

type Instrs = IntMap Instr

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

execInstr' (pc, acc, mem) instr = case instr of
  STORE addr -> (pc + 1, acc, IM.insert addr acc mem)
  LOAD addr -> (pc + 1, mem IM.! addr, mem)
  HALT -> (-1, acc, mem)
  ADDLit x -> (pc + 1, acc + x, mem)
  ADD addr -> (pc + 1, acc + mem IM.! addr, mem)
  JUMP -> (acc, acc, mem)
  RELJUMP n -> (pc + 1 + n, acc, mem)


exec' :: Instrs -> (Int, Int, Memory)
exec' instrs = go (0, 0, IM.empty)
  where
    go state@(pc, _acc, _mem) = case instrs IM.!? pc of
      Nothing -> error "out of bounds" state
      Just HALT -> state
      Just instr -> go (execInstr' state instr)

toInstrs :: [a] -> IntMap a
toInstrs = IM.fromList . zip [0..]

ex5 = [ADDLit 3, STORE 1, JUMP, HALT, ADDLit 10, HALT]
ex6= [ADDLit 3, STORE 1, RELJUMP 1, HALT, ADDLit 10, HALT]
