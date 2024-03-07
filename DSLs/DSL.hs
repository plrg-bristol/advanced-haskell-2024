

module DSL where

-- examples: CSS, HTML

-- eDSL
-- embedded DSL

-- deep
-- syntax = data type
-- semantics = function over data type

data RobotLang
  = Stop
  | Fwd Int
  | Then RobotLang RobotLang

progID :: RobotLang
progID = Stop

progFwd10 :: RobotLang
progFwd10 = Fwd 10

type Commands = [RobotLang]

prog :: Commands
prog = [Fwd 10, Stop]

progThen :: RobotLang
-- progThen = Then progFwd10 progID
progThen = progFwd10 `Then` progID

data RL
  = S
  | F Int RL
  | Turn Int RL

howFarFwd :: RL -> Int
howFarFwd S = 0
howFarFwd (F n rl)    = n + howFarFwd rl
howFarFwd (Turn n rl) = howFarFwd rl

commands :: RL -> Int
commands S = 1
commands (F n rl) = 1 + commands rl
commands (Turn n rl) = 1 + commands rl

-- shallow embedding

-- syntax - functions (first class object un haskell)
-- semantics - directly applied

type HowFarFwd = (Int, Int)
 -- (fwd, cmd)

stop :: HowFarFwd
stop = (0, 1)

fwd :: Int -> HowFarFwd -> HowFarFwd
fwd n (df, dc) = (n + df, dc + 1)

turn :: Int -> HowFarFwd -> HowFarFwd
turn n (x, y) = (x, y+1)

progShallow :: HowFarFwd
progShallow = fwd 10 $ turn 20 $ stop
 -- 10 + 0

-- comparisons: deep vs shallow
-- fav = deep (shallow annoying, semantics is wider than syntax)

-- shallow better at extending syntax, more direct
-- deep better at extending semantics, and you have access to the structure of the program (allows for optimisations (doesnt necessarily mean that deep will be faster, also with shallow compiler can do it for you), analysis, dependent semantics)

-- best of both worlds

-- classy embedding
-- extensible in syntax and semantics

-- syntax = type class
-- semantics = instances of this class

class Rob a where
  s :: a
  f :: Int -> a -> a
  t :: Int -> a -> a
  -- dance :: a -> a

instance Rob RL where
  s = S
  f = F
  t = Turn

newtype NumCmds = NumCmds Int

instance Rob NumCmds where
  s = NumCmds 1
  f _ (NumCmds n) = NumCmds (n + 1)
  t _ (NumCmds n) = NumCmds (n + 1)

newtype HowFar = HowFar Int
  deriving Num -- For convenience

instance Rob HowFar where
  s = 1 -- We can do this because we derived `Num` for `HowFar`
  f _ n = n + 1
  t _ n = n + 1


class Rob a => RobDance a where
  d :: a -> a

-- next time?
--  * binder (lambda, or forall)
--  * even more modular (fix, data types a la carte)
--  * parsers (this is actually a separate topic, so just ask for that)

