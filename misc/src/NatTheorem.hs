import GHC.TypeLits
import Data.Kind (Constraint)

type AssocType :: Nat -> Nat -> Nat -> Constraint
type AssocType a b c = (a + b) + c ~ a + (b + c)


