import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import BadMonoid

main :: IO ()
main =
  quickBatch (monoid Twoo)

instance Arbitrary Bull where
  arbitrary = oneof [pure Fools, pure Twoo]

instance EqProp Bull where (=-=) = eq
