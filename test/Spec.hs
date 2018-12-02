import Test.Tasty
import qualified D1P1Spec
import qualified D1P2Spec
import qualified D2P1Spec
import qualified D2P2Spec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [
        D1P1Spec.tests
      , D1P2Spec.tests
      , D2P1Spec.tests
      , D2P2Spec.tests
    ]