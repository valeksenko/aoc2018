import Test.Tasty
import qualified D1P1Spec
import qualified D1P2Spec
import qualified D2P1Spec
import qualified D2P2Spec
import qualified D3P1Spec
import qualified D3P2Spec
import qualified D4Spec
import qualified D5P1Spec
import qualified D5P2Spec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [
        D1P1Spec.tests
      , D1P2Spec.tests
      , D2P1Spec.tests
      , D2P2Spec.tests
      , D3P1Spec.tests
      , D3P2Spec.tests
      , D4Spec.tests
      , D5P1Spec.tests
      , D5P2Spec.tests
    ]