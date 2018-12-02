import Test.Tasty
import qualified D1P1Spec
import qualified D1P2Spec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [
        D1P1Spec.tests
        , D1P2Spec.tests
    ]