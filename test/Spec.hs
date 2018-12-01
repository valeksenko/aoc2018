import Test.Tasty
import qualified D1P1Spec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [
        D1P1Spec.tests
    ]