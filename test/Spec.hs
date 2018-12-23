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
import qualified D6P1Spec
import qualified D6P2Spec
import qualified D7P1Spec
import qualified D7P2Spec
import qualified D8P1Spec
import qualified D8P2Spec
import qualified D9Spec
import qualified D10Spec
import qualified D11P1Spec
import qualified D11P2Spec
import qualified D12Spec
import qualified D13P1Spec
import qualified D13P2Spec
import qualified D14P1Spec
import qualified D14P2Spec
import qualified D15P1Spec
import qualified D15P2Spec
import qualified D16P1Spec
import qualified D17P1Spec
import qualified D18P1Spec
import qualified D19P1Spec
import qualified D20P1Spec
import qualified D22P1Spec
import qualified D22P2Spec
import qualified D23P1Spec

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
      , D6P1Spec.tests
      , D6P2Spec.tests
      , D7P1Spec.tests
      , D7P2Spec.tests
      , D8P1Spec.tests
      , D8P2Spec.tests
      , D9Spec.tests
      , D10Spec.tests
      , D11P1Spec.tests
      -- , D11P2Spec.tests
      , D12Spec.tests
      , D13P1Spec.tests
      , D13P2Spec.tests
      , D14P1Spec.tests
      , D14P2Spec.tests
      , D15P1Spec.tests
      , D15P2Spec.tests
      , D16P1Spec.tests
      -- , D17P1Spec.tests
      , D18P1Spec.tests
      , D19P1Spec.tests
      , D20P1Spec.tests
      , D22P1Spec.tests
      , D22P2Spec.tests
      , D23P1Spec.tests
    ]