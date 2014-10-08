module Main
    ( main
    ) where

-------------------------------------------------------------------------------
import           Test.Tasty
-------------------------------------------------------------------------------
import           Snap.Extras.Tests.MethodOverride (methodOverrideTests)
-------------------------------------------------------------------------------

main = do
  defaultMain $ testGroup "tests"
    [ methodOverrideTests
    ]
