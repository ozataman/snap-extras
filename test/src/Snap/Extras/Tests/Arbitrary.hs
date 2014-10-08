module Snap.Extras.Tests.Arbitrary where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Char
import           Data.ByteString.Char8                  (ByteString, pack)
import           Snap.Core
-------------------------------------------------------------------------------
import           Test.QuickCheck
-------------------------------------------------------------------------------


instance Arbitrary Method where
  arbitrary = oneof [ pure HEAD
                    , pure POST
                    , pure PUT
                    , pure DELETE
                    , pure TRACE
                    , pure OPTIONS
                    , pure CONNECT
                    , pure PATCH
                    , Method <$> (pack <$> cleanString)
                    ]
    where
      cleanString = do
        NonEmpty s <- arbitrary
        return s


