{-# LANGUAGE OverloadedStrings #-}
module Snap.Extras.Tests.MethodOverride
    ( methodOverrideTests
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as B
import           Data.Char
import qualified Data.Map                    as M
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Snap.Core
import           Snap.Extras.MethodOverride
import           Snap.Extras.Tests.Arbitrary ()
import           Snap.Extras.TextUtils
import           Snap.Test
-------------------------------------------------------------------------------

methodOverrideTests = testGroup "Snap.Extras.MethodOverride"
    [ testProperty "leaves method alone with no override" $ \meth ->
       monadicIO $ assertOverride meth meth Nothing
    , testProperty "leaves non-POST alone even with override" $ \(NonPost meth) override ->
        monadicIO $ assertOverride  meth meth (Just $ showMethod override)
    , testProperty "leaves non-POST alone even with override and lowercased param" $ \(NonPost meth) override ->
        monadicIO $ assertOverride  meth meth (Just $ showDCMethod override)
    , testProperty "overrides POST" $ \meth ->
        monadicIO $ assertOverride meth POST (Just $ showMethod meth)
    , testProperty "overrides POST with lowercased method" $ \meth ->
         monadicIO $ assertOverride (lowerMeth meth) POST (Just $ showDCMethod meth)
    ]


-------------------------------------------------------------------------------
newtype NonPost = NonPost Method deriving (Show)

instance Arbitrary NonPost where
  arbitrary = NonPost <$> (arbitrary `suchThat` notPost)
    where
      notPost (POST) = False
      notPost _ = True

-------------------------------------------------------------------------------
showDCMethod = B.map toLower . showMethod

-------------------------------------------------------------------------------
showMethod :: Method -> ByteString
showMethod (Method m) = m
showMethod m          = showBS m


-------------------------------------------------------------------------------
lowerMeth (Method m) = Method (B.map toLower m)
lowerMeth m          = m


-------------------------------------------------------------------------------
assertOverride expectedMeth givenMeth override = do
    actualMeth <- run $ evalHandler builder app
    assert $ expectedMeth == actualMeth
  where
    builder = do
      setRequestType $ RequestWithRawBody givenMeth ""
      case override of
        Just m -> setQueryString $ M.singleton "_method" [m]
        _      -> return ()


-------------------------------------------------------------------------------
app = handleMethodOverride (getsRequest rqMethod)

