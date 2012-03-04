{-# LANGUAGE OverloadedStrings #-}

module Snap.Extras.JSON
    ( reqJSON
    ) where
    

-------------------------------------------------------------------------------
import           Data.Aeson            as A
import qualified Data.ByteString.Char8 as B
import           Snap.Core
-------------------------------------------------------------------------------
import           Snap.Extras.CoreUtils
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Demand the presence of JSON in the body. Terminate request early
-- if not found or unparseable.
reqJSON :: (MonadSnap m, A.FromJSON b) => m b
reqJSON = do
 bodyVal <- A.decode `fmap` readRequestBody 20000
 case bodyVal of
   Nothing -> badReq "Can't find JSON data in POST body"
   Just v -> case A.fromJSON v of
               A.Error e -> badReq $ B.concat ["Can't parse JSON: ", B.pack e]
               A.Success a -> return a
