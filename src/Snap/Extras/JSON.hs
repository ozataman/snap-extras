{-# LANGUAGE OverloadedStrings #-}

module Snap.Extras.JSON
    ( getBoundedJSON
    , reqBoundedJSON
    , reqJSON
    ) where
    

-------------------------------------------------------------------------------
import           Data.Aeson            as A
import Data.Int
import qualified Data.ByteString.Char8 as B
import           Snap.Core
-------------------------------------------------------------------------------
import           Snap.Extras.CoreUtils
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Demand the presence of JSON in the body assuming it is not larger
-- than 50000 bytes.
reqJSON :: (MonadSnap m, A.FromJSON b) => m b
reqJSON = reqBoundedJSON 50000


-------------------------------------------------------------------------------
-- | Demand the presence of JSON in the body with a size up to N
-- bytes. If parsing fails for any reson, request is terminated early
-- and a server error is returned.
reqBoundedJSON 
    :: (MonadSnap m, FromJSON a)
    => Int64
    -- ^ Maximum size in bytes
    -> m a
reqBoundedJSON n = do
  res <- getBoundedJSON n
  case res of
    Left e -> badReq $ B.pack e
    Right a -> return a


-------------------------------------------------------------------------------
-- | Parse request body into JSON or return an error string.
getBoundedJSON 
    :: (MonadSnap m, FromJSON a) 
    => Int64 
    -- ^ Maximum size in bytes
    -> m (Either String a)
getBoundedJSON n = do
  bodyVal <- A.decode `fmap` readRequestBody n
  return $ case bodyVal of
    Nothing -> Left "Can't find JSON data in POST body"
    Just v -> case A.fromJSON v of
                A.Error e -> Left e
                A.Success a -> Right a

