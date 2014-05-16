{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Snap.Extras.MsgPack
    (
    -- * Parsing MP from Request Body
      getBoundedMP
    , getMP
    , reqBoundedMP
    , reqMP
    , getMPField
    , reqMPField
    -- * Sending MP Data
    , writeMP
    ) where


-------------------------------------------------------------------------------
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Int
import           Data.MessagePack           as MP
import           Snap.Core
-------------------------------------------------------------------------------
import           Snap.Extras.CoreUtils
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | Demand the presence of MP in the body assuming it is not larger
-- than 50000 bytes.
reqMP :: (MonadSnap m, Unpackable b) => m b
reqMP = reqBoundedMP 50000

-------------------------------------------------------------------------------
-- | Demand the presence of MP in the body with a size up to N
-- bytes. If parsing fails for any reson, request is terminated early
-- and a server error is returned.
reqBoundedMP
    :: (MonadSnap m, Unpackable a)
    => Int64
    -- ^ Maximum size in bytes
    -> m a
reqBoundedMP n = do
  res <- getBoundedMP n
  case res of
    Left e -> badReq $ B.pack e
    Right a -> return a

-------------------------------------------------------------------------------
-- | Try to parse request body as MP with a default max size of
-- 50000.
getMP :: (MonadSnap m, Unpackable a) => m (Either String a)
getMP = getBoundedMP 50000

-------------------------------------------------------------------------------
-- | Parse request body into MP or return an error string.
getBoundedMP
    :: (MonadSnap m, Unpackable a)
    => Int64
    -- ^ Maximum size in bytes
    -> m (Either String a)
getBoundedMP n = do
  bodyVal <- A.decode `fmap` readRequestBody n
  return $ case bodyVal of
    Nothing -> Left "Can't find MP data in POST body"
    Just v -> tryUnpack v

-------------------------------------------------------------------------------
-- | Get MP data from the given Param field
getMPField
    :: (MonadSnap m, Unpackable a)
    => B.ByteString
    -> m (Either String a)
getMPField fld = do
  val <- getParam fld
  return $ case val of
    Nothing -> Left $ "Cant find field " ++ B.unpack fld
    Just val' -> tryUnpack val'

-------------------------------------------------------------------------------
-- | Force the MP value from field. Similar to 'getMPField'
reqMPField
    :: (MonadSnap m, Unpackable a)
    => B.ByteString
    -> m a
reqMPField fld = do
  res <- getMPField fld
  case res of
    Left e -> badReq $ B.pack e
    Right a -> return a

-------------------------------------------------------------------------------
-- | Set MIME to 'application/x-msgpack' and write given object into
-- 'Response' body.
writeMP :: (MonadSnap m, Packable a) => a -> m ()
writeMP a = do
  mpResponse
  writeBS . MP.pack $ a
