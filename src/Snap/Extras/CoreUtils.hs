{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Snap.Extras.CoreUtils
    ( finishEarly
    , badReq
    , notFound
    , serverError
    , plainResponse
    , jsonResponse
    , jsResponse
    , easyLog
    , getParam'
    , reqParam
    ) where

-------------------------------------------------------------------------------
import Snap.Core
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Control.Monad
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- | Discard anything after this and return given status code to HTTP
-- client immediately.
finishEarly :: MonadSnap m => Int -> ByteString -> m b 
finishEarly code str = do
  modifyResponse $ setResponseStatus code str
  modifyResponse $ addHeader "Content-Type" "text/plain"
  writeBS str
  getResponse >>= finishWith


-------------------------------------------------------------------------------
-- | Finish early with error code 400
badReq :: MonadSnap m => ByteString -> m b 
badReq = finishEarly 400 


-------------------------------------------------------------------------------
-- | Finish early with error code 404
notFound :: MonadSnap m => ByteString -> m b 
notFound = finishEarly 404


-------------------------------------------------------------------------------
-- | Finish early with error code 500
serverError :: MonadSnap m => ByteString -> m b 
serverError = finishEarly 500


-------------------------------------------------------------------------------
-- | Mark response as 'text/plain'
plainResponse :: MonadSnap m => m ()
plainResponse = modifyResponse $ setHeader "Content-Type" "text/plain"


-------------------------------------------------------------------------------
-- | Mark response as 'application/json'
jsonResponse :: MonadSnap m => m ()
jsonResponse = modifyResponse $ setHeader "Content-Type" "application/json"


-------------------------------------------------------------------------------
-- | Mark response as 'application/javascript'
jsResponse :: MonadSnap m => m ()
jsResponse = modifyResponse $ setHeader "Content-Type" "application/javascript"


------------------------------------------------------------------------------
-- | Easier debug logging into error log. First argument is a
-- category/namespace and the second argument is anything that has a
-- 'Show' instance.
easyLog :: (Show t, MonadSnap m) => String -> t -> m ()
easyLog k v = logError . B.pack $ ("[Debug] " ++ k ++ ": " ++ show v)


-------------------------------------------------------------------------------
-- | Alternate version of getParam that considers empty string Nothing
getParam' :: MonadSnap m => ByteString -> m (Maybe ByteString)
getParam' = return . maybe Nothing f <=< getParam
    where f "" = Nothing
          f x = Just x


-------------------------------------------------------------------------------
-- | Require that a parameter is present or terminate early.
reqParam :: (MonadSnap m) => ByteString -> m ByteString
reqParam s = do
  p <- getParam s
  maybe (badReq $ B.concat ["Required parameter ", s, " is missing."]) return p
 
