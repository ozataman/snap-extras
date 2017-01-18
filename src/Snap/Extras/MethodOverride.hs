{-# LANGUAGE OverloadedStrings #-}

module Snap.Extras.MethodOverride
    ( handleMethodOverride
    , handleMethodOverride'
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative  as A
import           Data.ByteString      (ByteString)
import           Data.CaseInsensitive (mk, original)
import           Data.Maybe           (fromMaybe)
import           Safe                 (headMay)
import           Snap.Core
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- | Wrap a handler with method override support. This means that if
-- (and only if) the request is a POST, _method param is passed, and
-- it is a parsable method name, it will change the request method to
-- the supplied one. This works around some browser limitations with
-- forms. If you use a different parameter name than _method, use
-- handleMethodOverride'
handleMethodOverride :: MonadSnap m
                     => m a
                     -- ^ Internal handler to call
                     -> m a
handleMethodOverride = handleMethodOverride' "_method"


-------------------------------------------------------------------------------
handleMethodOverride' :: MonadSnap m
                      => ByteString
                      -- ^ parameter name for method
                      -> m a
                      -- ^ Internal handler to call
                      -> m a
handleMethodOverride' pn = (modifyRequest (methodOverride pn) >>)


-------------------------------------------------------------------------------
methodOverride :: ByteString -> Request -> Request
methodOverride param r
  | rqMethod r == POST = r { rqMethod = overridden }
  | otherwise          = r
  where
    overridden = fromMaybe POST $ do
      meth <- mk A.<$> (headMay =<< rqParam param r)
      case meth of
       "HEAD"    -> Just HEAD
       "POST"    -> Just POST
       "PUT"     -> Just PUT
       "DELETE"  -> Just DELETE
       "TRACE"   -> Just TRACE
       "OPTIONS" -> Just OPTIONS
       "CONNECT" -> Just CONNECT
       "PATCH"   -> Just PATCH
       ""        -> Nothing
       s         -> Just $ Method $ original s

