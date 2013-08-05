{-# LANGUAGE OverloadedStrings #-}


module Snap.Extras.RestUtils
    ( AuthenticationRejection(..)
    , AuthPolicy(..)
    , withRestAuth) where

-------------------------------------------------------------------------------
import Snap.Snaplet
import Snap.Extras.CoreUtils (finishEarly)
import Data.ByteString (ByteString)
-------------------------------------------------------------------------------


data AuthenticationRejection = AuthenticationRejection
    { rejectionCode :: Int
      -- ^ The HTTP status code of the failure
    , rejectionMessage :: ByteString
      -- ^ The rejection message
    } deriving (Show)


--------------------------------------------------------------------------------
-- | Specify the logic the authenticator will run in order to decide whether the
-- route should be evaluated or rejected. It makes extremely simple writing
-- authenticator like the following:
--
-- tokenAuthenticator :: AuthPolicy b v
-- tokenAuthenticator = AuthPolicy $ hdlr -> do
--   req <- getRequest
--   let token = getHeader "Authorization" req
--   return (maybe rejection lookupFn token)
--   where
--     rejection = Left (AuthenticationRejection 500 "Invalid token.")
--     lookupFn t = if t == "SuperSecretToken" then Right () else rejection
newtype AuthPolicy b v = AuthPolicy
    { runPolicy :: Handler b v () ->
                   Handler b v (Either AuthenticationRejection ()) }


-------------------------------------------------------------------------------
-- | Restful Authenticator, inspired to [Spray](http://spray.io/). The idea
-- is to have an handler combinator which, given an authentication policy, is
-- able to determine if the HTTP request should be "allowed" to continue or
-- should be rejected with a particular error. This is useful
-- when developing restful services, which typically requires some sort of
-- "low level authentication", for example an OAuth token to be passed in the
-- HTTP header.
withRestAuth :: AuthPolicy b v -> Handler b v () -> Handler b v ()
withRestAuth policy hdlr = runPolicy policy hdlr >>= \res -> either
    (\rej -> finishEarly (rejectionCode rej) (rejectionMessage rej))
    (const hdlr) res
