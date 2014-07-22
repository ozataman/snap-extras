{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

{-|

This module provides infrastructure for polling the status of processes that
run longer than a single request-response lifecycle.  There are two main
components necessary to use this library: routes and splices.

The routes are set up by the 'jobsHandler' function.  Typically you'll add it
to your site's list of routes like this:

> route [ ...
>       , ("myJob", jobsHandler myJobStarter myJobStatusPage
>                               "myJobUpdateStatusTemplate" ".jobStatusDiv")
>       , ...
>       ]

You will also usually want to bind the three splices provided by this module.

> splices = do
>     ...
>     "jobId" ## jobIdSplice
>     "jobUpdateJs" ## updateSplice
>     "myJobStatus" ## statusSplice getMyJobStatus

You need to bind the last splice once for each type of action that you are
polling, each with its own function for getting the job status.

The third argument to jobsHandler is the template that will be rendered for
status update requests.  It will typically be not much more than a
@\<myJobStatus\>@ tag enclosing the custom markup for displaying your job status.
Usually the last thing in that tag will be something like the following:

> <ifNotFinished>
>   <jobUpdateJs interval="500" jobId="${jobId}"/>
> </ifNotFinished>

This will poll for updates every 500 milliseconds.  Notice that we're using
the jobId splice to get seamless handling of the job ID.  This is a
convenient convention, but you can use a custom method for that if you want.
See the documentation for 'statusSplice' for more information about the
splices available to you inside the @\<myJobStatus\>@ tag.

-}

module Snap.Extras.PollStatus where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Applicative
import           Control.Error
import           Control.Monad.Trans
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as B
import           Data.Maybe
import           Data.Monoid
import           Data.Readable
import           Data.Text
import qualified Data.Text                    as T
import           Data.Text.Encoding
import           Data.Time
import           Heist
import           Heist.Compiled
import           Heist.SpliceAPI
import           Heist.Splices
import           Language.Javascript.JMacro
import           Safe
import           Snap.Core
import           Snap.Extras.CoreUtils
import           Snap.Snaplet
import           Snap.Snaplet.Heist.Compiled  (HasHeist, render, withHeistState)
import           Text.PrettyPrint.Leijen.Text (renderOneLine)
import qualified Text.XmlHtml                 as X
------------------------------------------------------------------------------
import           Snap.Extras.Ajax
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Top-level handler that sets up routes for the job polling infrastructure.
-- This handler sets up three sub-routes: start, status, and updateStatus.
jobsHandler
    :: HasHeist b
    => Handler b b JobId
    -- ^ Handler that starts a job and returns a unique JobId.
    -> Handler b b ()
    -- ^ Handler for the status page.  This handler gets called with the
    -- \"jobId\" query string param set to the job id returned from the job
    -- start handler.
    -> ByteString
    -- ^ Name of the template to use for the updateStatus endpoint.
    -> T.Text
    -- ^ A CSS selector string used to substitute the contents of the AJAX
    -- template into the status page.
    -> Handler b b ()
jobsHandler startJob statusPageHandler updateTemplate selector = do
    route [ ("start", startHandler)
          , ("status", statusPageHandler)
          , ("updateStatus", updater)
          ]
  where
    startHandler = do
        jobId <- startJob
        redirect $ encodeUtf8 $ "status?jobId=" <> jobId
    updater = replaceWithTemplate updateTemplate selector


------------------------------------------------------------------------------
-- | A simple convenience splice to standardize the way templates get job IDs.
-- This splice gets the job ID from the request param \"jobId\".
jobIdSplice :: Splice (Handler b b)
jobIdSplice = return $ yieldRuntimeText $ lift $ do
    bs <- getParam "jobId"
    return $ maybe "" decodeUtf8 bs


------------------------------------------------------------------------------
-- | Function to generate a status splice.  This splice makes the following
-- splices available to its children:
--
-- Conditional splices:
-- ifPending, ifRunning, ifNotFinished, ifFinished, ifFinishedSuccess,
-- ifFinishedFailure
--
-- Data splices:
-- elapsedSeconds, startTime, endTime, jobState, messages, percentCompleted,
-- amountCompleted, amountTotal
statusSplice
    :: MonadSnap n
    => (T.Text -> n (Maybe Status))
    -> Splice n
statusSplice getReport =
    mayDeferMap return (withSplices runChildren statusSplices) $ lift $ do
        ts <- liftIO getCurrentTime
        sr <- runMaybeT $ do
            mbs <- MaybeT $ getParam "jobId"
            MaybeT $ getReport $ decodeUtf8 mbs
        return $ fmap (ts,) sr


------------------------------------------------------------------------------
-- | Splice that inserts the javascript required to send an AJAX updateStatus
-- request.  Takes two attributes:
--
-- interval - Delay in milliseconds before sending the request (default 1000)
--
-- jobId - Required attribute with the id of the job.  (Usually \"${jobId}\"
-- should work fine.)
updateSplice :: Splice (Handler b b)
updateSplice = do
    n <- getParamNode
    runAttrs <- runAttributesRaw $ X.elementAttrs n
    case X.getAttribute "jobId" n of
      Nothing -> error $ T.unpack (X.elementTag n) ++ " must have a jobId attribute!"
      _ -> return ()
    return $ yieldRuntimeText $ do
        attrs <- runAttrs
        let delay :: Int = fromMaybe 1000 $ fromText =<< lookup "interval" attrs
            jobId :: Text = fromMaybe "" $ lookup "jobId" attrs
            js = updateJS jobId delay
        return $ T.concat ["<script>", js, "</script>"]


------------------------------------------------------------------------------
type JobId = Text


------------------------------------------------------------------------------
-- | Enumeration of the states a job can be in.
data JobState = Pending | Running | FinishedSuccess | FinishedFailure
  deriving (Show,Eq)


------------------------------------------------------------------------------
-- | Returns a bool indicating whether the job is finished.
isFinished :: JobState -> Bool
isFinished FinishedSuccess = True
isFinished FinishedFailure = True
isFinished _ = False


------------------------------------------------------------------------------
-- | The complete status of a job.
data Status = Status
    { srStartTime       :: UTCTime
    , srJobState        :: JobState
    , srEndTime         :: Maybe UTCTime
    , srMessages        :: [Text]
    , srAmountCompleted :: Double
    , srAmountTotal     :: Double
    }


------------------------------------------------------------------------------
-- | Calculates the percent completed as an Int.
statusPercentCompleted :: Status -> Int
statusPercentCompleted Status{..} =
    round $ 100.0 * srAmountCompleted / srAmountTotal


------------------------------------------------------------------------------
tshow :: Show a => a -> Text
tshow = T.pack . show


------------------------------------------------------------------------------
-- | The status splices
statusSplices
    :: Monad n
    => Splices (RuntimeSplice n (UTCTime, Status) -> Splice n)
statusSplices = do
    "ifPending" ## ifCSplice ((==Pending) . srJobState . snd)
    "ifRunning" ## ifCSplice ((==Running) . srJobState . snd)
    "ifNotFinished" ## ifCSplice (not . isFinished . srJobState . snd)
    "ifFinished" ## ifCSplice (isFinished . srJobState . snd)
    "ifFinishedSuccess" ## ifCSplice ((==FinishedSuccess) . srJobState . snd)
    "ifFinishedFailure" ## ifCSplice ((==FinishedFailure) . srJobState . snd)
    mapS pureSplice $ do
      "elapsedSeconds" ## textSplice (tshow . getElapsed)
      "startTime" ## textSplice (tshow . srStartTime . snd)
      "endTime" ## textSplice (tshow . srEndTime . snd)
      "jobState" ## textSplice (tshow . srJobState  . snd)
      "messages" ## textSplice (tshow . srMessages . snd)
      "percentCompleted" ## textSplice (tshow . statusPercentCompleted . snd)
      "amountCompleted" ## textSplice (tshow . srAmountCompleted . snd)
      "amountTotal" ## textSplice (tshow . srAmountTotal . snd)


------------------------------------------------------------------------------
getElapsed :: (UTCTime, Status) -> Int
getElapsed (ts,sr) = round $ diffUTCTime ts (srStartTime sr)


------------------------------------------------------------------------------
-- | JS code to do an AJAX request to updateStatus after a delay.
updateJS
    :: Text
    -- ^ The job ID
    -> Int
    -- ^ The amount of time to wait before requesting the page in milliseconds
    -> Text
updateJS jobId delay = T.pack $ show $ renderOneLine $ renderJs $
    [jmacro|
      setTimeout(function() {
        $.get('updateStatus', {jobId: `(T.unpack jobId)`});
      }, `(show delay)`);
    |]

