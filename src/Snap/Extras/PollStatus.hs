{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

{-|

This module provides infrastructure for polling the status of processes that
run longer than a single request-response lifecycle.  To do this, we issue
AJAX calls at regular intervals to a route that updates the status on the
page.  There are two main components necessary to use this library: routes and
splices.

The routes are set up by the 'jobsHandler' function.  Typically you'll add it
to your site's list of routes like this:

> route [ ...
>       , ("myJob", jobsHandler myJobStarter myJobStatusPage
>                               "myJobUpdateStatusTemplate" ".statusdiv")
>       , ...
>       ]

The jobsHandler function binds three routes:

* myJob/start

* myJob/status

* myJob/updateStatus

start and status are the pages you will use for starting a job and showing the
user the job status.  The updateStatus page is used internally for the
individual status refreshes, so you probably will never need to use it
directly.

You will also need to bind the main splice provided by this module.

> splices = do
>     ...
>     "myJobStatus" ## statusSplice getMyJobStatus

You need to bind this splice once for each type of action that you are
polling, each with its own splice name and function for getting the job
status.

There is an optional jobIdSplice available if you need direct access to the
job ID in templates, but this should not be needed for normal use.

The third argument to jobsHandler is the template that will be rendered for
status update requests.  It will typically be not much more than a
@\<myJobStatus\>@ tag enclosing the custom markup for displaying your job status.
Here's an example using a bootstrap progress bar:

> <myJobStatus interval="300">
>   <ifRunning>
>     <elapsedSeconds/> seconds elapsed
>
>     <div class="progress">
>       <div class="progress-bar progress-bar-striped active"  role="progressbar"
>            aria-valuenow="${amountCompleted}" aria-valuemin="0"
>            aria-valuemax="${amountTotal}" style="width: ${percentCompleted}%">
>         <percentCompleted/>%
>       </div>
>     </div>
>
>   </ifRunning>
>   <ifFinished>
>     <div class="progress">
>       <div class="progress-bar progress-bar-striped"  role="progressbar"
>            aria-valuenow="${amountCompleted}" aria-valuemin="0"
>            aria-valuemax="${amountTotal}" style="width: ${percentCompleted}%">
>         <percentCompleted/>%
>       </div>
>     </div>
>   </ifFinished>
> </myJobStatus>

This will poll for updates every 300 milliseconds.  By default the myJobStatus
splice gets the job ID from the \"jobId\" param.  You can override this by
specifying a \"jobId\" attribute in the myJobStatus tag.  See the
documentation for 'statusSplice' for more information about the splices
available to you inside the @\<myJobStatus\>@ tag.

To get the above code working, you would have the myJobStatusPage handler
return markup that contains something like this:

> <h1>Status</h1>
> <div class="statusdiv col-md-4">
>   <apply template="myJobUpdateStatusTemplate"/>
> </div>

-}

module Snap.Extras.PollStatus
  ( jobsHandler
  , jobIdSplice
  , statusSplice
  , JobId
  , JobState(..)
  , isFinished
  , Status(..)
  , statusPercentCompleted
  , statusElapsed
  , statusSplices
  ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import qualified Blaze.ByteString.Builder.Char8 as BB
import           Control.Applicative
import           Control.Error
import           Control.Monad.Trans
import           Data.ByteString                (ByteString)
import           Data.Monoid
import           Data.Readable
import           Data.Text
import qualified Data.Text                      as T
import           Data.Text.Encoding
import           Data.Time
import           Heist
import           Heist.Compiled
import           Heist.Splices
import           Language.Javascript.JMacro
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist.Compiled    (HasHeist)
import           Text.PrettyPrint.Leijen.Text   (renderOneLine)
import qualified Text.XmlHtml                   as X
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
-- | Internal type to make splice code clearer.
data StatusData = StatusData
    { sdTimestamp :: UTCTime
    , sdStatus    :: Status
    , sdJs        :: Text
    }


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
statusSplice getReport = do
    n <- getParamNode
    runAttrs <- runAttributesRaw $ X.elementAttrs n

    let nodes = X.elementChildren n ++ [X.Element "updateJs" [] []]
        run = runNodeList nodes
    mayDeferMap return (withSplices run statusSplices) $ do
        attrs <- runAttrs
        ts <- liftIO getCurrentTime
        let delay = fromMaybe 1000 $ fromText =<< lookup "interval" attrs
            mjobId = lookup "jobId" attrs
        runMaybeT $ do
            jobId <- maybe (decodeUtf8 <$> MaybeT (lift $ getParam "jobId"))
                           return mjobId
            let js = T.concat ["<script>", updateJS jobId delay, "</script>"]
            s <- MaybeT $ lift $ getReport jobId
            return $ StatusData ts s js


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
    { statusStartTime       :: UTCTime
    , statusJobState        :: JobState
    , statusEndTime         :: Maybe UTCTime
    , statusMessages        :: [Text]
    , statusAmountCompleted :: Double
    , statusAmountTotal     :: Double
    }


------------------------------------------------------------------------------
-- | Calculates the percent completed as an Int.
statusPercentCompleted :: Status -> Int
statusPercentCompleted Status{..} =
    round $ 100.0 * statusAmountCompleted / statusAmountTotal


------------------------------------------------------------------------------
tshow :: Show a => a -> Text
tshow = T.pack . show


------------------------------------------------------------------------------
-- | The status splices
statusSplices
    :: Monad n
    => Splices (RuntimeSplice n StatusData -> Splice n)
statusSplices = do
    "ifPending" ## ifCSplice ((==Pending) . statusJobState . sdStatus)
    "ifRunning" ## ifCSplice ((==Running) . statusJobState . sdStatus)
    "ifNotFinished" ## ifCSplice (not . isFinished . statusJobState . sdStatus)
    "ifFinished" ## ifCSplice (isFinished . statusJobState . sdStatus)
    "ifFinishedSuccess" ## ifCSplice ((==FinishedSuccess) . statusJobState . sdStatus)
    "ifFinishedFailure" ## ifCSplice ((==FinishedFailure) . statusJobState . sdStatus)
    mapS pureSplice $ do
      "elapsedSeconds" ## textSplice (tshow . statusElapsed)
      "startTime" ## textSplice (tshow . statusStartTime . sdStatus)
      "endTime" ## textSplice (tshow . statusEndTime . sdStatus)
      "jobState" ## textSplice (tshow . statusJobState  . sdStatus)
      "messages" ## textSplice (tshow . statusMessages . sdStatus)
      "percentCompleted" ## textSplice (tshow . statusPercentCompleted . sdStatus)
      "amountCompleted" ## textSplice (tshow . statusAmountCompleted . sdStatus)
      "amountTotal" ## textSplice (tshow . statusAmountTotal . sdStatus)

      -- I didn't mention this splice in the docs because I always add it onto
      -- the end of the status splice children, so the end user should never
      -- need it.
      "updateJs" ## internalUpdate
  where
    internalUpdate :: StatusData -> Builder
    internalUpdate StatusData{..} =
        if not $ isFinished $ statusJobState sdStatus
          then BB.fromText sdJs
          else mempty


------------------------------------------------------------------------------
statusElapsed :: StatusData -> Int
statusElapsed StatusData{..} =
    round $ diffUTCTime sdTimestamp (statusStartTime sdStatus)


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

