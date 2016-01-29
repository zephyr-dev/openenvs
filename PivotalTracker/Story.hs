{-# LANGUAGE OverloadedStrings #-}
module PivotalTracker.Story(storyIdsFromCommits, getStory, PivotalStory(..), storyAccepted) where
import Control.Monad.Trans.Either(right, left, runEitherT, EitherT)
import Control.Exception as E
import Data.Time.Clock      (UTCTime)
import PivotalTracker.Types(PivotalStory(..))
import Data.Aeson.Lens (_String, key)
import qualified Data.Text as T
import Data.Time.Format     (parseTime)
import Data.Time.Format(defaultTimeLocale)
import Control.Monad.Trans(liftIO)
import Network.HTTP.Conduit(HttpException(StatusCodeException) )
import qualified Data.ByteString.Char8 as BCH
import Control.Lens((&), (.~), (^.))
import qualified Data.List as DL
import qualified Text.Regex as TR
import qualified Data.Maybe as MB
import Data.Maybe(Maybe)
import qualified Network.HTTP.Types as NHT
import Network.Wreq(defaults, header, getWith, responseBody)

type EIO = EitherT String IO

storyAccepted :: PivotalStory -> Bool
storyAccepted story = pivotalStoryStatus story == "accepted" || pivotalStoryStatus story == "invalid_story_id"

textToDate  :: String -> Maybe UTCTime
textToDate  = parseTime defaultTimeLocale "%FT%X%QZ"

storyIdsFromCommits :: [String] -> [String]
storyIdsFromCommits = DL.nub . concat . (MB.mapMaybe parseStoryId) where
  parseStoryId :: String -> Maybe [String]
  parseStoryId commitMessage = TR.matchRegex (TR.mkRegex "#([0-9]+)") commitMessage

getStory :: String -> String -> EIO PivotalStory
getStory token storyId = do
  let options = defaults & header "X-TrackerToken" .~ [BCH.pack token]
  res <- liftIO $ tryRequest (getWith options $ "https://www.pivotaltracker.com/services/v5/stories/" ++ storyId)
  case res of
    Right response -> do
      let updatedAt = textToDate . T.unpack $ response ^. responseBody . key "updated_at" . _String
      let state =  response ^. responseBody . key "current_state" . _String
      return $ PivotalStory state updatedAt
    Left (StatusCodeException status headers _) -> do
      case NHT.statusCode status of
        403 -> return $ PivotalStory "invalid_story_id" Nothing
        404 -> return $ PivotalStory "invalid_story_id" Nothing
        statusCode   -> return $ PivotalStory "not_accepted" Nothing
  where
    tryRequest :: IO a ->  IO (Either HttpException a)
    tryRequest = E.try
