{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Base.Scotty where

import           Control.Concurrent.STM              (TBQueue, atomically,
                                                      readTBQueue)
import           Control.Monad.Trans                 (liftIO)
import           Data.Aeson                          (FromJSON, ToJSON)
import qualified Data.Aeson                          as Json
import           Data.Binary.Builder                 (fromByteString)
import           Data.ByteString                     (ByteString)
import           Data.Function                       (fix)
import           Data.Maybe                          (fromJust)
import           Data.String.Conversions             (cs)
import           Data.Text                           (Text)
import           Database.Redis                      (Connection)
import qualified Database.Redis                      as Redis
import           GHC.Generics                        (Generic)
import qualified Network.HTTP.Types.Method           as HTTP
import qualified Network.HTTP.Types.Status           as HTTP
import           Network.Wai                         (StreamingBody)
import qualified Network.Wai                         as WAI
import           Network.Wai.EventSource             (ServerEvent (..))
import qualified Network.Wai.EventSource.EventStream as Sse
import           Network.Wai.Middleware.Cors         (CorsResourcePolicy (..))
import qualified Network.Wai.Middleware.Cors         as Cors
import           System.RandomString                 (Alphabet (..),
                                                      StringOpts (..))
import qualified System.RandomString                 as Rand
import           Text.Digestive                      (Form)
import qualified Text.Digestive.Aeson                as Dig
import           Web.Cookie                          (SetCookie (..))
import qualified Web.Cookie                          as Cookie
import           Web.Scotty                          (ActionM)
import qualified Web.Scotty                          as Scotty
import qualified Web.Scotty.Cookie                   as Scotty


newtype Message = Message { message :: Text }
  deriving Generic

instance ToJSON Message

getJson :: Form Text Scotty.ActionM a -> ActionM a
getJson form = do
  value <- Scotty.jsonData
  (v, m) <- Dig.digestJSON form value
  case m of
    Just m' -> return m'
    Nothing -> do
      Scotty.status HTTP.status422
      Scotty.json $ Dig.jsonErrors v
      Scotty.finish


complexCors :: [Cors.Origin] -> WAI.Middleware
complexCors origins = Cors.cors (const $ Just complexCorsResourcePolicy)
  where
    complexCorsResourcePolicy = Cors.simpleCorsResourcePolicy
      { corsOrigins = Just (origins, True)
      , corsMethods =
        [ HTTP.methodHead
        , HTTP.methodGet
        , HTTP.methodPut
        , HTTP.methodPatch
        , HTTP.methodPost
      ]
      , corsRequireOrigin = True
      }


createSession :: ToJSON a => Connection -> ByteString -> a -> ActionM ()
createSession db cookieName value = do
  sessionId <- liftIO $ Rand.randomString $ StringOpts Base16 32
  let encoded = Json.encode value
      cookie = serverCookie cookieName sessionId
  _ <- liftIO $ Redis.runRedis db $
    Redis.set sessionId $ cs encoded
  Scotty.setCookie $ cookie { setCookieSecure = False }


getSession :: FromJSON a => Connection -> Text -> ActionM a
getSession db cookieName = do
  maybeId<- Scotty.getCookie cookieName
  case maybeId of
    Nothing  -> unauthorized "You don't have a session"
    Just sId -> do
      Right maybeSession <- liftIO $ Redis.runRedis db $ Redis.get (cs sId)
      case maybeSession of
        Nothing      -> unauthorized "You don't have a session"
        Just session -> return $ fromJust $ Json.decode $ cs session
  where
    unauthorized msg = do
      Scotty.status HTTP.status401
      Scotty.json $ Message msg
      Scotty.finish


serverCookie :: ByteString -> ByteString -> SetCookie
serverCookie n v = Cookie.def
  { setCookieName = n
  , setCookieValue = v
  , setCookiePath = Just "/"
  , setCookieHttpOnly = True
  , setCookieSecure = True
  }


sseHeaders :: ActionM ()
sseHeaders = do
  Scotty.addHeader "X-Accel-Buffering" "no"
  Scotty.addHeader "Cache-Control" "no-cache"
  Scotty.addHeader "Content-Type" "text/event-stream"


sseData :: ByteString -> ServerEvent
sseData msg = ServerEvent
  { eventName = Nothing
  , eventId = Nothing
  , eventData = [fromByteString msg]
  }


sseEvent :: ByteString -> ByteString -> ServerEvent
sseEvent event msg = ServerEvent
  { eventName = Just $ fromByteString event
  , eventId = Nothing
  , eventData = [fromByteString msg]
  }


sseClose :: ServerEvent
sseClose = CloseEvent


sseStreaming :: TBQueue ServerEvent -> StreamingBody
sseStreaming src sendChunk flush = fix $ \loop ->
  do
    result <- atomically $ readTBQueue src
    case Sse.eventToBuilder result of
      Nothing  -> return ()
      Just ans -> sendChunk ans >> flush >> loop
