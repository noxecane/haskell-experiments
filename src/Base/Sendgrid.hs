{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Base.Sendgrid where

import           Context             (SendgridConfig (..))
-- import qualified Control.Exception   as E
import           Control.Lens        ((&), (.~), (?~))
import           Data.Aeson          (ToJSON, object, toJSON, (.=))
import           Data.ByteString     (ByteString)
import           Data.HashMap.Strict (HashMap)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
-- import           Network.HTTP.Client (HttpExceptionContent (..))
import           Network.Wreq        (Options)
import qualified Network.Wreq        as Wreq

data Contributor = Contributor
  { email :: Text
  , name  :: Text
  } deriving Generic

type Vars = HashMap Text Text

data Mail = Mail
  { from       :: Contributor
  , to         :: [Contributor]
  , templateId :: Text
  , vars       :: Vars
  }


instance ToJSON Contributor

instance ToJSON Mail where
  toJSON (Mail f t tmpl vs) = object
    [ "personalizations" .=
      [ object
        [ "to" .= t
        , "dynamic_template_data" .= vs
        ]
      ]
    , "from" .= f
    , "template_id" .= tmpl
    ]


sendgridOpts :: ByteString -> Options
sendgridOpts key =
  Wreq.defaults
    & Wreq.header "Accept" .~ ["application/json"]
    & Wreq.auth ?~ Wreq.oauth2Bearer key


-- mailErrorHandler e@(StatusCodeException s _ _)
--       | s ^. Wreq.statusCode == 429 = "Limit reached please try again later"
--       | otherwise              = throwIO e


sendMail :: SendgridConfig -> Mail -> IO ()
sendMail SendgridConfig{ api_key, api_url } mail = do
  let opts = sendgridOpts api_key
  _ <- Wreq.postWith opts api_url (toJSON mail)
  return ()
