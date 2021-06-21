{-# LANGUAGE OverloadedStrings #-}
module Base.Digestive where

import qualified Data.ByteString.Lazy    as BL
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Text.Digestive

requiredText :: Monad m => Text -> Maybe Text -> Form Text m Text
requiredText name maybeText =
  check errorMessage (not . T.null) $ text maybeText
  where
    errorMessage = T.unwords [name, "cannot be empty"]

lazyByteString :: Monad m => Text -> Maybe Text -> Form Text m BL.ByteString
lazyByteString name base = validate process (requiredText name base)
  where
    process = Success . TL.encodeUtf8 . TL.pack . T.unpack
