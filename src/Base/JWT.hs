{-# LANGUAGE OverloadedStrings #-}
module Base.JWT where

import           Context                    (JWTConfig (..))
import           Control.Lens.Operators     ((?~), (^.))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (runExceptT)
import           Crypto.JWT                 (Audience (..), ClaimsSet,
                                             JWTError (..), NumericDate (..),
                                             SignedJWT, StringOrURI)
import qualified Crypto.JWT                 as J
import           Data.Aeson                 (FromJSON, Result (..), ToJSON,
                                             fromJSON, toJSON)
import           Data.ByteString.Lazy       (ByteString)
import           Data.Either                (isRight)
import           Data.Function              ((&))
import qualified Data.HashMap.Strict        as HM
import           Data.Time                  (addUTCTime, getCurrentTime)


data ExtractionError
  = ClaimNotFound
  | JSONDecodeError String


extractData :: (FromJSON a) => ClaimsSet -> Either ExtractionError a
extractData claims =
  let payload = HM.lookup "payload" (claims ^. J.unregisteredClaims)
  in
    case payload of
      Nothing -> Left ClaimNotFound
      Just payload' ->
        case fromJSON payload' of
          Error e        -> Left $ JSONDecodeError e
          Success parsed -> Right parsed


quickExtractData :: (FromJSON a) => JWTConfig -> ByteString -> IO (Maybe a)
quickExtractData conf bytes = do
  claims <- toClaims conf bytes
  return $ case claims of
    Left _ -> Nothing
    Right claims' ->
      case extractData claims' of
        Left _        -> Nothing
        Right payload -> payload


quickSignData :: (ToJSON a) => JWTConfig -> a -> IO (Maybe ByteString)
quickSignData conf = fmap eToM . toJWT conf
  where
    eToM (Left _)  = Nothing
    eToM (Right t) = Just $ J.encodeCompact t


toClaims :: JWTConfig -> ByteString -> IO (Either JWTError ClaimsSet)
toClaims (JWTConfig _ a k) token =
  runExceptT $ do
    let audCheck = (== a)
    token' <- J.decodeCompact token
    J.verifyClaims (J.defaultJWTValidationSettings audCheck) k token'


toJWT :: (ToJSON a) => JWTConfig -> a -> IO (Either JWTError SignedJWT)
toJWT (JWTConfig h a k) d =
  runExceptT $ do
    alg <- J.bestJWSAlg k
    claims <- liftIO $ mkDataClaims a h d
    J.signClaims k (J.newJWSHeader ((), alg)) claims


verifyToken :: JWTConfig -> ByteString -> IO Bool
verifyToken conf = fmap isRight . toClaims conf


mkDataClaims :: (ToJSON a, Real n) => StringOrURI -> n -> a -> IO ClaimsSet
mkDataClaims a h d = do
  now <- getCurrentTime
  let space = realToFrac (h * 60 * 60)
      till = addUTCTime space now
  return $
    J.emptyClaimsSet & J.claimAud ?~ Audience [a] &
    J.claimIat ?~ NumericDate now &
    J.claimExp ?~ NumericDate till &
    J.addClaim "payload" (toJSON d)
