module Hyper.Node.Session.Cookie where
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..), joinWith, split)
import Hyper.Session (SessionID(..), class SessionStore)
import Node.Buffer (BUFFER)
import Node.Crypto (CRYPTO)
import Node.Crypto.Cipher as Cipher
import Node.Crypto.Decipher as Decipher
import Node.Crypto.Hash as Hash
import Node.Crypto.Hmac as Hmac
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)

foreign import randString :: forall e. Eff (buffer :: BUFFER, crypto :: CRYPTO | e) String

type Key = { hmacKey :: String, cipherKey :: String }

mkSecret :: forall e m. MonadEff (buffer :: BUFFER, crypto :: CRYPTO | e) m => m Key
mkSecret = do
  hmacKey <- liftEff randString
  cipherKey <- liftEff randString
  pure $ { hmacKey, cipherKey }

newtype CookieStore session = CookieStore Key
derive instance newtypeCookieStore :: Newtype (CookieStore session) _

encrypt :: forall e m. MonadEff (crypto :: CRYPTO, buffer :: BUFFER | e) m => Key -> String -> m String
encrypt { cipherKey, hmacKey } text = do
  encrypted <- liftEff $ Cipher.hex Cipher.AES256 cipherKey text
  hmac <- liftEff $ Hmac.hex Hash.SHA512 hmacKey encrypted
  pure $ joinWith "," [hmac, encrypted]

decrypt :: forall e m. MonadEff (crypto :: CRYPTO, buffer :: BUFFER | e) m => Key -> String -> m (Maybe String)
decrypt { cipherKey, hmacKey } text = case split (Pattern ",") text of
  [hmac, encrypted] ->
    let
      calcHmac = liftEff $ Hmac.hex Hash.SHA512 hmacKey encrypted
      decryptWhen hmac' | hmac == hmac' =
        Just <$> liftEff (Decipher.fromHex Cipher.AES256 cipherKey encrypted)
      decryptWhen _ = pure Nothing
    in
      calcHmac >>= decryptWhen
  _ -> pure Nothing

instance sessionStoreCookieStore ::
  ( ReadForeign session
  , WriteForeign session
  , Monad m
  , MonadEff (buffer :: BUFFER, crypto :: CRYPTO | e) m
  ) =>
  SessionStore (CookieStore session) m session where
  newSessionID _ = pure $ SessionID "new-id"
  get store id = do
    text <- decrypt (unwrap store) $ unwrap id
    pure $ text >>= readJSON >>> hush
  put store _ session = SessionID <$> encrypt (unwrap store) json
    where json = writeJSON session
  delete store _ = pure unit
