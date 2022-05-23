{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Taggant
  ( Taggant(..),
  Config(..),
  collectHeaders,
  Context(..),
  defaultConfig,
  middleware,
  fromVault,
  fromWaiRequest,
  alterClientRequest,
  alterClientManager,
  )
  where

import Data.Aeson as J
import Data.CaseInsensitive (CI, foldedCase)
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KM
#else
import qualified Data.HashMap.Strict as KM
#endif
import qualified Data.Map.Strict as M
import qualified Data.Vault.Lazy as Vault
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Network.HTTP.Client.Internal as HC
import Network.Wai
import System.IO.Unsafe

newtype Taggant = Taggant { unTaggant :: J.Value }
  deriving (Eq, Ord, FromJSON, ToJSON)

instance Show Taggant where
  show = BL.unpack . J.encode

instance Monoid Taggant where
  mempty = Taggant Null

instance Semigroup Taggant where
  Taggant a <> Taggant b = Taggant $ mergeValues a b

mergeValues :: Value -> Value -> Value
mergeValues (Array xs) (Array ys) = Array $ xs <> ys
mergeValues (Object xs) (Object ys) = Object $ KM.unionWith mergeValues xs ys
mergeValues Null x = x
mergeValues x Null = x
mergeValues (Array xs) y = Array (xs <> pure y)
mergeValues x (Array ys) = Array (pure x <> ys)
mergeValues x y = Array $ pure x <> pure y

vaultKey :: Vault.Key Context
vaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE vaultKey #-}

collectHeaders :: [CI B.ByteString] -> Request -> Taggant
collectHeaders names req = Taggant $ toJSON $ M.fromList
  [ (lenientDecode $ foldedCase k, fmap lenientDecode $ lookup k $ requestHeaders req)
  | k <- names
  ]

data Config = Config
  { headerName :: CI B.ByteString
  , fromRequest :: Request -> Taggant
  -- ^ custom function that obtains a taggant from a wai 'Request'.
  }

defaultConfig :: Config
defaultConfig = Config
  { headerName = "X-TAGGANT"
  , fromRequest = mempty
  }

data Context = Context
  { config :: Config
  , taggant :: Taggant
  }

instance Show Context where
  show = show . taggant

instance ToJSON Context where
  toJSON = toJSON . taggant

lenientDecode :: B.ByteString -> T.Text
lenientDecode = T.decodeUtf8With T.lenientDecode

parseTaggant :: B.ByteString -> Taggant
parseTaggant bs = Taggant $ case J.decode $ BL.fromStrict bs of
  Nothing -> J.String $ lenientDecode bs
  Just obj -> obj

middleware :: Config -> Middleware
middleware config@Config{..} app req sendResp = do
  let tag = maybe mempty parseTaggant $ lookup headerName $ requestHeaders req
  let cxt = Context config $ tag <> fromRequest req
  app
    req { vault = Vault.insert vaultKey cxt (vault req) }
    sendResp

fromVault :: Vault.Vault -> Maybe Context
fromVault = Vault.lookup vaultKey

fromWaiRequest :: Request -> Maybe Context
fromWaiRequest = fromVault . vault

alterClientRequest :: Maybe Context -> HC.Request -> HC.Request
alterClientRequest Nothing req = req
alterClientRequest (Just Context{..}) req = req
    { HC.requestHeaders
      = (headerName config, BL.toStrict $ J.encode taggant)
      : HC.requestHeaders req
    }

alterClientManager :: Maybe Context -> HC.Manager -> HC.Manager
alterClientManager cxt man = man
  { HC.mModifyRequest = HC.mModifyRequest man . alterClientRequest cxt }