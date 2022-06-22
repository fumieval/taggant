{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Taggant
  ( Taggant(..),
  headerName,
  -- * wai
  middleware,
  fromVault,
  fromWaiRequest,
  -- * http-client
  alterClientRequest,
  alterClientManager,
  -- * Utility
  collectHeaders,
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

instance J.KeyValue Taggant where
  k .= v = Taggant $ J.Object $ k .= v

mergeValues :: Value -> Value -> Value
mergeValues (Array xs) (Array ys) = Array $ xs <> ys
mergeValues (Object xs) (Object ys) = Object $ KM.unionWith mergeValues xs ys
mergeValues Null x = x
mergeValues x Null = x
mergeValues (Array xs) y = Array (xs <> pure y)
mergeValues x (Array ys) = Array (pure x <> ys)
mergeValues x y = Array $ pure x <> pure y

vaultKey :: Vault.Key Taggant
vaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE vaultKey #-}

-- | Construct a 'Taggant' from a set of headers
collectHeaders :: [CI B.ByteString] -> Request -> Taggant
collectHeaders names req = Taggant $ toJSON $ M.fromList
  [ (lenientDecode $ foldedCase k, fmap lenientDecode $ lookup k $ requestHeaders req)
  | k <- names
  ]

-- | The name of the taggant header (@"X-TAGGANT"@).
headerName :: CI B.ByteString
headerName = "X-TAGGANT"

lenientDecode :: B.ByteString -> T.Text
lenientDecode = T.decodeUtf8With T.lenientDecode

parseTaggant :: B.ByteString -> Taggant
parseTaggant bs = Taggant $ case J.decode $ BL.fromStrict bs of
  Nothing -> J.String $ lenientDecode bs
  Just obj -> obj

middleware :: Middleware
middleware app req sendResp = do
  let tag = maybe mempty parseTaggant $ lookup headerName $ requestHeaders req
  app
    req { vault = Vault.insert vaultKey tag (vault req) }
    sendResp

fromVault :: Vault.Vault -> Taggant
fromVault = maybe mempty id . Vault.lookup vaultKey

fromWaiRequest :: Request -> Taggant
fromWaiRequest = fromVault . vault

alterClientRequest :: Taggant -> HC.Request -> HC.Request
alterClientRequest taggant req = req
  { HC.requestHeaders
    = (headerName, BL.toStrict $ J.encode taggant)
    : HC.requestHeaders req
  }

alterClientManager :: Taggant -> HC.Manager -> HC.Manager
alterClientManager cxt man = man
  { HC.mModifyRequest = HC.mModifyRequest man . alterClientRequest cxt }