{-# LANGUAGE OverloadedStrings #-}

module BaseConvert.JSON where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B

import BaseConvert.Bases

instance FromJSON Base where
  parseJSON (Object v) = Base
    <$> (v .: "name")
    <*> (v .: "alphabet")

  parseJSON _ = error "Not an object"

instance FromJSON Bases where
  parseJSON (Object v) = Bases
    <$> (v .: "bases")

  parseJSON _ = error "Not an object"

readBasesJSON :: String -> Either String Bases
readBasesJSON text = do
  (Bases bases) <- eitherDecode . B.pack $ text
  foldl (
    \accum base -> do
      (Bases bases') <- accum
      _              <- validateBase base
      return $ Bases (base : bases')
    ) (Right . Bases $ []) bases
