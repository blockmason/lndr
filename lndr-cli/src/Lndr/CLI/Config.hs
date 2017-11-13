{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lndr.CLI.Config (
      Config(..)
    , configPath
    ) where

import           Data.Default
import           Dhall
import           Network.Ethereum.Web3
import           System.Directory (getCurrentDirectory)

data Config = Config { url :: Text
                     , address :: Text
                     , secretKey :: Text
                     , friends :: [Text]
                     } deriving (Generic, Show)

instance Interpret Config

instance Default Config where
    def = Config "http://localhost:80"
                 "6a362e5cee1cf5a5408ff1e12b0bc546618dffcb"
                 "024f55d169862624eec05be973a38f52ad252b3bcc0f0ed1927defa4ab4ea101"
                 []

configPath :: IO FilePath
configPath = (++) <$> getCurrentDirectory <*> pure "/lndr.conf"
