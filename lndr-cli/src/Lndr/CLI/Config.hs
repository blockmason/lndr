{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lndr.CLI.Config where

import           Data.Default
import           Dhall

data Config = Config { url :: Text
                     , secretKey :: Text
                     } deriving (Generic, Show)
