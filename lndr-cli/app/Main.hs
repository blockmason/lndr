{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Default
import qualified Data.Text.Lazy             as LT
import qualified Dhall
import           Lndr.CLI.Args
import           Lndr.CLI.Config
import           Network.Ethereum.Web3
import           System.Console.CmdArgs     hiding (def)
import           System.Directory           (doesFileExist)

main :: IO ()
main = do mode <- cmdArgsRun $ cmdArgsMode programModes
          confPath <- configPath
          -- check for presence of a config file
          configExists <- doesFileExist confPath
          config <- if configExists
            then Dhall.input Dhall.auto (LT.pack confPath)
            else do putStrLn "using default config..."
                    return def

          runMode config mode
