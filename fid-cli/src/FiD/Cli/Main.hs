{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module FiD.Cli.Main
    ( main
    ) where

import System.Console.CmdArgs

data FiDCmd = Info
            | Init
            | Request {debtor :: String}
            | Send {creditor :: String}
            deriving (Show, Data, Typeable)

main :: IO ()
main = print =<< cmdArgs (modes [Info, Init, Request "", Send ""])

-- print all cp logs
-- print all cp logs related to FiD

--
