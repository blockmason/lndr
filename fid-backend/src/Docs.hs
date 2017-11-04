{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Docs where

import Data.ByteString.Lazy (ByteString)
import Data.Proxy
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (pack)
import Network.HTTP.Types
import Network.Wai
import Servant.API
import Servant.Docs
import Servant.Server

import Server

-- instance ToCapture (Capture "x" Int) where
--   toCapture _ =
--     DocCapture "x"                                -- name
--                "(integer) position on the x axis" -- description
--
-- instance ToCapture (Capture "y" Int) where
--   toCapture _ =
--     DocCapture "y"                                -- name
--                "(integer) position on the y axis" -- description
--
-- instance ToSample Position Position where
--   toSample _ = Just (Position 3 14) -- example of output
--
-- instance ToParam (QueryParam "name" String) where
--   toParam _ =
--     DocQueryParam "name"                     -- name
--                   ["Alp", "John Doe", "..."] -- example of values (not necessarily exhaustive)
--                   "Name of the person to say hello to." -- description
--                   Normal -- Normal, List or Flag
--
-- instance ToSample HelloMessage HelloMessage where
--   toSamples _ =
--     [ ("When a value is provided for 'name'", HelloMessage "Hello, Alp")
--     , ("When 'name' is not specified", HelloMessage "Hello, anonymous coward")
--     ]
--     -- mutliple examples to display this time
--
-- ci :: ClientInfo
-- ci = ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"]
--
-- instance ToSample ClientInfo ClientInfo where
--   toSample _ = Just ci
--
-- instance ToSample Email Email where
--   toSample _ = Just (emailForClient ci)

-- apiDocs :: API
-- apiDocs = docs fiddyAPI
