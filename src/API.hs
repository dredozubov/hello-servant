{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Backend
import Servant

type GetCatMapAPI = "cat" :> Get '[JSON] [(Int, Cat)]

-- Router
-- /cat -- path
-- Get -- HTTP Verb
-- '[JSON] -- list of content-types
-- [(Int, Cat)] -- Content response data type
-- :<|> -- "choice"/"or" operator
type CatAPI = GetCatMapAPI
         :<|> "cat" :> ReqBody '[JSON] Cat :> Put '[JSON] Int
         :<|> "cat" :> ReqBody '[JSON] Int :> Delete '[JSON] ()

api :: Proxy CatAPI
api = Proxy
