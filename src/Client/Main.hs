{-# LANGUAGE OverloadedStrings #-}

module Main where

import API
import Backend (Cat(..), Color(..), CatMap(..))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.IntMap.Strict
import Servant
import Servant.Client


getCatMap :<|> getCat :<|> addCat :<|> deleteCat =
  client api (BaseUrl Http "localhost" 8080)

main :: IO ()
main = void . runEitherT $ do
  getAndPrint
  id <- addCat (Cat "Grumpy" White 3)
  getAndPrint
  deleteCat id
  getAndPrint
  liftIO $ print "Done!"
  where
    getAndPrint = getCatMap >>= liftIO . print
