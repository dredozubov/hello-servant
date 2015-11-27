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
