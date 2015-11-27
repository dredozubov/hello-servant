module Main where

import API
import Backend
  (Cat(..), getCatMap, getCat, addCat, deleteCat, makeAndAddCat, DB, CatMap(..))
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Acid
import Data.Aeson
import Data.IntMap.Strict (empty)
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
