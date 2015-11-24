module Main where

import API
import Backend (Cat(..), getCatMap, getCat, addCat, deleteCat, makeAndAddCat, DB, CatMap(..))
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
import Servant.Mock


app :: Application
app = serve api (mock api)

main :: IO ()
main = run 8081 app
