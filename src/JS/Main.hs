module Main where

import           API
import qualified Language.Javascript.JQuery as JQ
import           Servant
import           Servant.JQuery

apiJS :: String
apiJS = jsForAPI api

main :: IO ()
main = do
  writeFile "js/api.js" apiJS
  jqFile <- readFile =<< JQ.file
  writeFile "js/jq.js" jqFile
