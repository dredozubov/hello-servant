{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Backend
  ( Cat(..)
  , Color(..)
  , CatMap(..)
  , addCat
  , getCatMap
  , deleteCat
  , makeAndAddCat
  , DB
  ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Acid
import Data.Aeson
import Data.IntMap.Strict hiding (update)
import Data.SafeCopy
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Servant

data Color = Black | White | Brown | Spotted
  deriving (Show, Eq, Generic)

instance ToJSON Color

instance FromJSON Color

deriveSafeCopy 0 'base ''Color

data Cat = Cat
  { name  :: Text
  , color :: Color
  , age   :: Int
  } deriving (Show, Eq, Generic, Typeable)

instance ToJSON Cat

instance FromJSON Cat

deriveSafeCopy 0 'base ''Cat

newtype CatMap = CatMap { unCatMap :: IntMap Cat }
  deriving (Show, Typeable, Generic)

instance ToJSON CatMap

instance FromJSON CatMap

deriveSafeCopy 0 'base ''CatMap

getCatMap' :: Query CatMap [(Int, Cat)]
getCatMap' = toList . unCatMap <$> ask

addCat' :: Cat -> Update CatMap Int
addCat' cat = do
  intMap <- unCatMap <$> get
  let
    (newKey, newIntMap) = case maxViewWithKey intMap of
      Nothing            -> (1, (singleton 1 cat))
      Just ((max, _), _) ->
        let newMax = succ max
        in (newMax, insert newMax cat intMap)
  put (CatMap  newIntMap)
  return newKey

deleteCat' :: Int -> Update CatMap ()
deleteCat' i = modify (\(CatMap intMap) -> CatMap $ delete i intMap)

makeAcidic ''CatMap ['getCatMap', 'addCat', 'deleteCat']

type DB = AcidState CatMap

getCatMap :: MonadIO m => DB -> m [(Int, Cat)]
getCatMap db = liftIO $ query db GetCatMap'

addCat :: MonadIO m => DB -> Cat -> m Int
addCat db cat = liftIO $ update db $ AddCat' cat

makeAndAddCat :: MonadIO m => DB -> Text -> Color -> Int -> m Int
makeAndAddCat db name color age =
  liftIO $ update db . AddCat' $ Cat name color age

deleteCat :: MonadIO m => DB -> Int -> m ()
deleteCat db i = liftIO $ update db $ DeleteCat' i
