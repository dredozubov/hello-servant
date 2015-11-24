{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Backend
  ( Cat(..)
  , Color(..)
  , CatMap(..)
  , addCat
  , getCatMap
  , getCat
  , deleteCat
  , makeAndAddCat
  , DB
  ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Acid
import Data.Aeson
import Data.DeriveTH
import Data.IntMap.Strict hiding (update)
import Data.SafeCopy
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Prelude hiding (lookup)
import Servant
import Test.QuickCheck


data Color = Black | White | Brown | Spotted
  deriving (Show, Eq, Generic)

instance ToJSON Color

instance FromJSON Color

deriveSafeCopy 0 'base ''Color

derive makeArbitrary ''Color

data Cat = Cat
  { name  :: Text
  , color :: Color
  , age   :: Int
  } deriving (Show, Eq, Generic, Typeable)

instance ToJSON Cat

instance FromJSON Cat

deriveSafeCopy 0 'base ''Cat

instance Arbitrary Cat where
  arbitrary = Cat <$> name <*> arbitrary <*> age
    where
      name = elements ["tigger", "Oliver", "Lucy", "Kitty", "Oreo", "Rocky"]
      age  = arbitrary `suchThat` (>0)

newtype CatMap = CatMap { unCatMap :: IntMap Cat }
  deriving (Show, Typeable, Generic)

instance ToJSON CatMap

instance FromJSON CatMap

instance Arbitrary CatMap where
  arbitrary = CatMap <$> catMap
    where
      catMap  = fromList <$> catList
      catList = zip <$> arbitrary <*> arbitrary

deriveSafeCopy 0 'base ''CatMap

getCatMap' :: Query CatMap [(Int, Cat)]
getCatMap' = toList . unCatMap <$> ask

getCat' :: Int -> Query CatMap (Maybe Cat)
getCat' i = lookup i . unCatMap <$> ask

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

makeAcidic ''CatMap ['getCatMap', 'getCat', 'addCat', 'deleteCat']

type DB = AcidState CatMap

getCatMap :: MonadIO m => DB -> m [(Int, Cat)]
getCatMap db = liftIO $ query db GetCatMap'

getCat :: MonadIO m => DB -> Int -> m (Maybe Cat)
getCat db i = liftIO $ query db $ GetCat' i

addCat :: MonadIO m => DB -> Cat -> m Int
addCat db cat = liftIO $ update db $ AddCat' cat

makeAndAddCat :: MonadIO m => DB -> Text -> Color -> Int -> m Int
makeAndAddCat db name color age =
  liftIO $ update db . AddCat' $ Cat name color age

deleteCat :: MonadIO m => DB -> Int -> m ()
deleteCat db i = liftIO $ update db $ DeleteCat' i
