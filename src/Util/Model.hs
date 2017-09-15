{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Util.Model (
    AsChanges
  , tellChange
  , AsRemovals
  , tellRemoval
  , ModelChanges
  , updateModel
  , setupModel
  ) where

import Data.Semigroup

import Control.Monad.Fix (MonadFix)

import Control.Lens
import Control.Monad.Reader (MonadReader(..))

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Reflex

newtype Changes k i =
  Changes (Map k (i -> i))

makeWrapped ''Changes

instance Ord k => Semigroup (Changes k i) where
  (<>) (Changes m1) (Changes m2) =
    Changes (Map.unionWith (.) m1 m2)

instance Ord k => Monoid (Changes k i) where
  mempty =
    Changes mempty
  mappend =
    (<>)

class AsChanges f k i | f -> k, f -> i where
  changes :: Lens' f (Changes k i)

instance AsChanges (Changes k i) k i where
  changes = id

tellChange ::
  ( Reflex t
  , Monoid w
  , EventWriter t w m
  , AsChanges w k i
  , MonadReader k m
  ) =>
  Event t (i -> i) ->
  m ()
tellChange e = do
  k <- ask
  tellEvent $ (\f -> mempty & changes . _Wrapped .~ (Map.singleton k f)) <$> e

newtype Removals k =
  Removals (Set k)

instance Ord k => Semigroup (Removals k) where
  (<>) (Removals s1) (Removals s2) =
    Removals (Set.union s1 s2)

instance Ord k => Monoid (Removals k) where
  mempty =
    Removals mempty
  mappend =
    (<>)

makeWrapped ''Removals

class AsRemovals f k | f -> k where
  removals :: Lens' f (Removals k)

instance AsRemovals (Removals k) k where
  removals = id

tellRemoval ::
  ( Reflex t
  , Monoid w
  , EventWriter t w m
  , AsRemovals w k
  , MonadReader k m
  ) =>
  Event t a ->
  m ()
tellRemoval e = do
  k <- ask
  tellEvent $ (mempty & removals . _Wrapped .~ Set.singleton k) <$ e

data ModelChanges k i =
  ModelChanges {
    _mcChanges :: Changes k i
  , _mcRemovals :: Removals k
  }

instance Ord k => Semigroup (ModelChanges k i) where
  (<>) (ModelChanges c1 r1) (ModelChanges c2 r2) =
    ModelChanges (mappend c1 c2) (mappend r1 r2)

instance Ord k => Monoid (ModelChanges k i) where
  mempty =
    ModelChanges mempty mempty
  mappend =
    (<>)

makeLenses ''ModelChanges

instance AsChanges (ModelChanges k i) k i where
  changes = mcChanges

instance AsRemovals (ModelChanges k i) k where
  removals = mcRemovals

updateModel ::
  ( Reflex t
  , Ord k
  ) =>
  Event t (ModelChanges k i) ->
  Event t (Map k i -> Map k i)
updateModel e =
  let
    eChanges = view (mcChanges . _Wrapped) <$> e
    eChangeMap = Map.mergeWithKey (\_ f x -> Just (f x)) (const mempty) id <$> eChanges
    eRemovals = view (mcRemovals . _Wrapped) <$> e
    eRemoveMap = flip (foldr Map.delete) <$> eRemovals
  in
    mergeWith (.) [eRemoveMap, eChangeMap]

setupModel ::
  ( Reflex t
  , Ord k
  , Num k
  , Enum k
  , MonadFix m
  , MonadHold t m
  ) =>
  [i] ->
  Event t i ->
  Event t (ModelChanges k i) ->
  m (Dynamic t (Map k i))
setupModel initialItems eAdd eChange = do
  dCount <- count eAdd
  let
    initialCount = fromIntegral $ length initialItems
    initialMap = Map.fromList $ zip [fromInteger 0 ..] initialItems
    dKey = fmap (+ initialCount) dCount

  foldDyn ($) initialMap . mergeWith (.) $ [
              Map.insert <$> current dKey <@> eAdd
            , updateModel eChange
            ]

