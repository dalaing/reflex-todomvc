{-# LANGUAGE OverloadedStrings #-}
module TodoMVC.Types.Filter (
    Filter(..)
  , filterComplete
  , filterLink
  , filterName
  ) where

import Data.Text (Text)

data Filter =
    All
  | Active
  | Completed
  deriving (Eq, Ord, Show)

filterComplete ::
  Filter ->
  Bool ->
  Bool
filterComplete All =
  const True
filterComplete Active =
  not
filterComplete Completed =
  id

filterLink ::
  Filter ->
  Text
filterLink All =
  "#/"
filterLink Active =
  "#/active"
filterLink Completed =
  "#/completed"

filterName ::
  Filter ->
  Text
filterName All =
  "All"
filterName Active =
  "Active"
filterName Completed =
  "Completed"
