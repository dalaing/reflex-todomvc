{-# LANGUAGE TemplateHaskell #-}
module TodoMVC.Types.TodoItem (
    TodoItem(..)
  , tiComplete
  , tiName
  ) where

import Control.Lens
import Data.Text (Text)

data TodoItem =
  TodoItem {
    _tiComplete :: Bool
  , _tiName :: Text
  }

makeLenses ''TodoItem
