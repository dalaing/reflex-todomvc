{-# LANGUAGE OverloadedStrings #-}
module Util.Reflex (
    buttonDynAttr
  ) where

import Data.Text (Text)
import Data.Map (Map)

import Reflex.Dom.Core

buttonDynAttr ::
  MonadWidget t m =>
  Dynamic t (Map Text Text) ->
  Text ->
  m (Event t ())
buttonDynAttr dAttrs label = do
  (e, _) <- elDynAttr' "button" dAttrs $ text label
  pure $ domEvent Click e
