{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module TodoMVC.Header (
    header
  ) where

import Data.Monoid

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)

import Reflex.Dom.Core

header ::
  MonadWidget t m =>
  m (Event t Text)
header =
  elClass "header" "header" $ do
    el "h1" $ text "todos"
    addItem

addItemAttrs ::
  Map Text Text
addItemAttrs =
  "class" =: "new-todo" <>
  "placeholder" =: "What needs to be done?" <>
  "autofocus" =: ""

addItem ::
  MonadWidget t m =>
  m (Event t Text)
addItem = mdo
  ti <- textInput $
    def & textInputConfig_attributes .~ pure addItemAttrs
        & textInputConfig_setValue .~ ("" <$ eEnter)

  let
    bValue = current . fmap (Text.strip) $ ti ^. textInput_value
    isKey k = (== k) . keyCodeLookup . fromIntegral
    eKey = ti ^. textInput_keypress
    eEnter = ffilter (isKey Enter) eKey
    eAtEnter = bValue <@ eEnter
    eDone = ffilter (not . Text.null) eAtEnter

  pure eDone
