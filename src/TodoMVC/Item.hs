{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module TodoMVC.Item (
    todoItem
  ) where

import Control.Monad (void)
import Data.Monoid ((<>))

import Control.Lens
import Control.Monad.Reader (MonadReader)

import Data.Text (Text)
import qualified Data.Text as Text


import Reflex.Dom.Core

import Util.Reflex
import Util.Model
import TodoMVC.Types.TodoItem

todoItemAttrs ::
  Reflex t =>
  Dynamic t Bool ->
  Dynamic t Bool ->
  Dynamic t Text
todoItemAttrs dComplete dEditing =
  let
    mkComplete False = ""
    mkComplete True = "completed "
    clsComplete = mkComplete <$> dComplete
    mkEditing False = ""
    mkEditing True = "editing "
    clsEditing = mkEditing <$> dEditing
  in
    clsComplete <> clsEditing

todoItem ::
  ( MonadWidget t m
  , Ord k
  , Monoid w
  , EventWriter t w m
  , AsRemovals w k
  , MonadReader k m
  ) =>
  Event t () ->
  Event t Bool ->
  Dynamic t TodoItem ->
  m (Dynamic t Bool)
todoItem eClearComplete eMarkAllComplete dItem = mdo
  (dComplete, dEditing) <- elDynClass "li" (todoItemAttrs dComplete dEditing) $ do
    (dComplete, eEdit) <- todoItemView eClearComplete eMarkAllComplete dItem
    eView <- todoItemEdit (view tiName <$> dItem)
    dEditing <- holdDyn False . leftmost $ [True <$ eEdit, False <$ eView]
    pure (dComplete, dEditing)
  pure dComplete

todoItemView ::
  ( MonadWidget t m
  , Ord k
  , Monoid w
  , EventWriter t w m
  , AsRemovals w k
  , MonadReader k m
  ) =>
  Event t () ->
  Event t Bool ->
  Dynamic t TodoItem ->
  m (Dynamic t Bool, Event t ())
todoItemView eClearComplete eMarkAllComplete dItem =
  divClass "view" $ do
    dComplete <- complete eClearComplete eMarkAllComplete (view tiComplete <$> dItem)
    eEdit <- itemName (view tiName <$> dItem)
    remove
    pure (dComplete, eEdit)

todoItemEdit ::
  ( MonadWidget t m
  , Ord k
  , Monoid w
  , EventWriter t w m
  , AsRemovals w k
  , MonadReader k m
  ) =>
  Dynamic t Text ->
  m (Event t ())
todoItemEdit dName = mdo
  ti <- textInput $
    def & textInputConfig_attributes .~ pure ("class" =: "edit")
        & textInputConfig_setValue .~ ("" <$ eEnter)

  let
    bValue = current . fmap (Text.strip) $ ti ^. textInput_value
    isKey k = (== k) . keyCodeLookup . fromIntegral
    eKey = ti ^. textInput_keypress
    eEnter = ffilter (isKey Enter) eKey
    eAtEnter = bValue <@ eEnter
    eDone = ffilter (not . Text.null) eAtEnter

  pure $ void eDone

complete ::
  ( MonadWidget t m
  , Ord k
  , Monoid w
  , EventWriter t w m
  , AsRemovals w k
  , MonadReader k m
  ) =>
  Event t () ->
  Event t Bool ->
  Dynamic t Bool ->
  m (Dynamic t Bool)
complete eClearComplete eMarkAllComplete dCompleteIn = do
  initial <- sample . current $ dCompleteIn
  cb <- checkbox initial $
    def & checkboxConfig_attributes .~ pure ("class" =: "toggle")
        & checkboxConfig_setValue .~ eMarkAllComplete

  let
    dComplete = cb ^. checkbox_value
  tellRemoval $ gate (current dComplete) eClearComplete

  pure dComplete

itemName ::
  MonadWidget t m =>
  Dynamic t Text ->
  m (Event t ())
itemName dText = do
  (e, _) <- el' "label" . dynText $ dText
  pure . void $ domEvent Dblclick e

remove ::
  ( MonadWidget t m
  , Ord k
  , Monoid w
  , EventWriter t w m
  , AsRemovals w k
  , MonadReader k m
  ) =>
  m ()
remove = do
  eRemove <- buttonDynAttr (pure $ "class" =: "destroy") ""
  tellRemoval eRemove
