{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module TodoMVC.Main (
    mainSection
  ) where

import Data.Monoid

import Control.Lens

import Control.Monad.Reader (runReaderT)

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom.Core

import Util.Model
import TodoMVC.Types.TodoItem
import TodoMVC.Types.Filter

import TodoMVC.Item

mainAttrs ::
  Reflex t =>
  Dynamic t Int ->
  Dynamic t (Map Text Text)
mainAttrs dSize =
  let
    attr = "class" =: "main"
    mkAttr 0 = "hidden" =: ""
    mkAttr _ = mempty
    dAttr = mkAttr <$> dSize
  in
    pure attr <> dAttr

mainSection ::
  MonadWidget t m =>
  [TodoItem] ->
  Event t Text ->
  Event t () ->
  Dynamic t Filter ->
  m (Dynamic t Int, Dynamic t Bool)
mainSection initialItems eAdd eClearComplete dFilter = mdo
  (dSize, dAnyComplete) <- elDynAttr "section" (mainAttrs dSize) $ mdo
      eMarkAllComplete <- markAllComplete (all (view tiComplete) initialItems) dAllComplete

      let
        eNewItem = TodoItem False <$> eAdd
      dModel <- setupModel initialItems eNewItem eModelChanges

      let
        dModelFiltered =
          (\f -> Map.filter (filterComplete f . view tiComplete)) <$> dFilter <*> dModel

      (dmdComplete, eModelChanges) <-
        runEventWriterT . elClass "ul" "todo-list" .
        listWithKey dModelFiltered $ \k ->
        flip runReaderT k .
        todoItem eClearComplete eMarkAllComplete

      let
        dComplete    = joinDynThroughMap dmdComplete
        dSize        = Map.size <$> dComplete
        dAnyComplete = or       <$> dComplete
        dAllComplete = and      <$> dComplete

      pure (dSize, dAnyComplete)
  pure (dSize, dAnyComplete)

markAllComplete ::
  MonadWidget t m =>
  Bool ->
  Dynamic t Bool ->
  m (Event t Bool)
markAllComplete initial dAllComplete = do
  cb <- checkbox initial $
    def & checkboxConfig_attributes .~ pure ("id" =: "toggle-all" <> "class" =: "toggle-all")
        & checkboxConfig_setValue .~ updated dAllComplete
  elAttr "label" ("for" =: "toggle-all") $ text "Mark all as complete"
  pure $ cb ^. checkbox_change
