{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module TodoMVC.Footer (
    footer
  ) where

import Data.Monoid hiding (All)

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)

import Reflex.Dom.Core

import Util.Reflex
import TodoMVC.Types.Filter

footerAttrs ::
  Reflex t =>
  Dynamic t Int ->
  Dynamic t (Map Text Text)
footerAttrs dCount =
  let
    attr = "class" =: "footer"
    mkAttr 0 = "hidden" =: ""
    mkAttr _ = mempty
    dAttr = mkAttr <$> dCount
  in
    pure attr <> dAttr

footer ::
  MonadWidget t m =>
  Dynamic t Int ->
  Dynamic t Bool ->
  m (Dynamic t Filter, Event t ())
footer dSize dAnyComplete =
  elDynAttr "footer" (footerAttrs dSize) $ do
    counter dSize
    dFilter <- filters
    eClearComplete <- clearComplete dAnyComplete
    pure (dFilter, eClearComplete)

counter ::
  MonadWidget t m =>
  Dynamic t Int ->
  m ()
counter dCount =
  let
    countNumber = Text.pack . show
    countWords 1 = " item left"
    countWords _ = " items left"
  in
  elClass "span" "todo-count" $ do
    el "strong" $ dynText $ countNumber <$> dCount
    dynText $ countWords <$> dCount

filterAttrs ::
  Reflex t =>
  Dynamic t Filter ->
  Filter ->
  Dynamic t (Map Text Text)
filterAttrs dFilter f =
  let
    attrs = "href" =: filterLink f
    mkAttrs False = mempty
    mkAttrs True = "class" =: "selected"
    dAttrs = (mkAttrs . (== f)) <$> dFilter
  in
    pure attrs <> dAttrs

mkFilter ::
  MonadWidget t m =>
  Dynamic t Filter ->
  Filter ->
  m (Event t Filter)
mkFilter dFilter f = el "li" $ do
  (e, _) <- elDynAttr' "a" (filterAttrs dFilter f) . text . filterName $ f
  let
    eClick = domEvent Click e
  pure $ f <$ eClick

filters ::
  MonadWidget t m =>
  m (Dynamic t Filter)
filters = elClass "ul" "filters" $ mdo
  eAll <- mkFilter dFilter All
  eActive <- mkFilter dFilter Active
  eCompleted <- mkFilter dFilter Completed
  dFilter <- holdDyn All . leftmost $ [eAll, eActive, eCompleted]
  pure dFilter

clearCompleteAttrs ::
  Reflex t =>
  Dynamic t Bool ->
  Dynamic t (Map Text Text)
clearCompleteAttrs dAnyComplete =
  let
    attrs = "class" =: "clear-completed"
    mkAttrs False = "hidden" =: ""
    mkAttrs True  = mempty
    dAttrs = mkAttrs <$> dAnyComplete
  in
    pure attrs <> dAttrs

clearComplete ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t ())
clearComplete dAnyComplete =
  buttonDynAttr (clearCompleteAttrs dAnyComplete) "Clear completed"
