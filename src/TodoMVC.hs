{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module TodoMVC where

import Control.Monad (void)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Monoid ((<>))

import Reflex.Dom.Core

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

data TodoItem =
  TodoItem {
    tiComplete :: Bool
  , tiName :: Text
  }

todomvc' ::
  MonadWidget t m =>
  [TodoItem] ->
  m ()
todomvc' initialItems =
  elClass "section" "todoapp" $ mdo
    eAdd <- header
    (dCount, dAnyComplete) <- mainSection initialItems eAdd eClearComplete dFilter
    (dFilter, eClearComplete) <- footer dCount dAnyComplete
    pure ()

todomvc ::
  MonadWidget t m =>
  m ()
todomvc =
  todomvc' [TodoItem False "A", TodoItem True "B", TodoItem False "C"]

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
      eMarkAllComplete <- markAllComplete (all tiComplete initialItems) dAllComplete

      dCount <- count eAdd
      let
        initialCount = length initialItems
        initialMap = Map.fromList $ zip [0..] initialItems
        dKey = fmap (+ initialCount) dCount
        eNewItem = TodoItem False <$> eAdd

      dModel <- foldDyn ($) initialMap . mergeWith (.) $ [
                  Map.insert <$> current dKey <@> eNewItem
                , flip (foldr Map.delete) <$> eRemoves
                ]

      let
        dModelFiltered =
          (\f -> Map.filter (filterComplete f . tiComplete)) <$> dFilter <*> dModel

      (dmdComplete, eRemoves) <-
        runEventWriterT . elClass "ul" "todo-list" .
        listWithKey dModelFiltered $ \k v ->
        todoItem eClearComplete eMarkAllComplete k v

      let
        dComplete    = joinDynThroughMap dmdComplete
        dSize        = Map.size <$> dComplete
        dAnyComplete = or       <$> dComplete
        dAllComplete = and      <$> dComplete

      pure (dSize, dAnyComplete)
  pure (dSize, dAnyComplete)

todoItemAttrs ::
  Reflex t =>
  Dynamic t Bool ->
  Dynamic t Text
todoItemAttrs dComplete =
  let
    mkClass False = ""
    mkClass True = "completed"
    dClass = mkClass <$> dComplete
  in
    dClass

todoItem ::
  ( MonadWidget t m
  , Ord k
  , EventWriter t (Set k) m
  ) =>
  Event t () ->
  Event t Bool ->
  k ->
  Dynamic t TodoItem ->
  m (Dynamic t Bool)
todoItem eClearComplete eMarkAllComplete k dItem = mdo
  dComplete <- elDynClass "li" (todoItemAttrs dComplete) $ do
    (dComplete, eEdit) <- todoItemView eClearComplete eMarkAllComplete k dItem
    pure dComplete
  pure dComplete

todoItemView ::
  ( MonadWidget t m
  , Ord k
  , EventWriter t (Set k) m
  ) =>
  Event t () ->
  Event t Bool ->
  k ->
  Dynamic t TodoItem ->
  m (Dynamic t Bool, Event t ())
todoItemView eClearComplete eMarkAllComplete k dItem =
  divClass "view" $ do
    dComplete <- complete eClearComplete eMarkAllComplete k (tiComplete <$> dItem)
    eEdit <- itemName (tiName <$> dItem)
    remove k
    pure (dComplete, eEdit)

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

complete ::
  ( MonadWidget t m
  , Ord k
  , EventWriter t (Set k) m
  ) =>
  Event t () ->
  Event t Bool ->
  k ->
  Dynamic t Bool ->
  m (Dynamic t Bool)
complete eClearComplete eMarkAllComplete k dCompleteIn = do
  initial <- sample . current $ dCompleteIn
  cb <- checkbox initial $
    def & checkboxConfig_attributes .~ pure ("class" =: "toggle")
        & checkboxConfig_setValue .~ eMarkAllComplete

  let
    dComplete = cb ^. checkbox_value
  tellEvent $ Set.singleton k <$ gate (current dComplete) eClearComplete

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
  , EventWriter t (Set k) m
  ) =>
  k ->
  m ()
remove k = do
  eRemove <- buttonDynAttr (pure $ "class" =: "destroy") $ ""
  tellEvent $ Set.singleton k <$ eRemove

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

-- TODO move to a helpers module
buttonDynAttr ::
  MonadWidget t m =>
  Dynamic t (Map Text Text) ->
  Text ->
  m (Event t ())
buttonDynAttr dAttrs label = do
  (e, _) <- elDynAttr' "button" dAttrs $ text label
  pure $ domEvent Click e

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
