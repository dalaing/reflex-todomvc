{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module TodoMVC where

import Reflex.Dom.Core

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import TodoMVC.Types.TodoItem

import TodoMVC.Header
import TodoMVC.Main
import TodoMVC.Footer

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

