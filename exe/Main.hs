{-# LANGUAGE OverloadedStrings #-}
module Main (
    main
  ) where

import Data.Monoid ((<>))

import Reflex.Dom.Core

import TodoMVC (todomvc)

headSection ::
  MonadWidget t m =>
  m ()
headSection = do
  elAttr "meta" ("charset" =: "utf-8") $
    pure ()
  el "title" $
    text "TodoMVC"
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "/css/index.css") $
    pure ()

main ::
  IO ()
main =
  mainWidgetWithHead headSection todomvc
