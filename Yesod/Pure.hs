{-# LANGUAGE NoMonomorphismRestriction #-}
module Yesod.Pure
    ( module Yesod
    , module Yesod.Pure
    ) where

import Yesod
import Data.Text (Text)
import Data.Text.Lazy.Builder (fromText)
import System.Log.FastLogger (Logger)

type RouteParse master = [Text] -> Maybe (Route master)


handler = Just . fmap toContent

dispatch parse dispatch' logger master sub toMaster on404 on405 method pieces session =
    case parse pieces of
        Just route ->
            case dispatch' method route of
                Just h -> yesodRunner logger h (Just route) session
                Nothing -> on405 route session
        Nothing -> on404 session

addCSS = toWidget . const . CssBuilder . fromText
