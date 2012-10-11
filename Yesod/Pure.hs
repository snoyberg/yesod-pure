module Yesod.Pure
    ( module Yesod
    , module Yesod.Pure
    ) where

import Yesod
import Data.Text (Text)
import Data.Text.Lazy.Builder (fromText)

type RouteParse master = [Text] -> Maybe (Route master)

type RouteDispatch master = Text
                         -> Route master
                         -> Maybe (GHandler master master ChooseRep)

handler :: HasReps a
        => GHandler sub master a
        -> Maybe (GHandler sub master ChooseRep)
handler = Just . fmap chooseRep

dispatch parse dispatch logger master sub toMaster on404 on405 method pieces session =
    case parse pieces of
        Just route ->
            case dispatch method route of
                Just h -> yesodRunner logger h master sub (Just route) toMaster session
                Nothing -> on405 route session
        Nothing -> on404 session

addCSS :: Text -> GWidget sub master ()
addCSS = toWidget . const . CssBuilder . fromText
