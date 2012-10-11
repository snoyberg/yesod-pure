{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Create apps without any route data type. This loses some of the features
-- of type-safe URLs, but simplifies app creation.
module Yesod.Pure.NoRoute
    ( module Yesod.Pure
    , module Yesod.Pure.NoRoute
    ) where

import Yesod.Pure
import Data.Text (Text)
import Control.Applicative (Applicative (..), Alternative (..))
import Data.Monoid (Monoid (..))

data App = App (NoRouteDispatch ())

instance YesodDispatch App App where
    yesodDispatch logger master@(App (NoRouteDispatch d _)) sub toMaster on404 _ =
        dispatch (Just . AppRoute) d' logger master sub toMaster on404 (const on404)
      where
        d' m (AppRoute ps) = d m ps

instance RenderRoute App where
    newtype Route App = AppRoute [Text]
        deriving Eq
    renderRoute (AppRoute x) = (x, [])

data NoRouteDispatch a = NoRouteDispatch (Text -> [Text] -> Maybe (GHandler App App ChooseRep)) (Maybe a)

instance Functor NoRouteDispatch where
    fmap f (NoRouteDispatch x ma) = NoRouteDispatch x (fmap f ma)
instance Applicative NoRouteDispatch where
    pure = NoRouteDispatch (\_ _ -> Nothing) . Just
    NoRouteDispatch a f <*> NoRouteDispatch b x =
        NoRouteDispatch (\m p -> a m p <|> b m p) (f <*> x)
instance Alternative NoRouteDispatch where
    empty = NoRouteDispatch (\_ _ -> Nothing) Nothing
    (<|>) = (*>)
instance Monoid (NoRouteDispatch a) where
    mempty = empty
    mappend = (<|>)

-- | I'm not convinced this instance is correct, for now consider it a dummy
-- placeholder for playing around with do-syntax.
instance Monad NoRouteDispatch where
    return = pure
    NoRouteDispatch f Nothing >>= _ = NoRouteDispatch f Nothing
    NoRouteDispatch a (Just x) >>= f = NoRouteDispatch a (Just ()) *> f x

serve :: HasReps a => GHandler App App a -> NoRouteDispatch ()
serve h =
    NoRouteDispatch go (Just ())
  where
    go _ [] = Just $ fmap chooseRep h
    go _ _ = Nothing

method :: Text -> NoRouteDispatch a -> NoRouteDispatch a
method x (NoRouteDispatch f a) =
    NoRouteDispatch go a
  where
    go m ps
        | m == x = f m ps
        | otherwise = Nothing

static :: Text -> NoRouteDispatch a -> NoRouteDispatch a
static x (NoRouteDispatch f a) =
    NoRouteDispatch go a
  where
    go _ [] = Nothing
    go m (t:ts)
        | t == x = f m ts
        | otherwise = Nothing

dynamic :: PathPiece p => (p -> NoRouteDispatch b) -> NoRouteDispatch ()
dynamic f =
    NoRouteDispatch go (Just ())
  where
    go _ [] = Nothing
    go m (t:ts) =
        case fromPathPiece t of
            Nothing -> Nothing
            Just p ->
                let (NoRouteDispatch f' _) = f p
                 in f' m ts

multi :: PathMultiPiece ps => (ps -> NoRouteDispatch b) -> NoRouteDispatch ()
multi f =
    NoRouteDispatch go (Just ())
  where
    go m ts =
        case fromPathMultiPiece ts of
            Nothing -> Nothing
            Just ps ->
                let (NoRouteDispatch f' _) = f ps
                 in f' m []
