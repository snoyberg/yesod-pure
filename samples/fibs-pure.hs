{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Yesod.Pure
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Control.Applicative ((<$>))
import Text.Blaze.Html (toHtml, (!), toValue)

data App = App

instance RenderRoute App where
    data Route App = HomeR
                   | FibR Int
        deriving Eq
    renderRoute HomeR = ([], [])
    renderRoute (FibR i) = (["fib", toPathPiece i], [])

parseRoute1 [] = Just HomeR
parseRoute1 ["fib", i] = FibR <$> fromPathPiece i
parseRoute1 _ = Nothing

dispatchRoute "GET" HomeR = handler getHomeR
dispatchRoute "GET" (FibR i) = handler $ getFibR i
dispatchRoute _ _ = Nothing

instance Yesod App
yesodRunner
instance YesodDispatch App where
    yesodDispatch = yesodRunner ?? dispatch parseRoute1 dispatchRoute
{-

-}
-- getHomeR :: (HandlerSite m, MonadWidget m) => m ()
getHomeR = do
    setTitle "Hello World!"
    toWidget $ \render -> do
        H.p "Hello World"
        H.a ! HA.href (toValue $ render (FibR 5) []) $ "Fifth fib"
    addCSS "p { color: red }"

getFibR i = do
    setTitle "Fibs"
    toWidget $ \render -> do
        H.p $ do
            "Fib for "
            toHtml i
            ": "
            toHtml $ fibs !! i
        H.a ! HA.href (toValue $ render (FibR $ i + 1) []) $ "Next fib"

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
main :: IO ()
main = warpDebug 3000 App
{-
-}