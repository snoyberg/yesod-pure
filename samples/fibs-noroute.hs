{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Yesod.Pure.NoRoute
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Control.Applicative ((<$>), (<|>))
import Text.Blaze.Html (toHtml, (!), toValue)
import Data.Monoid (mconcat)

instance Yesod App
type Handler = GHandler App App

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
    setTitle "Hello World!"
    toWidget $ \render -> do
        H.p "Hello World"
        H.a ! HA.href "/fib/5" $ "Fifth fib"
    addCSS "p { color: red }"

getFibR :: Int -> Handler RepHtml
getFibR i = defaultLayout $ do
    setTitle "Fibs"
    toWidget $ \render -> do
        H.p $ do
            "Fib for "
            toHtml i
            ": "
            toHtml $ fibs !! i
        H.a ! HA.href (toValue $ render (AppRoute ["fib", toPathPiece $ i + 1]) []) $ "Next fib"

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = warpDebug 3000 $ App $
{-
    method "GET" (serve getHomeR)
    <|> static "fib" (dynamic $ \i -> method "GET" $ serve $ getFibR i)
    -}
    {-
    mconcat
        [ method "GET" $ serve getHomeR
        , static "fib" $ dynamic $ \i -> method "GET" $ serve $ getFibR i
        ]
        -}
    do
        method "GET" $ serve getHomeR
        static "fib" $ dynamic $ \i -> method "GET" $ serve $ getFibR i
