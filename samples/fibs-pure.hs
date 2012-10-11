{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

parseRoute :: RouteParse App
parseRoute [] = Just HomeR
parseRoute ["fib", i] = FibR <$> fromPathPiece i
parseRoute _ = Nothing

dispatchRoute :: RouteDispatch App
dispatchRoute "GET" HomeR = handler getHomeR
dispatchRoute "GET" (FibR i) = handler $ getFibR i
dispatchRoute _ _ = Nothing

instance YesodDispatch App App where
    yesodDispatch = dispatch parseRoute dispatchRoute

instance Yesod App
type Handler = GHandler App App

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
    setTitle "Hello World!"
    toWidget $ \render -> do
        H.p "Hello World"
        H.a ! HA.href (toValue $ render (FibR 5) []) $ "Fifth fib"
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
        H.a ! HA.href (toValue $ render (FibR $ i + 1) []) $ "Next fib"

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = warpDebug 3000 App
