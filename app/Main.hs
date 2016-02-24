{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Lucid
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant                  ((:<|>) (..), (:>), Config (..), Get,
                                           Proxy (..), Server, serve)
import           Servant.HTML.Lucid       (HTML)

import           Lib                      (BooksApi, apiServer)

type SiteApi = "api" :> BooksApi
          :<|> Get '[HTML] (Html ())

server :: Server SiteApi
server =
  apiServer :<|> home
  where
    home =
      return homePage

homePage :: Html ()
homePage =
  doctypehtml_ $ do
    head_ $
      title_ "Servant-Elm Books"
    body_ $ do
      h1_ "Hi there"
      p_ "This is a website."

app :: Application
app = serve (Proxy :: Proxy SiteApi) EmptyConfig server

main :: IO ()
main = do
  let port = 8000
  putStrLn $ "Serving on port " ++ show port ++ "..."
  run port app
