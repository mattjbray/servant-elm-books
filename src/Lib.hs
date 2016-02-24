{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Lib
    ( BooksApi
    , apiServer
    ) where

import           Data.Aeson   (ToJSON)
import GHC.Generics (Generic)
import Servant ((:>), Get, JSON, Server)

data Book = Book
  { name :: String
  } deriving (Show, Eq, Generic)

instance ToJSON Book

type BooksApi = "books" :> Get '[JSON] [Book]

apiServer :: Server BooksApi
apiServer = listBooks
  where
    listBooks =
      return [ Book "Real World Haskell"
             , Book "Learn You a Haskell for Great Good"
             ]
