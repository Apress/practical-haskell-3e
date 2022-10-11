{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module MyLib (someFunc) where

import Data.Aeson
import Data.Text

import Data.Aeson.KeyMap as M

import Lens.Micro ((^?))
import Lens.Micro.Aeson

import Data.Aeson.Types
import Control.Applicative

import Data.Conduit
import qualified Data.Conduit.Binary as B
import qualified Data.Conduit.List as L
import qualified Data.ByteString.Lazy as LB

import GHC.Generics

data Client i = GovOrg i String
              | Company i String Person String
              | Individual i Person
              deriving (Show, Generic, ToJSON, FromJSON)

data Person = Person String String
              deriving (Show, Generic, ToJSON, FromJSON)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

clientToJSON :: Client Integer -> Value
clientToJSON (GovOrg i n) =
  object [ "type"  .= String "govorg"
         , "id"    .= Number (fromInteger i)
         , "name"  .= String (pack n) ]
clientToJSON (Company i n p d) =
  object [ "type"   .= String "company"
         , "id"     .= Number (fromInteger i)
         , "name"   .= String (pack n)
         , "person" .= personToJSON p
         , "duty"   .= String (pack d) ]
clientToJSON (Individual i p) =
  object [ "type"   .= String "individual"
         , "id"     .= Number (fromInteger i)
         , "person" .= personToJSON p ]

personToJSON :: Person -> Value
personToJSON (Person f l) = object [ "first" .= String (pack f)
                                   , "last"  .= String (pack l) ]

jsonToPerson :: Value -> Maybe Person
jsonToPerson (Object o) = do String f <- M.lookup "first" o
                             String l <- M.lookup "last"  o
                             return $ Person (unpack f) (unpack l)
jsonToPerson _          = Nothing

jsonToPerson2 :: Value -> Maybe Person
jsonToPerson2 j = do String f <- j ^? key "first"
                     String l <- j ^? key "last"
                     return $ Person (unpack f) (unpack l)

jsonToPerson3 :: Value -> Parser Person
jsonToPerson3 (Object o) = Person <$> o .: "first" <*> o .: "last"
jsonToPerson3 _          = Control.Applicative.empty

saveClients :: FilePath -> [Client Integer] -> IO ()
saveClients fPath clients = runConduitRes $
  yield (toJSON clients) .| L.map (LB.toStrict . encode)
                         .| B.sinkFile fPath