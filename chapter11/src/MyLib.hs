
{-# LANGUAGE TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, TypeFamilies, GADTs, DerivingStrategies, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleInstances, UndecidableInstances, DataKinds #-}
{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module MyLib (someFunc) where

import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Sqlite
import Control.Monad.Logger
import Control.Monad.IO.Class
import Data.Maybe

import Database.Esqueleto.Experimental ((^.), (?.))
import qualified Database.Esqueleto.Experimental as E

data Gender = Male | Female
  deriving (Show, Read, Eq)

derivePersistField "Gender"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Country
  name String
  canWeSend Bool default=True
  UniqueCountryName name
  deriving Show
Client
  firstName String
  lastName  String
  address   String
  country   CountryId
  age       Int Maybe
  gender    Gender Maybe
  UniqueClient firstName lastName address country
  deriving Show
Product
  price Double
  inStock Int
  deriving Show
Purchase
  client  ClientId
  product ProductId
  number  Int
  amount  Double
  deriving Show
|]

exampleConn = runNoLoggingT $
  withSqliteConn @(NoLoggingT IO) "example.db" $ \conn ->
    liftIO $ flip runSqlPersistM conn $ do
      spain <- insert $ Country "Spain" True
      _client <- insert $ Client "Alejandro" "Serrano" "Home Town, 1" spain (Just 30) (Just Male)
      return ()

exampleConn2 = runSqlite @IO "example.db" $ do
  spain <- insert $ Country "Spain" True
  _client <- insert $ Client "Alejandro" "Serrano" "Home Town, 1" spain (Just 30) (Just Male)
  return ()

examplePool = runNoLoggingT $
  withSqlitePool @(NoLoggingT IO) "example.db" 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      spain <- insert $ Country "Spain" True
      _client <- insert $ Client "Alejandro" "Serrano" "Home Town, 1" spain (Just 30) (Just Male)
      return ()

getPurchaseClient p = get (purchaseClient p)

getClientById n = get $ ClientKey n

getClientByInfo :: MonadIO m
  => String -> String -> String -> String
  -> SqlPersistT m (Maybe Client)
getClientByInfo fName lName addr cnName = do
  cn <- getBy $ UniqueCountryName cnName
  case cn of
    Just (Entity cId _) ->
      do cl <- getBy $ UniqueClient fName lName addr cId
         case cl of
           Just (Entity _ client) -> return $ Just client
           Nothing -> return Nothing
    Nothing -> return Nothing

getAdultsOfSpainAndGermany :: MonadIO m => SqlPersistT m [Entity Client]
getAdultsOfSpainAndGermany = do
  es <- getBy $ UniqueCountryName "Spain"
  de <- getBy $ UniqueCountryName "Germany"
  let countries = map entityKey (catMaybes [es, de])
  selectList [ ClientCountry <-. countries, ClientAge >=. Just 18] [ LimitTo 10 ]

getPeopleOver25 :: MonadIO m => SqlPersistT m [Entity Client]
getPeopleOver25 =
  E.select $ do
  client <- E.from $ E.table @Client
  E.where_ $ client ^. ClientAge E.>. E.just (E.val 25)
  E.orderBy [ E.asc (client ^. ClientLastName), E.asc (client ^. ClientFirstName) ]
  return client

getPeopleOver25FromSpainOrGermany :: MonadIO m => SqlPersistT m [Entity Client]
getPeopleOver25FromSpainOrGermany =
  E.select $ do
  client <- E.from $ E.table @Client
  country <- E.from $ E.table @Country
  E.where_ $ 
    (client ^. ClientAge E.>. E.just (E.val 25))
    E.&&. (country ^. CountryName `E.in_` E.valList [ "Spain", "Germany" ])
    E.&&. (client ^. ClientCountry E.==. country ^. CountryId)
  E.orderBy [ E.asc (client ^. ClientLastName), E.asc (client ^. ClientFirstName) ]
  return client

getPeopleOver25FromSpainOrGermanyJoin :: MonadIO m => SqlPersistT m [Entity Client]
getPeopleOver25FromSpainOrGermanyJoin =
  E.select $ do
  client E.:& country <- E.from $ 
    E.table @Client `E.innerJoin` E.table @Country
    `E.on` \(client E.:& country) ->
      client ^. ClientCountry E.==. country ^. CountryId
  E.where_ $ 
    (client ^. ClientAge E.>. E.just (E.val 25))
    E.&&. (country ^. CountryName `E.in_` E.valList [ "Spain", "Germany" ])
  E.orderBy [ E.asc (client ^. ClientLastName), E.asc (client ^. ClientFirstName) ]
  return client

getMoneyByClient :: MonadIO m => SqlPersistT m [(Entity Client, E.Value (Maybe Double))]
getMoneyByClient = do
  E.select $ do
  client E.:& purchase <- E.from $
    E.table @Client `E.leftJoin` E.table @Purchase
    `E.on` \(client E.:& purchase) ->
      E.just (client ^. ClientId) E.==. purchase ?. PurchaseClient
  E.groupBy (client ^.ClientId)
  let s = E.sum_ (purchase ?. PurchaseAmount)
  return (client, s)

-- USES OLD SYNTAX!!
betterDiscount :: MonadIO m => SqlPersistT m ()
betterDiscount = E.update $ \product -> do
  let totalAmount = E.subSelectUnsafe $ do
                    purchase <- E.from $ E.table @Purchase
                    E.where_ $ product ^. ProductId E.==. purchase ^. PurchaseProduct
                    E.groupBy (purchase ^. PurchaseProduct)
                    return $ E.sum_ (purchase ^. PurchaseAmount)
  E.where_ $ E.isNothing totalAmount E.||. totalAmount E.<. E.just (E.val (10 :: Double))
  E.set product [ ProductPrice E.*=. E.val 0.9]

cleanProductStock' :: MonadIO m => SqlPersistT m ()
cleanProductStock' = E.delete $ do
  product <- E.from $ E.table @Product
  E.where_ $ 
    (product ^. ProductInStock E.==. E.val 0)
    E.&&. (E.notExists $ do 
           purchase <- E.from $ E.table @Purchase
           E.where_ (purchase ^. PurchaseProduct E.==. product ^. ProductId))

someFunc :: IO ()
someFunc = putStrLn "someFunc"
