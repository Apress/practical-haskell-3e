{-# LANGUAGE TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, TypeFamilies, GADTs, DerivingStrategies, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleInstances, UndecidableInstances, DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

import Data.Pool
import Database.Persist.TH
import Database.Persist.Sql
import qualified Database.Persist.Sqlite as Db
import Control.Monad.Logger
import Control.Monad.IO.Class
import qualified Data.Text.Read as Read

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Hamlet

import Text.Digestive
import Text.Digestive.Util
import Text.Digestive.Blaze.Html5
import Network.Wai.Digestive

import Data.String

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Country
  name String
  canWeSend Bool
  deriving Show
Product
  name String
  description String
  price Double
  inStock Int
  deriving Show
|]

main :: IO ()
main = run 3000 $ \request respond ->
  respond $ responseLBS ok200 [] $
    mconcat [ "<html><body>"
            , "  <h1>Hello Practical Haskell!</h1>"
            , "</body></html>"]

responseHtml status = responseLBS status [("Content-Type", "text/html")]

redirect newLocation = responseLBS seeOther303 [("Location", fromString newLocation)] ""

app1 :: Application 
app1 request respond = 
  case (parseMethod (requestMethod request), pathInfo request) of
    (Right GET, ["about"]) -> 
      respond $ responseHtml ok200 $
        mconcat [ "<html><body>"
                , "  <h1>Hello Practical Haskell!</h1>"
                , "</body></html>"]
    _ ->
      respond $ responseHtml notFound404
        "<h1>Not found :(</h1>"

mainDb :: IO ()
mainDb = do
  Db.runSqlite "example.db" $ Db.runMigration migrateAll
  runNoLoggingT $ Db.withSqlitePool "example.db" 10 $ \pool ->
    liftIO $ run 3000 (app2 pool)

app2 :: Pool SqlBackend -> Application 
app2 pool request respond = 
  case (parseMethod (requestMethod request), pathInfo request) of
    (Right GET, ["product", productIdParam])
      | Right (productId, "") <- Read.decimal productIdParam -> do
      product <- flip runSqlPersistMPool pool $
        Db.get $ ProductKey productId
      case product of
        Just Product { .. } -> respond $ responseHtml ok200 $
          renderHtml $
            H.html $ do
              H.head $
                H.title "Time Machine Store"
              H.body $ do
                H.h1 $ H.toHtml productName
                H.p H.! A.id "descr" $ H.toHtml productDescription
        Nothing -> respond $ responseHtml notFound404
          "<h1>Product not found :(</h1>"
    (Right GET, ["about"]) -> 
      respond $ responseHtml ok200 $
        mconcat [ "<html><body>"
                , "  <h1>Hello Practical Haskell!</h1>"
                , "</body></html>"]
    _ ->
      respond $ responseHtml notFound404
        "<h1>Not found :(</h1>"

app3 :: Pool SqlBackend -> Application 
app3 pool request respond = 
  case (parseMethod (requestMethod request), pathInfo request) of
    (Right GET, ["product", productIdParam])
      | Right (productId, "") <- Read.decimal productIdParam -> do
      product <- flip runSqlPersistMPool pool $
        Db.get $ ProductKey productId
      case product of
        Just Product { .. } -> respond $ responseHtml ok200 $
          renderHtml [shamlet|
            <html>
              <head>
                <title>Time Machine Store
              <body>
                <h1>#{productName}
                <p id=descr>#{productDescription}
          |]
        Nothing -> respond $ responseHtml notFound404
          "<h1>Product not found :(</h1>"
    (Right GET, ["products"]) -> do
      products <- flip runSqlPersistMPool pool $
        Db.selectList [] []
      respond $ responseHtml ok200 $
        renderHtml [shamlet|
          <html>
            <head>
              <title>Time Machine Store
            <body>
              <h1>Products
              <table>
                <tr>
                  <th>Name
                  <th>Description
                $forall Db.Entity _ p <- products
                  <tr>
                    <td>#{productName p}
                    <td>#{productDescription p}
        |]
    (Right GET, ["new-product"]) -> do
      view <- getForm "product" productForm
      respond $ responseHtml ok200 $ renderHtml $
        H.html $ do
          H.head $ H.title "Time Machine Store"
          H.body $ productView (fmap H.toHtml view)
    (Right POST, ["new-product"]) -> do
      (view, product) <- postForm "product" productForm (\_ -> bodyFormEnv undefined request)
      case product of
        Just p -> do
          ProductKey newId <- flip runSqlPersistMPool pool $ Db.insert p
          respond $ redirect $ "/product/" <> show newId
        Nothing -> respond $ responseHtml ok200 $ renderHtml $
          H.html $ do
            H.head $ H.title "Time Machine Store"
            H.body $ productView (fmap H.toHtml view)
    (Right GET, ["about"]) -> 
      respond $ responseHtml ok200 $
        mconcat [ "<html><body>"
                , "  <h1>Hello Practical Haskell!</h1>"
                , "</body></html>"]
    _ ->
      respond $ responseHtml notFound404
        "<h1>Not found :(</h1>"

countryForm :: Monad m => Form String m Country
countryForm = Country <$> "name" .: string Nothing
                      <*> "send" .: bool (Just True)

productForm :: Monad m => Form String m Product
productForm
  = Product <$> "name" .: string Nothing
            <*> "description" .: string Nothing
            <*> "price" .: validate isANumber (string Nothing)
            <*> "inStock" .: check "Must be >= 0" (>= 0) (validate isANumber (string Nothing))

isANumber :: (Num a, Read a) => String -> Result String a
isANumber = maybe (Error "Not a number") Success . readMaybe

productView :: View H.Html -> H.Html
productView view =
  form view "/new-product" $ do
    label "name" view "Name:"
    inputText "name" view
    H.br
    inputSubmit "Submit"