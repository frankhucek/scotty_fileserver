{-# LANGUAGE OverloadedStrings #-}
import Prelude as P
import System.Environment

import Data.Monoid (mconcat, mappend)

import Data.Text.Lazy as T

import Control.Monad.IO.Class (liftIO)

import Web.Scotty as S
import Text.Blaze.Html.Renderer.Text as B
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static

import Database.Persist
import Database.Persist.Sqlite

import Pages
import Model


main = do envPort <- getEnv "PORT"
	  scotty (read envPort) $ do
	    liftIO $ runDB $ runMigration migrateAll
	    middleware logStdoutDev
	    middleware $ staticPolicy (noDots >-> addBase "static")
	    routes

routes :: ScottyM ()
routes = do S.get "/" $ html "Hi"

	    S.get "/users" $ do users <- liftIO getUsers
				blaze $ userPageHtml $
				  P.head $ fmap (T.pack . show) users

	    S.get "/posts" $ do posts <- liftIO getAllPosts
				blaze $ userPageHtml $
				      mconcat $ fmap (T.pack . show) posts

	    S.get "/users/:name" $ do name <- param "name"
				      users <- liftIO $ getUsersByName name
				      if P.null users then next
					 else blaze $ userPageHtml $
						mconcat $ fmap (T.pack . show) users
	    S.get "/create" $ (do name <- param "name"
				  email <- param "email"
				  usn <- param "usn"
				  liftIO $ insertUser name email usn
				  redirect $ "/users/" `mappend` T.pack name) `rescue` (\m -> text m)
	    notFound $ text "nope"

blaze = S.html . renderHtml

getUsersByName :: T.Text -> IO [Entity User]
getUsersByName name = runDB $ selectList [UserName ==. T.unpack name] [LimitTo 5]

getUsers :: IO [Entity User]  -- select * from User;
getUsers = runDB $ selectList [] []

insertUser n e u = runDB $ insert $ User n e u

getAllPosts :: IO [Entity Post]
getAllPosts = runDB $ selectList [] []

getPostsByUserId :: UserId -> IO [Entity Post]
getPostsByUserId uid = runDB $ selectList [PostAuthor ==. uid] []

runDB = runSqlite "db.sqlite3"
-- database file. ":memory:" for ram
