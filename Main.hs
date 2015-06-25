{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import           Control.Monad.IO.Class               (liftIO)
import           Data.Monoid                          (mappend, mconcat)
import           Data.Text.Lazy                       as T
import           Prelude                              as P
import           System.Environment
import           System.IO

import           Data.ByteString.Char8                as BS (pack)
import qualified Network.Wai.Middleware.HttpAuth      as Auth
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Text.Blaze.Html.Renderer.Text        as B
import           Web.Scotty                           as S

import           Pages

-- for servedir
import           Control.Monad
import           System.Directory                     (doesDirectoryExist,
                                                       doesFileExist,
                                                       getDirectoryContents)

-- look into 'files' in Web.Scoty for uploads

main = do envPort <- getEnv "PORT"
          h <- openFile "auth.txt" ReadMode
          c <- hGetContents h
          let l = P.lines c
          scotty (read envPort) $ do
            middleware logStdoutDev
            middleware static
            when (P.length l == 2) $ do
              let usn = l !! 0
                  passwd = l !! 1
              middleware $ Auth.basicAuth
                (\u p -> return $ u == (BS.pack usn) && p == (BS.pack passwd)) " Welcome "
            routes

routes :: ScottyM ()
routes = do S.get "/" $ html "hi"

            S.get "/testing" $ serveDir "stuff/"

            S.get (regex "^/files/$") $ do serveDir ""  -- directory names must end in '/'

            S.get (regex "^/files/(.+/)$") $ do (fp :: String) <- param "1"
                                                liftIO $ print fp
                                                serveDir fp

            S.get (regex "^/files/(.*[^/])$") $ do (fp :: String) <- param "1"
                                                   file' $ fp

            S.get "/files/:file" $ do fname <- param "file"
                                      file' $ fname

            S.notFound $ html "not here"

blaze = S.html . renderHtml

file' :: String -> ActionM ()
file' f = file $ prefix `mappend` f

dirInfo :: String -> IO ([String], [String])
dirInfo p = do let path = prefix ++ p
               entries <- liftIO $ getDirectoryContents path
               fs <- liftIO $ filterM (doesFileExist . (path ++)) entries
               ds <- liftIO $ filterM (doesDirectoryExist . ( path ++)) entries
               liftIO $ print path >> print entries >> print fs >> print ds -- debugging
               return (fs, ds)

serveDir p = do (fs, ds) <- liftIO $ dirInfo p
                blaze $ template p $ renderDir p fs ds

prefix :: String
prefix = "static/" -- "/home/miles/haskell/scotty/fileserver/static/"
