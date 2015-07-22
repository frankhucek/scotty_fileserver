{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import           Types

import           Control.Monad.IO.Class               (liftIO)
import           Data.Monoid                          ((<>))
import qualified Data.Text.Lazy                       as T
import           Prelude                              as P
import           System.Environment
import           System.IO

import           Data.ByteString.Char8                as B (elem, pack, unpack)
import           Data.ByteString.Lazy.Char8           as BL (writeFile)

import           Network.Wai.Middleware.AddHeaders    (addHeaders)
import qualified Network.Wai.Middleware.HttpAuth      as Auth
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static

import           Text.Blaze.Html.Renderer.Text        (renderHtml)
import           Web.Scotty                           as S

import           Pages

-- for servedir
import           Control.Monad
import           Data.Time.Clock.POSIX                (posixSecondsToUTCTime)
import           Data.Time.Format                     (formatTime)
import           System.Directory                     (doesDirectoryExist,
                                                       doesFileExist,
                                                       getDirectoryContents)
import           System.Locale                        (defaultTimeLocale)
import qualified System.Posix.Files                   as F

-- for upload handling
import           Network.Wai.Parse                    as N (fileContent,
                                                            fileName)
-- for donnerator
import           System.Process

-- look into 'files' in Web.Scoty for uploads

main = do envPort <- getEnv "PORT"
          h <- openFile "auth.txt" ReadMode
          c <- hGetContents h
          let l = P.lines c
          scotty (read envPort) $ do
            middleware $ addHeaders $ [(B.pack "X-Clacks-Overhead", B.pack "GNU Terry Pratchett")]
            middleware logStdoutDev
            middleware static
            when (P.length l == 2) $ do
              let usn = l !! 0
                  passwd = l !! 1
              middleware $ Auth.basicAuth
                (\u p -> return $ u == B.pack usn && p == B.pack passwd) " Welcome "
            routes

routes :: ScottyM ()
routes = do S.get "/" $ blaze $ template "HOME" homePage

            S.get "/testing" $ blaze $ videoPage "/files/movies/archer_vice/05.10%20-%20Palace%20Intrigue%20Part%201.mp4"

            S.get (regex "^/files/$") $ serveDir ""  -- directory names must end in '/'

            S.get (regex "^/files/(.+/)$") $ do (fp :: String) <- param "1"
                                                liftIO $ print $ "opening directory: " ++ fp
                                                liftIO $ print fp
                                                serveDir fp

            S.get (regex "^/files/(.*[^/])$") $ do (fp :: String) <- param "1"
                                                   file' fp

            S.get "/files/:file" $ do fname <- param "file"
                                      file' fname

            S.get "/upload" $ blaze uploadPage

            S.post "/uploaded" $ do fs <- files
                                    liftIO $ handleFiles fs
                                    html $ T.pack $ show fs


            S.get "/donnerator" $ do dism <- liftIO $ getDonnered
                                     blaze $ donnerPage dism

            -- S.get "/donnerfile" $ blaze donnerFilePage


            S.notFound $ html "not here"

blaze = S.html . renderHtml

file' :: String -> ActionM ()
file' f = file $ prefix <> f

dirInfo :: String -> IO ([FileEntry], [FileEntry])
dirInfo p = do let path = prefix ++ p
               entries <- liftIO $ getDirectoryContents path
               fs <- liftIO $ filterM (doesFileExist . (path ++)) entries
               ds <- liftIO $ filterM (doesDirectoryExist . ( path ++)) entries

               fattrs <- mapM (F.getFileStatus . (path ++)) fs
               dattrs <- mapM (F.getFileStatus . (path ++)) ds

               let fs' = zip3 fs (map (strTime . F.modificationTimeHiRes) fattrs) (map (showSize . fromIntegral) $ map F.fileSize fattrs)
                   ds' = zip3 ds (map (strTime . F.modificationTimeHiRes) dattrs) (map (showSize . fromIntegral) $ map F.fileSize dattrs)

               liftIO $ print path >> print entries >> print fs' >> print ds' -- debugging
               return (fs', ds')

  where strTime ptime = formatTime defaultTimeLocale "%F - %T" (posixSecondsToUTCTime ptime)
        showSize :: Int -> String
        showSize n
          | n < 1000    = (show n)                 ++ " bytes"
          | n < 1000000 = (show (n `div` 1000))    ++ " kb"
          | otherwise   = (show (n `div` 1000000)) ++ " mb"

          --bytes




serveDir p = do (fs, ds) <- liftIO $ dirInfo p
                blaze $ template p $ renderDir p fs ds

prefix :: String
prefix = "served_files/"

-- type File = (Text, FileInfo ByteString) -- (field where came from, info)
-- FileInfo { fileName :: ByteString, fileContentType :: ByteString, fileContent :: c }
handleFiles :: [S.File] -> IO ()
handleFiles fs = let fis = map snd fs
                     fis' = filter (\f -> not ('/' `B.elem` fileName f))  fis
                 in void $ forM fis' $
                    \f ->
                     BL.writeFile (B.unpack (B.pack prefix  <> fileName f)) $ fileContent f


getDonnered :: IO String
getDonnered =  do (_, Just hout, _, _) <- createProcess (proc "./donnerate.sh" []) { std_out = CreatePipe, cwd = Just "/home/miles/ruby/donnerator" }
                  c <- hGetContents hout
                  return c
