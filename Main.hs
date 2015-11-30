{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import           Pages
import           Types

import           Control.Monad
import           Control.Monad.IO.Class               (liftIO)
import           Data.ByteString.Char8                as B (elem, pack, unpack)
import           Data.ByteString.Lazy.Char8           as BL (writeFile)
import           Data.Functor                         ((<$>))
import           Data.List                            (isPrefixOf)
import           Data.Monoid                          ((<>))
import qualified Data.Text.Lazy                       as T
import qualified Data.Text.Lazy.IO                    as TIO
import           Prelude                              as P
import           System.Directory                     (doesDirectoryExist,
                                                       doesFileExist,
                                                       getDirectoryContents)
import           System.Environment
import           System.IO
import           System.Locale                        (defaultTimeLocale)
import qualified System.Posix.Files                   as F
import           System.Process

import           Network.Wai.Handler.Warp             (Settings (..),
                                                       defaultSettings, setPort)
import           Network.Wai.Middleware.AddHeaders    (addHeaders)
import qualified Network.Wai.Middleware.HttpAuth      as Auth
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Network.Wai.Parse                    as N (fileContent,
                                                            fileName)
import           Text.Blaze.Html.Renderer.Text        (renderHtml)
import           Web.Scotty                           as S

-- run with environment var PORT set to whatever port
-- for auth, make a file "auth.txt"
-- containing two lines: username and then password

main = do envPort <- getEnv "PORT"
          h <- openFile "auth.txt" ReadMode
          c <- hGetContents h
          let l = P.lines c
          scottyOpts
            (Options 1 (setPort (read envPort) defaultSettings)) $ do
              middleware $
                addHeaders [(B.pack "X-Clacks-Overhead", B.pack "GNU Terry Pratchett")]
              middleware logStdoutDev
              middleware $ staticPolicy $
                hasPrefix "static/" <|> hasPrefix "served_files/"
              when (P.length l == 2) $ do
                let usn = head l
                    passwd = l !! 1
                middleware $ Auth.basicAuth
                  (\u p -> return $ u == B.pack usn && p == B.pack passwd) " Welcome "
              routes

routes :: ScottyM ()
routes = do S.get "/" $ do c <- liftIO $ readFile "/home/frank/bin_storage/LoginWelcome/LoginWelcome.txt"
                           blaze $ homePage c

            S.get "/files/" $ serveDir ""

            S.get (regex "^/files/(.+)$") $ do (f :: String) <- param "1"
                                               b <- liftIO $ doesFileExist (prefix <> f)
                                               unless b $ next
                                               liftIO $ print $ "opening file: " ++ f
                                               file' f

            S.get (regex "^/files/(.+)$") $ do dir <- param "1"
                                               liftIO $ print $ "opening dir: " ++ dir
                                               serveDir dir

            S.get "/upload" $ blaze uploadPage

            S.post "/uploaded" $ do fs <- files
                                    liftIO $ handleFiles fs
                                    html $ T.pack $ show fs  -- does not get displayed when using dropzone

            S.get "/torrents" $ do blaze addTorrentPage

            S.post "/torrents" $ do (magnet :: String) <- param "magnet"
                                    liftIO $ do
                                      createProcess $ shell ("transmission-remote -a '" ++ magnet ++ "'")
                                    redirect "/"
            --S.get "/donnerator" $ do dism <- liftIO getDonnered
             --                        blaze $ donnerPage dism

            --S.get "/donneradd" $ blaze donnerAddPage

            --S.post "/donneradd" $ do (line :: String) <- param "donner_line"
              --                       liftIO $
              --                         appendFile "/home/miles/ruby/donnerator/donnerisms.txt" $ line ++ "\n"
              --                       redirect "/donnerfile"

            --S.get "/donnerfile" $ file "/home/miles/ruby/donnerator/donnerisms.txt"

            S.get "/favicon.ico" $ file "static/favicon.ico"

            S.notFound $ html "not here"

blaze = S.html . renderHtml

file' :: String -> ActionM ()
file' f = file $ prefix <> f

dirInfo :: String -> IO ([FileEntry], [FileEntry])
dirInfo p = do let path = if p /= "" then prefix ++ p ++ "/" else prefix
               entries <- getDirectoryContents path
               fs <- filterM (doesFileExist . (path ++ )) entries
               ds <- filterM (doesDirectoryExist . ( path ++)) entries

               fattrs <- mapM (F.getFileStatus . (path ++)) fs
               dattrs <- mapM (F.getFileStatus . (path ++)) ds

               -- will have to write zipWith4 if we add more properties to FileEntry
               let fs'  = zipWith3 FileEntry fs (map (strTime . F.modificationTimeHiRes) fattrs) (map (showSize . fromIntegral . F.fileSize) fattrs)
                   ds'  = zipWith3 FileEntry ds (map (strTime . F.modificationTimeHiRes) dattrs) (map (showSize . fromIntegral . F.fileSize) dattrs)
                   ds'' = filter (\f -> feName f `notElem` [".",".."]) ds'

               print path >> print entries >> print fs' >> print ds'' -- debugging

               return (fs', ds'')

  where strTime x = "fish" --formatTime defaultTimeLocale "%F" . posixSecondsToUTCTime
        showSize :: Int -> String
        showSize n
          | n < 1000    = show n                 ++ " b"
          | n < 1000000 = show (n `div` 1000)    ++ " k"
          | otherwise   = show (n `div` 1000000) ++ " m"

serveDir p = do (fs, ds) <- liftIO $ dirInfo p
                blaze $ template p $ renderDir p fs ds

prefix :: String
prefix = "served_files/"

-- type File = (Text, FileInfo ByteString) -- (field in form where came from, info)
-- FileInfo { fileName :: ByteString, fileContentType :: ByteString, fileContent :: c }
handleFiles :: [S.File] -> IO ()
handleFiles fs = let fis = map snd fs
                     fis' = filter (\f -> not ('/' `B.elem` fileName f))  fis
                 in void $ forM fis' $
                    \f ->
                     BL.writeFile (B.unpack (B.pack prefix <> fileName f)) $ fileContent f

--getDonnered :: IO String
--getDonnered =  do (_, Just hout, _, _) <- createProcess (proc "./donnerate.sh" []) { std_out = CreatePipe, cwd = Just "/home/miles/ruby/donnerator" }
--                  hGetContents hout
