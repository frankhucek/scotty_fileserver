{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE BangPatterns        #-}
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
import           System.Posix.Escape                  (escape)
import           System.Locale                        (defaultTimeLocale)
import qualified System.Posix.Files                   as F
import           System.Process

import           Text.Blaze                           (preEscapedString)
import           Text.Blaze.Html5                     as H (a, div, p, script,
                                                            table, tbody,
                                                            toHtml, (!))
import           Text.Blaze.Html5.Attributes          as A (class_, href, id,
                                                            type_)

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
                                    liftIO $ handleFiles prefix fs
                                    html $ T.pack $ show fs  -- does not get displayed when using dropzone

            S.get "/torrents" $ blaze addTorrentPage

            S.post "/torrents" $ do (magnet :: String) <- param "magnet"
                                    -- (file :: S.File) <- param "torrentfiles"
                                    if (magnet /= "") then liftIO $ void $ do
                                      createProcess $ shell ("transmission-remote -a '" ++ escape magnet ++ "'")
                                      else liftIO $ return ()
                                    redirect "/torrents"

            S.post "/torrentupload" $ do file <- files
                                         liftIO $ handleFiles torrentdir file
                                         redirect "/torrents"

            S.get "/torrentstatus" $ do
                 js <- liftIO $ readFile "static/js/torrentstatus.js"
                 blaze $ torrentStatusPage js
                                 -- add functionality to move files to specified folders

            S.get "/torrentstatusraw" $ do
                 res <- liftIO $ shellWithOut "transmission-remote --debug -l 2>&1 | sed '1,/200 OK/d'| sed '1,/got response/d'| grep -A 1 -- '--------'| head -n 2| tail -n 1"
                 S.json res

            -- handle input from stuff on torrentstatus page, and move downloaded files accordingly
                 -- adjust transmission directory for file also
            S.post "/torrentfilemove" $ do
              (torrentnum :: Int) <- param "torrentnumber"
              (movedir :: String) <- param "movedir"
              -- liftIO $ putStrLn $ "torrent number to move " ++ show torrentnum ++ " move to: " ++ movedir
              if (torrentnum >= 0 && movedir /= "") then liftIO $ void $ do
                createProcess $ shell ("transmission-remote -t " ++ (escape $ show torrentnum) ++ " --move " ++ escape movedir ++ " --find " ++ escape movedir ++ " && transmission-remote -t " ++ (escape $ show torrentnum) ++ " -v")
                else liftIO $ return ()
              redirect "/torrentstatus"


            --S.get "/torrentstatus" $ do
              --res <- liftIO $ do
                --(_, Just hout, _, _) <- createProcess (shell "transmission-remote -l") { std_out = CreatePipe }
                --hGetContents hout
              --script <- liftIO $ readFile "refreshscript.txt"
              --let rlines = lines res
                                 -- TODO parse this better in terms of tabs etc
                  --output = foldl (>>) mempty $ (p . toHtml) <$> rlines
                  --script' = toHtml script
              --blaze $ template "torrent list" (output `mappend` script')

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


shellWithOut :: String -> IO String
shellWithOut s = do
  (_, Just hout, __, pHandle) <- createProcess (shell s) { std_out = CreatePipe }
  waitForProcess pHandle
  c <- hGetContents' hout
  hClose hout
  return c

hGetContents' h = hGetContents h >>= (\s -> length s `seq` return s)


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
          | n < 1000000 = show (P.div n 1000)    ++ " k"
          | otherwise   = show (P.div n 1000000) ++ " m"

serveDir p = do (fs, ds) <- liftIO $ dirInfo p
                blaze $ template p $ renderDir p fs ds

prefix :: String
prefix = "served_files/"

torrentdir :: String
torrentdir = "/home/frank/torrents/"

-- type File = (Text, FileInfo ByteString) -- (field in form where came from, info)
-- FileInfo { fileName :: ByteString, fileContentType :: ByteString, fileContent :: c }
handleFiles :: String -> [S.File] -> IO ()
handleFiles pref fs = let fis = map snd fs
                          fis' = filter (\f -> not ('/' `B.elem` fileName f))  fis
                      in void $ forM fis' $
                         \f ->
                         BL.writeFile (B.unpack (B.pack pref <> fileName f)) $ fileContent f

--getDonnered :: IO String
--getDonnered =  do (_, Just hout, _, _) <- createProcess (proc "./donnerate.sh" []) { std_out = CreatePipe, cwd = Just "/home/miles/ruby/donnerator" }
--                  hGetContents hout
