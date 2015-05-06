{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Model where

-- import Database.Persist
import Database.Persist.TH
import qualified Data.Text.Lazy as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
   User
       name String
       email String
       username String
       UniqueUsn username
       deriving Show

   Post
       title String
       content T.Text
       authorId UserId -- foreign key constraint onto primary key
       deriving Show
  |]
