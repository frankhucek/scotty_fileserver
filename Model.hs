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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Model where

-- import Database.Persist
import Database.Persist.TH
import qualified Data.Text.Lazy as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
   User  -- UserId is automatically generated, primary key
       name String  --UserName
       email String
       username String
       UniqueUsername username  -- uniqueness constraint
       deriving Show

   Post
       title String
       content T.Text
       author UserId
       deriving Show
  |]
