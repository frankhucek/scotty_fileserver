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

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
   User  -- UserId is automatically generated, primary key
       name String  --UserName
       email String
       username String
       UniqueUsername username  -- uniqueness constraint
       deriving Show
  |]
