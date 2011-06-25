{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Model where

--import Yesod
import Yesod.Persist
import Types
import Data.Text (Text)
import Data.Time
import Text.Blaze

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist, mkMigrate "migrateAll"] $(persistFile "config/models")
