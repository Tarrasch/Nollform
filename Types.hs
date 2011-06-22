{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings #-}
module Types where
import Database.Persist
import Database.Persist.TH



-- Egna datatyper
data Sex = Tjej | Kille
  deriving (Show, Read, Eq)
derivePersistField "Sex"
  
data Helgon = Hacke
  deriving (Show, Read, Eq)
derivePersistField "Helgon"

data Farg = Orange
  deriving (Show, Read, Eq)
derivePersistField "Farg"

