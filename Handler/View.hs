{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.View where

import MySite
import Data.Text (Text) 
import qualified Data.Text as T
import Data.Monoid
import Data.List
import Data.Function

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getViewR :: Handler RepHtml
getViewR = do
    mu <- maybeAuth
    xs <- case mu of
            (Just u) -> do
              let query = selectList [] [SvarCreatedDesc] 0 0
              list <- fmap (map snd) $ runDB query
              return $ nubBy ((==) `on` svarEpost) list
            _        -> return ([] :: [Svar])
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "Kolla igenom nollenkäter"
        addWidget $(widgetFile "view")
        
siffror :: Svar -> [Text]
siffror s = [desc `mappend` ": " `mappend` (T.pack $ show (f s)) | (desc, f) <- fs]
  where fs = zip
              ["nollning","studier","fest","alkohol","lekar"] 
              [svarInstNoll,svarInstStud,svarInstFest,svarInstAlko,svarInstLek] 
