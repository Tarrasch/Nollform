{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.View where

import MySite

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
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "Kolla igenom nollenkäter"
        addWidget $(widgetFile "view")
