{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import MySite

import Yesod.Form (aopt, areq)
import qualified Yesod.Form as F
import qualified Yesod.Form.Fields as Fi
import Yesod.Form.Jquery
import Yesod.Form.Nic
import Control.Applicative
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T 
import Data.Time (getCurrentTime)

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
handleRootR :: Handler RepHtml
handleRootR = do
    let settings = def {
                       jdsChangeMonth = True,
                       jdsChangeYear = True
                   }
    ((res, form), enctype) <- F.runFormPost $ F.renderTable $ (Svar)
     <$> areq Fi.textField "Förnamn" Nothing
     <*> areq Fi.textField "Efternamn" Nothing
     <*> areq Fi.textField "Epost"{F.fsTooltip = Just "Se not ovan"} Nothing
     <*> areq (Fi.radioField [("Tjej", Tjej), ("Kille", Kille)]) "Kön" Nothing -- special
     <*> areq (jqueryDayField settings) "Födelsedatum" Nothing -- special
     <*> areq Fi.textField "Hemort"{F.fsTooltip = Just "(ej adress)"} Nothing
     <*> areq Fi.textField "Telefonnummer" Nothing
     
     <*> areq (Fi.radioField [("Orange", Orange)]) "Favoritfärg" Nothing
     <*> areq (Fi.radioField [("Hacke", Hacke)]) "Favorithelgon" Nothing
     <*> aopt myNicHtmlField "Beskrivning"{F.fsTooltip = Just "lite om dig själv"} Nothing -- special
     <*> aopt myNicHtmlField "Fritid"{F.fsTooltip = Just "lite om din fritid"} Nothing -- special
     <*> aopt Fi.textField "Saker bra att veta om dig"{F.fsTooltip = Just "allgeri, specialkost, eller annat"} Nothing
     <*> areq Fi.textField "Dina förväntingar på nollningen" Nothing

     <*> areq (Fi.radioField list_1_5) ("Din inställning till nollningen"{F.fsTooltip = Just "5 = du tror du kommer älska nollningen"}) Nothing
     <*> areq (Fi.radioField list_1_5) ("Din inställning till studier") Nothing
     <*> areq (Fi.radioField list_1_5) ("Din inställning till fester") Nothing
     <*> areq (Fi.radioField list_1_5) ("Din inställning till alkohol") Nothing
     <*> areq (Fi.radioField list_1_5) ("Din inställning till lekar") Nothing

    success <- case res of  
                F.FormSuccess svar -> do
                  now <- liftIO getCurrentTime
                  runDB $ insert (svar now)
                  return True
                _                 -> return False         
    
    -- mu <- maybeAuth -- behövs ej va?
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "Nollformulär 2011 för blivande datateknologer"
        addWidget $(widgetFile "formular")
  where   
    myNicHtmlField :: YesodNic master =>
      F.Field (GWidget sub master ()) F.FormMessage Html
    myNicHtmlField = nicHtmlField
    list_1_5 = [(T.pack $ show i, i :: Int) | i <-[1..5]]








