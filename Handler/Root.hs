{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Text(Text) 
import Data.Time (UTCTime, getCurrentTime)

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
     <*> areq Fi.textField "Epost" Nothing
     <*> areq (Fi.radioField [("Tjej", Tjej), ("Kille", Kille)]) "Kön" Nothing
     <*> areq (jqueryDayField settings) "Födelsedatum" Nothing
     <*> areq Fi.textField "Hemort" Nothing
     <*> areq Fi.textField "Telefonnummer" Nothing
     
     <*> areq (Fi.selectField $ replicate 5 ("Orange", Orange)) "Favoritfärg" Nothing
     <*> areq (Fi.selectField $ replicate 5 ("Hacke Hackspett", Hacke)) "Favorithelgon" Nothing
     <*> areq myNicHtmlField "Beskrivning"{F.fsTooltip = Just "lite om dig själv"} Nothing
     <*> areq myNicHtmlField "Fritid"{F.fsTooltip = Just "lite om din fritid"} Nothing
     <*> areq myNicHtmlField "Saker bra att veta om dig"{F.fsTooltip = Just "allgeri, specialkost, eller annat"} Nothing
     <*> areq myNicHtmlField "Dina förväntningar på nollningen" Nothing
     <*> aopt Fi.textField "Spelar du något instrument?" Nothing

     <*> areq (Fi.radioField $ list_1_5 "Är nog inget för mig" "Kan knappt vänta!") ("Din inställning till nollningen") Nothing
     <*> areq (Fi.radioField $ list_1_5 "Trist" "Uppfriskande") ("Din inställning till studier") Nothing
     <*> areq (Fi.radioField $ list_1_5 "Vadå fest?" "Varje dag helst") ("Din inställning till fester") Nothing
     <*> areq (Fi.radioField $ list_1_5 "Nykterist" "Packad varje dag") ("Din inställning till alkohol") Nothing
     <*> areq (Fi.radioField $ list_1_5 "Fånigt" "Askul") ("Din inställning till lekar") Nothing

    success <- case res of  
                F.FormSuccess (svar :: UTCTime -> Svar) -> do
                  now <- liftIO getCurrentTime
                  runDB $ insert (svar now)
                  return True
                _                 -> return False         
    
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "Nollformulär 2011 för blivande datateknologer"
        addWidget $(widgetFile "formular")
  where   
    myNicHtmlField :: YesodNic master =>
      F.Field (GWidget sub master ()) F.FormMessage Html
    myNicHtmlField = nicHtmlField
    list_1_5 :: String -> String -> [(Text, Int)]
    list_1_5 s1 s5 =  
        [(T.pack $ show i ++ f i, i :: Int) | i <-[1..5]]
      where
        f 1 = " - " ++ s1
        f 5 = " - " ++ s5
        f _ = ""





