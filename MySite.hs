{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MySite
    ( MySite (..)
    , MySiteRoute (..)
    , resourcesMySite
    , Handler
    , Widget
    , maybeAuth
    , requireAuth
    , module Yesod.Helpers.Static
    , module Yesod.Handler
    , module Yesod.Widget
    , module Yesod.Dispatch
    , module Yesod.Persist
    , module Yesod.Content
    , module Yesod.Core
    , module Text.Blaze
    , module Control.Monad.IO.Class
    , module Control.Monad.Trans
    , module Settings
    , module Model
    , StaticRoute (..)
    , AuthRoute (..)
    , Sex(..)
    , Helgon(..)
    , Farg(..)
    , isNollkUser
    ) where

-- import Yesod
import Yesod.Helpers.Static
import Yesod.Handler
import Yesod.Widget
import Yesod.Dispatch
import Yesod.Persist
import Yesod.Core
import Yesod.Content
import Text.Blaze
import Control.Monad.Trans
import Control.Monad.IO.Class
import Yesod.Auth
import Yesod.Auth.OpenId
import Yesod.Auth.Email
import qualified Settings
import System.Directory
import qualified Data.ByteString.Lazy as L
import Database.Persist.GenericSql
import Settings (hamletFile, cassiusFile, luciusFile, juliusFile, widgetFile)
import Settings (isNollk)
import Model
import StaticFiles
import Data.Maybe (isJust)
import Control.Monad (join, unless)
import Network.Mail.Mime
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import Text.Jasmine (minifym)
import qualified Data.Text as T
import qualified Yesod.Form as F
import Yesod.Form.Jquery
import Yesod.Form.Nic
import Types
import Kerberos

isNollkUser :: User -> Bool
isNollkUser = isNollk . userIdent

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data MySite = MySite
    { getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Settings.ConnectionPool -- ^ Database connection pool.
    }

-- | A useful synonym; most of the handler functions in your application
-- will need to be of this type.
type Handler = GHandler MySite MySite

-- | A useful synonym; most of the widgets functions in your application
-- will need to be of this type.
type Widget = GWidget MySite MySite

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://docs.yesodweb.com/book/web-routes-quasi/
--
-- This function does three things:
--
-- * Creates the route datatype MySiteRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route MySite = MySiteRoute
-- * Creates the value resourcesMySite which contains information on the
--   resources declared below. This is used in Controller.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- MySite. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the MySiteRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "MySite" $(parseRoutesFile "config/routes")

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod MySite where
    approot _ = Settings.approot

    defaultLayout widget = do
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            widget
            addCassius $(Settings.cassiusFile "default-layout")
        hamletToRepHtml $(Settings.hamletFile "default-layout")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticroot setting in Settings.hs
    urlRenderOverride a (StaticR s) =
        Just $ uncurry (joinPath a Settings.staticroot) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : T.unpack ext'
        let content' =
                if ext' == "js"
                    then case minifym content of
                            Left _ -> content
                            Right y -> y
                    else content
        let statictmp = Settings.staticdir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        let fn' = statictmp ++ fn
        exists <- liftIO $ doesFileExist fn'
        unless exists $ liftIO $ L.writeFile fn' content'
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", T.pack fn] [], [])

-- How to run database actions.
instance YesodPersist MySite where
    type YesodDB MySite = SqlPersist
    runDB db = liftIOHandler
             $ fmap connPool getYesod >>= Settings.runConnectionPool db

instance YesodAuth MySite where
    type AuthId MySite = UserId

    -- Where to send a user after successful login
    loginDest _ = ViewR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (uid, _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User (credsIdent creds)

    authPlugins = [ authKerberos
                  ]


instance YesodNic MySite
instance YesodJquery MySite  
   
instance RenderMessage MySite F.FormMessage where
  renderMessage _ _ F.MsgSelectNone = "(obesvarad)"
  renderMessage _ _ other = F.defaultFormMessage other
  

