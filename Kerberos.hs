{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Kerberos 
  (authKerberos)
  where

--import Yesod
import Text.Hamlet
import Yesod.Auth
import Yesod.Handler
import Yesod.Widget
import Yesod.Form
import qualified Data.Text as T
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromJust)
import Data.Text (Text)
import System.Process
import System.Exit (ExitCode(ExitSuccess))
import Data.Monoid (mappend)
import Control.Monad.IO.Class 

data ValidationResult = Ok 
                      | Error Text


authKerberos :: YesodAuth m => AuthPlugin m
authKerberos =
    AuthPlugin "kerberos" dispatch login
  where
    loginRoute :: AuthRoute
    loginRoute = PluginR "kerberos" ["login"]
    login :: (Yesod.Handler.Route Auth -> Yesod.Handler.Route m)
                         -> Yesod.Widget.GWidget s m ()
    login tm = [hamlet|
    <div id="header">
        <h1>Login

    <div id="login">
        <form method="post" action="@{tm loginRoute}">
            <table>
                <tr>
                    <th>Username:
                    <td>
                        <input id="x" name="username" autofocus="" required>
                <tr>
                    <th>Password:
                    <td>
                        <input type="password" name="password" required>
                <tr>
                    <td>&nbsp;
                    <td>
                        <input type="submit" value="Login">

            <script>
                if (!("autofocus" in document.createElement("input"))) {
                    document.getElementById("x").focus();
                }              
|]

    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch _ _              = notFound
    

-- | Handle the login form
postLoginR :: (YesodAuth y)
           => GHandler Auth y ()
postLoginR = do
    (mu,mp) <- runInputPost $ (,)
        <$> iopt textField "username"
        <*> iopt textField "password"

    validation <- case (mu,mp) of
        (Nothing, _      ) -> return $ Error "Please fill in the username"
        (_      , Nothing) -> return $ Error "Please fill in the password"
        (Just u , Just p ) -> validateUser (u `mappend` "/net" ,p)

    case validation of
        Ok -> do
            let cid = fromJust mu -- this cant fail
            let creds = Creds 
                  { credsIdent  = cid
                  , credsPlugin = "Kerberos"
                  , credsExtra  = []
                  }                                 
            setCreds True creds
        (Error message) -> do
            setMessage [hamlet| Error: #{message} |]
            toMaster <- getRouteToMaster
            redirect RedirectTemporary $ toMaster LoginR

-- Make safe to pass as shell argument
-- I'm really unsure if this is failproof!!!!!!!
escape :: Text -> Text
escape = T.filter (/= '\'')  

-- | Given a (user,password) in plaintext, accept any
validateUser :: (Text, Text) -> GHandler sub y ValidationResult
validateUser (cid,password) = do
    let cid_safe = escape cid   
    liftIO $ putStrLn $ T.unpack cid_safe
    (exitCode, out, err) <- liftIO $ readProcessWithExitCode 
        "kinit" [T.unpack cid_safe] (T.unpack password)
    case exitCode of
      ExitSuccess   -> return Ok
      _             -> return . Error $ T.pack err
   


{-
    fmap (== ExitSuccess) $ liftIO io
  where
    io :: IO ExitCode
    io   = rawSystem cmd args 
    cmd  = T.unpack $ "echo " ++ password ++ " | kinit " 
    args = [T.unpack $ cidnet]
    (++) = mappend
    cidnet = cid ++ "/net"
    -- rawSystem "kdestroy" []    
-}
