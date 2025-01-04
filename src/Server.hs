{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Lucid
import Servant ( Server )
import Servant.API ( Get, type (:<|>)(..) )
import qualified Servant.HTML.Lucid
import Servant.Static.TH ( createApiAndServerDecs )

type RootAPI
  = Get '[Servant.HTML.Lucid.HTML] (Html ())

$(createApiAndServerDecs "ResourcesApi" "resourcesApi" "static")

type API = RootAPI :<|> ResourcesApi

page :: Html ()
page = html_ do
  head_ do
    title_ "Introduction page."
    link_ [rel_ "stylesheet", type_ "text/css", href_ "screen.css"]
  body_ do
    div_ [id_ "header"] "Syntax"
    p_ "This is an example of Lucid syntax."
    ul_ $ mapM_ (li_ . toHtml . show @Int) [1,2,3]

handler :: Server API
handler = pure page :<|> resourcesApi
