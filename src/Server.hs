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
    meta_ [charset_ "UTF-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "tw.css"]
    title_ "Introduction page."
  body_ do
    div_ [id_ "header"] "Syntax"
    h1_ [class_ "text-3xl font-bold underline"] "Hello world!"
    p_ "This is an example of Lucid syntax."
    ul_ $ mapM_ (li_ . toHtml . show @Int) [1,2,3]

handler :: Server API
handler = pure page :<|> resourcesApi
