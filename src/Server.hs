{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Lucid
import Servant ( Server )
import Servant.API ( Get, type (:<|>)(..), (:>) )
import qualified Servant.HTML.Lucid
import Servant.Static.TH ( createApiAndServerDecs )

type RootAPI
  = Get '[Servant.HTML.Lucid.HTML] (Html ())
type TwAPI
  = "tw" :> Get '[Servant.HTML.Lucid.HTML] (Html ())

$(createApiAndServerDecs "ResourcesApi" "resourcesApi" "static")

type API = RootAPI :<|> TwAPI :<|> ResourcesApi

tailwindcss_ex :: Html ()
tailwindcss_ex = doctype_ *> html_ do
  head_ do
    meta_ [charset_ "UTF-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/tw.css"]
    title_ "Tailwind Test."
  body_ do
    div_ [class_ "p-6 max-w-sm mx-auto bg-white rounded-xl shadow-lg flex items-center gap-x-4"] do
      div_ [class_ "shrink-0"] do
        img_ [class_ "size-12", src_ "/img/CC0.svg", alt_ "CC0 Logo"]
      div_ do
        div_ [class_ "text-xl font-medium text-black"] "ChitChat"
        p_ [class_ "text-slate-500"] "You have a new message!"

page :: Html ()
page = doctype_ *> html_ do
  head_ do
    meta_ [charset_ "UTF-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/tw.css"]
    title_ "Introduction page."
  body_ do
    div_ [id_ "header"] "Syntax"
    h1_ [class_ "text-3xl font-bold underline"] "Hello world!"
    p_ "This is an example of Lucid syntax."
    ul_ $ mapM_ (li_ . toHtml . show @Int) [1,2,3]

handler :: Server API
handler = pure page :<|> pure tailwindcss_ex :<|> resourcesApi
