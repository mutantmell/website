{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Lucid
import Servant ( Server )
import Servant.API ( Get, type (:<|>)(..), (:>), Capture )
import qualified Servant.API.ContentTypes.Lucid
import Servant.Static.TH ( createApiAndServerDecs )
import Data.Text ( Text )
import qualified Data.Text
import qualified Data.Text.IO
import Control.Monad.IO.Class (MonadIO(..))
import qualified CMark

type SvHtml = Servant.API.ContentTypes.Lucid.HTML

type RootAPI
  = Get '[SvHtml] (Html ())
type TwAPI
  = "tw" :> Get '[SvHtml] (Html ())
type PostAPI
  = "post" :> Capture "post" Text :> Get '[SvHtml] (Html ())

$(createApiAndServerDecs "ResourcesApi" "resourcesApi" "static")

type API = RootAPI :<|> TwAPI :<|> PostAPI :<|> ResourcesApi

wrap :: Monad m => Text -> HtmlT m () -> HtmlT m ()
wrap title body = doctype_ *> html_ do
  head_ do
    meta_ [charset_ "UTF-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/tw.css"]
    title_ (toHtml title)
  body_ body

tailwindcss_ex :: Html ()
tailwindcss_ex = wrap "Tailwind Test." do
  div_ [class_ "p-6 max-w-sm mx-auto bg-white rounded-xl shadow-lg flex items-center gap-x-4"] do
    div_ [class_ "shrink-0"] do
      img_ [class_ "size-12", src_ "/img/CC0.svg", alt_ "CC0 Logo"]
    div_ do
      div_ [class_ "text-xl font-medium text-black"] "ChitChat"
      p_ [class_ "text-slate-500"] "You have a new message!"

page :: Html ()
page = wrap "Introduction page." do
  div_ [id_ "header"] "Syntax"
  h1_ [class_ "text-3xl font-bold underline"] "Hello world!"
  p_ "This is an example of Lucid syntax."
  ul_ $ mapM_ (li_ . toHtml . show @Int) [1,2,3]

post :: forall m . (MonadIO m) => Text -> m (Html ())
post p = do
  -- todo: proper escaping, sanitizing, etc for the file path
  file <- liftIO $ Data.Text.IO.readFile ("./data/" <> (Data.Text.unpack p) <> ".md")
  let postHtml = CMark.commonmarkToHtml [CMark.optSafe] file
  pure $ wrap p do
    div_ [class_ "flex justify-center items-center"] do
      article_ [class_ "mx-auto prose lg:prose-xl"] do
        toHtmlRaw postHtml

handler :: Server API
handler = pure page :<|> pure tailwindcss_ex :<|> post :<|> resourcesApi
