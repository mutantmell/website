{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Lucid
import Lucid.Base (commuteHtmlT2)
import Servant ( Server, Raw )
import Servant.API ( Get, type (:<|>)(..), (:>), Capture )
import qualified Servant.API.ContentTypes.Lucid
import Data.Text ( Text )
import qualified Data.Text
import qualified Data.Text.IO
import Control.Monad.IO.Class (MonadIO(..))
import qualified CMark
import Servant.Server.StaticFiles (serveDirectoryWebApp)

type SvHtml = Servant.API.ContentTypes.Lucid.HTML
type GetHtml = Get '[SvHtml] (Html ())

type RootApi = GetHtml
type AboutApi = "about" :> GetHtml
type BlogApi = "blog" :> GetHtml
type BlogPostApi = "blog" :> Capture "post" Text :> GetHtml

-- $(createApiAndServerDecs "ResourcesApi" "resourcesApi" "static")

type API = RootApi
      :<|> AboutApi
      :<|> BlogApi
      :<|> BlogPostApi
      :<|> "static" :> Raw

wrap :: Monad m => Text -> HtmlT m () -> HtmlT m ()
wrap title body = doctype_ *> html_ do
  head_ do
    meta_ [charset_ "UTF-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/css/tw.css"]
    title_ (toHtml title)
  -- TODO: better bg color?
  body_ [class_ "bg-indigo-800"] body

tailwindcss_ex :: Html ()
tailwindcss_ex = wrap "Tailwind Test." do
  div_ [class_ "p-6 max-w-sm mx-auto bg-white rounded-xl shadow-lg flex items-center gap-x-4"] do
    div_ [class_ "shrink-0"] do
      img_ [class_ "size-12", src_ "/img/CC0.svg", alt_ "CC0 Logo"]
    div_ do
      div_ [class_ "text-xl font-medium text-black"] "ChitChat"
      p_ [class_ "text-slate-500"] "You have a new message!"

rootPage :: Html ()
rootPage = wrap "Introduction page" do
  header_ [class_ "bg-amber-300 border-b-2 border-black"] do
    div_ [class_ "py-6 mx-12 max-w-sm flex flex-row gap-x-4 justify-left text-xl"] do
      a_ [href_ "/", class_ "text-blue-600"] "mutantmell.net" -- "text-yellow-950"
      a_ [href_ "/about", class_ "text-blue-600"] "About"
      a_ [href_ "/blog", class_ "text-blue-600"] "Blog"
  div_ [class_ "p-8 my-12 mx-10 border-2 border-black bg-amber-300 gap-x-4"] do
    h1_ [class_ "text-yellow-950 text-2xl flex justify-left"] "mutantmell's spot on the internet"
    div_ [class_ "p-6 max-w-sm mx-auto flex items-center gap-x-4"] do
      p_ [] "This is my web page"

about :: Html ()
about = wrap "About" do
  div_ "About Me"

blog :: Html ()
blog = wrap "Blog Archive" do
  div_ "All the blogs (with some htmx?)"

blogPost :: (MonadIO m) => Text -> HtmlT m ()
blogPost p = do
  -- todo: proper escaping, sanitizing, etc for the file path
  file <- liftIO $ Data.Text.IO.readFile ("./data/" <> (Data.Text.unpack p) <> ".md")
  let postHtml = CMark.commonmarkToHtml [CMark.optSafe] file
  wrap p do
    div_ [class_ "flex justify-center items-center"] do
      article_ [class_ "mx-auto prose lg:prose-xl"] do
        toHtmlRaw postHtml

handler :: Server API
handler = pure rootPage
        :<|> pure about
        :<|> pure blog
        :<|> commuteHtmlT2 . blogPost
        :<|> serveDirectoryWebApp "static"
