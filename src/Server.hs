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
  body_ [class_ "bg-white"] body

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
  header_ [class_ "bg-soft-blue border-b-2 border-black"] do
    nav_ [class_ "py-6 mx-12 max-w-sm flex flex-row gap-x-4 justify-left"] do
      a_ [href_ "/", class_ "text-2xl"] "mutantmell.net" -- "text-yellow-950"
      a_ [href_ "/about", class_ "text-brass text-xl pt-1"] "About"
      a_ [href_ "/blog", class_ "text-brass text-xl pt-1"] "Blog"
  div_ [class_ "my-12 mx-12 mx-auto max-w-prose"] do
    h1_ [class_ "text-2xl justify-left border-b border-black"] "mutantmell's spot on the internet"
    div_ [class_ "mx-auto gap-x-4"] do
      p_ [] $ toHtml $ Data.Text.unwords
        [ "Lorem ipsum odor amet, consectetuer adipiscing elit."
        , "Lacus pulvinar sodales himenaeos ad at mi."
        , "Netus quis congue orci; dictum pretium nec."
        , "Hac parturient in rhoncus finibus penatibus parturient habitasse."
        , "Quam commodo himenaeos massa duis maecenas pellentesque integer purus."
        , "Penatibus proin adipiscing suspendisse ac vivamus."
        ]
      p_ [] $ toHtml $ Data.Text.unlines
        [ "Quam nibh consequat feugiat sit ridiculus viverra."
        , "Sociosqu odio ante lectus odio rutrum platea posuere ante."
        , "Nec mattis bibendum pulvinar inceptos primis magna ex litora."
        , "Ante euismod a lectus lacinia quam et facilisis accumsan quisque?"
        , "Nisl enim ultrices volutpat iaculis torquent integer tortor risus consectetur."
        , "Litora elementum erat efficitur pharetra pellentesque scelerisque curae."
        , "Vehicula suscipit maecenas, interdum elementum quam dapibus."
        , "Auctor rhoncus rhoncus nascetur urna consectetur commodo tristique montes."
        , "Sodales himenaeos etiam dolor quis sem."
        , "Ultrices ridiculus vehicula in fames ac duis ex libero augue."
        ]
      p_ [] $ toHtml $ Data.Text.unlines
        [ "Elit viverra penatibus placerat hac a ridiculus."
        , "Nostra mi fusce tellus enim dictum ridiculus."
        , "Etiam porta sapien hendrerit porttitor, tempus nascetur integer fermentum."
        , "Libero ut lacus; in natoque inceptos nam vehicula."
        , "Sit montes hac curae ornare blandit purus."
        , "Mattis sagittis finibus phasellus scelerisque lobortis lorem mus turpis augue."
        , "Dis ornare torquent posuere praesent tortor senectus leo dolor."
        ]
      p_ [] $ toHtml $ Data.Text.unlines
        [ "Aptent mollis ante felis parturient lacus nibh enim in lectus."
        , "Auctor varius velit pharetra auctor turpis hendrerit justo purus."
        , "Parturient himenaeos nisi lacinia, eleifend facilisi malesuada sem?"
        , "Accumsan at dui potenti tortor magnis, pretium in."
        , "Convallis hendrerit nulla sed libero fringilla mollis."
        , "Eu curabitur rutrum efficitur accumsan; ornare est."
        , "Vehicula facilisi ut class placerat id."
        , "Rhoncus eleifend netus lectus ut magna semper maximus."
        ]
      p_ [] $ toHtml $ Data.Text.unlines
        [ "Donec lorem montes natoque donec elit nunc rhoncus rhoncus."
        , "Lectus felis sollicitudin magna faucibus diam tristique torquent commodo dictumst."
        , "Facilisis posuere phasellus magna class amet curae."
        , "Facilisis mus non nec fames nibh ligula arcu orci consectetur?"
        , "Montes tincidunt aliquet eu augue magna egestas neque."
        , "Sem etiam dapibus iaculis pharetra nisi praesent phasellus montes."
        , "In molestie sem orci parturient nostra congue fermentum ornare curabitur."
        , "Odio senectus suscipit venenatis semper odio est odio."
        , "Odio orci nisi conubia fusce fusce pulvinar ornare ut."
        ]

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
