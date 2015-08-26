module Handler.Blogpost where

import Import
import Yesod.Text.Markdown

renderSemantic :: Monad m => FormRender m a
renderSemantic aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
    let widget = [whamlet|
$newline never
\#{fragment}
$forall view <- views
    <div .ui .field :fvRequired view:.required :not $ fvRequired view:.optional>
        <label for=#{fvId view}>#{fvLabel view}
        $maybe tt <- fvTooltip view
            <div .tooltip>#{tt}
        ^{fvInput view}
        $maybe err <- fvErrors view
            <div .errors>#{err}
|]
    return (res, widget)

newCommentForm :: BlogpostId -> AForm Handler Comment
newCommentForm blogpostId = Comment <$>
  areq textField "Author" Nothing <*>
  pure blogpostId <*>
  areq markdownField "Content" Nothing

newPostForm :: AForm Handler Blogpost
newPostForm = Blogpost <$>
  areq textField "Title" Nothing <*>
  areq markdownField "Content"Nothing

getNewBlogPostR :: Handler Html
getNewBlogPostR = do
  (widget, enctype) <- generateFormPost $ renderSemantic newPostForm
  defaultLayout $ do
    $(widgetFile "blogpost/new_post")

getBlogpostR :: Handler Html
getBlogpostR = do
  blogposts <- runDB $ selectList [] [Desc BlogpostId]
  defaultLayout $ do
    $(widgetFile "blogpost/list_posts")

postBlogpostR :: Handler Html
postBlogpostR = do
  ((result, _), _) <- runFormPost $ renderDivs newPostForm
  case result of
    FormSuccess blogpost -> do
      blogpostId <- runDB $ insert blogpost
      setMessage (toHtml ("You just posted a new Blogpost. Gratz!" :: Text))
      redirect $ BlogpostDetailR blogpostId
    _ -> defaultLayout [whamlet|<p>ERROR|]


getBlogpostDetailR :: BlogpostId -> Handler Html
getBlogpostDetailR blogpostId = do
  blogpost <- runDB $ get404 blogpostId
  comments <- runDB $ selectList [CommentBlogpost ==. blogpostId] []
  (commentWidget, commentEnctype) <- generateFormPost $ renderSemantic (newCommentForm blogpostId)
  defaultLayout $(widgetFile "blogpost/post_detail")


postNewCommentR :: BlogpostId -> Handler Html
postNewCommentR blogpostId = do
  ((result, _), _) <- runFormPost $ renderDivs (newCommentForm blogpostId)
  case result of
    FormSuccess comment -> do
      _ <- runDB $ insert comment
      redirect $ BlogpostDetailR blogpostId
    _ -> do
      defaultLayout [whamlet|<p>ERROR|]
  
