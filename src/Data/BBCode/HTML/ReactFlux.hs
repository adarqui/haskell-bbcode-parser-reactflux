{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.BBCode.HTML.ReactFlux (
    runBBCodeToHTML
  , runBBCodeToHTMLWith
  , bbcodeToHTML
  , codeToHTML
) where



import           Control.Monad.Trans.RWS
import           Data.Bimap              (Bimap)
import qualified Data.Bimap              as Bimap
import qualified Data.Map                as Map
import           Data.Monoid             ((<>))
import qualified Data.Text               as Text
import           Data.Text               (Text)
import           React.Flux
import           Text.Printf

import           Data.BBCode
import           Web.Media.Embed.ReactFlux
import           Web.Media.Embed



#ifdef __GHCJS__
import           Data.JSString           (JSString)
import qualified Data.JSString.Text      as JSS (textToJSString)
#else
type JSString = String
#endif



#ifdef __GHCJS__
textToJSString' :: Text -> JSString
textToJSString' = JSS.textToJSString
#else
textToJSString' :: Text -> String
textToJSString' = Text.unpack
#endif



type HTMLView_ = ReactElementM ViewEventHandler ()



runBBCodeToHTML :: [BBCode] -> HTMLView_
runBBCodeToHTML = runBBCodeToHTMLWith defaultParseReader



runBBCodeToHTMLWith :: ParseReader -> [BBCode] -> HTMLView_
runBBCodeToHTMLWith parse_reader codes =
  fst $ evalRWS (bbcodeToHTML codes) parse_reader defaultParseState



bbcodeToHTML :: [BBCode] -> ParseEff HTMLView_
bbcodeToHTML codes = go [] codes
  where
  go acc [] = pure $ mconcat $ reverse acc
  go acc (x:xs) = do
    html <- codeToHTML x
    go (html : acc) xs



codeToHTML :: BBCode -> ParseEff HTMLView_
codeToHTML tag = do
  case tag of
    Bold xs              -> strong_ ["className" $= "bbcode-bold"] <$> bbcodeToHTML xs
    Italic xs            -> em_ ["className" $= "bbcode-italic"] <$> bbcodeToHTML xs
    Underline xs         -> span_ ["className" $= "bbcode-underline", style [("text-decoration", "underline")]] <$> bbcodeToHTML xs
    Strike xs            -> del_ ["className" $= "bbcode-strike"] <$> bbcodeToHTML xs
    Font opts xs         -> runFont opts xs
    Size opts xs         -> runSize opts xs
    Color opts xs        -> runColor opts xs
    Center xs            -> p_ ["className" $= "bbcode-align-center", style [("text-align", "center")]] <$> bbcodeToHTML xs
    AlignLeft xs         -> p_ ["className" $= "bbcode-align-left", style [("text-align", "left")]] <$> bbcodeToHTML xs
    AlignRight xs        -> p_ ["className" $= "bbcode-align-right", style [("text-align", "right")]] <$> bbcodeToHTML xs
    Quote m_author m_avatar m_link m_date xs -> runQuote m_author m_avatar m_link m_date xs
    Link m_name url      -> runLink m_name url
    List list            -> pure mempty
    OrdList list         -> pure mempty
    Table table          -> pure mempty
    Pre text             -> pure $ pre_ ["className" $= "bbcode-pre"] $ elemText text
    Code _ code          -> pure $ pre_ ["className" $= "bbcode-code"] $ elemText code
    Move xs              -> pure mempty
    Text text            -> pure $ elemText text
    Emoticon emot        -> runEmoticon emot
    Image opts url       -> runImage opts url
    Youtube url          -> runYoutube url
    Vimeo url            -> pure mempty
    Facebook url         -> pure mempty
    Instagram url        -> runInstagram url
    Streamable url       -> pure mempty
    Imgur url            -> pure mempty
    HR                   -> pure $ hr_ mempty
    NL                   -> pure $ br_ mempty
    _                    -> pure $ p_ ["className" $= "bbcode-unknown"] $ elemText "unknown"



runSize :: SizeOpts -> [BBCode] -> ParseEff HTMLView_
runSize size_opts xs = do
  r <- ask
  let code = (case Map.lookup "size" (trfm r) of
                Nothing   -> Size size_opts xs
                Just trfm -> trfm (Size size_opts xs))
  go code
  where
  go (Size SizeOpts{..} xs) = do
    html <- bbcodeToHTML xs
    let size = (case sizeValue of
                  Just (SizePx n) -> (Text.pack $ show n) <> "px"
                  Just (SizePt n) -> (Text.pack $ show n) <> "pt"
                  Just (SizeEm n) -> (Text.pack $ show n) <> "em"
                  _               -> "12pt")
    pure $ span_ ["className" $= "bbcode-size"] html
  go _ = pure mempty



runFont :: FontOpts -> [BBCode] -> ParseEff HTMLView_
runFont font_opts xs = do
  r <- ask
  let code = (case Map.lookup "font" (trfm r) of
                Nothing   -> Font font_opts xs
                Just trfm -> trfm (Font font_opts xs))
  go code
  where
  go (Font FontOpts{..} xs) = do
    html <- bbcodeToHTML xs
    let font_family = (case fontFamily of
                         Nothing  -> "sans-serif"
                         Just fam -> fam)
    pure $ span_ ["className" $= "bbcode-font", style [("font-family", textToJSString' font_family)]] html
  go _ = pure mempty



runColor :: ColorOpts -> [BBCode] -> ParseEff HTMLView_
runColor color_opts xs = do
  r <- ask
  let code = (case Map.lookup "color" (trfm r) of
                Nothing   -> Color color_opts xs
                Just trfm -> trfm (Color color_opts xs))
  go code
  where
  go (Color ColorOpts{..} xs) = do
    html <- bbcodeToHTML xs
    let color' = (case colorValue of
                    Just (ColorName name) -> name
                    Just (ColorHex hex)   -> hex
                    _                     -> "black")
    pure $ span_ ["className" $= "bbcode-color", style [("color", textToJSString' color')]] html
  go _ = pure mempty



runImage :: ImageOpts -> MediaURL -> ParseEff HTMLView_
runImage image_opts url = do
  r <- ask
  let code = (case Map.lookup "img" (trfm r) of
                Nothing   -> Image image_opts url
                Just trfm -> trfm (Image image_opts url))
  go code
  where
  go :: BBCode -> ParseEff HTMLView_
  go (Image ImageOpts{..} url) = do
    let
      height_props = case imageHeight of
                       Nothing               -> []
                       Just (ImagePx n)      -> [(textToJSString' "height", ((textToJSString' $ Text.pack $ show n) <> "px"))]
                       Just (ImagePercent n) -> [(textToJSString' "height", ((textToJSString' $ Text.pack $ show n) <> "pct"))]
      width_props = case imageWidth of
                      Nothing               -> []
                      Just (ImagePx n)      -> [(textToJSString' "width", ((textToJSString' $ Text.pack $ show n) <> "px"))]
                      Just (ImagePercent n) -> [(textToJSString' "width", ((textToJSString' $ Text.pack $ show n) <> "pct"))]
      props = height_props <> width_props
    pure $ img_ ["className" $= "bbcode-img", style props, "src" $= textToJSString' url] mempty
  go _ = pure mempty



runYoutube :: Text -> ParseEff HTMLView_
runYoutube url = do
  pure $ simpleRenderYoutube url (defaultIFrame { iframeHeight = Just 405, iframeWidth = Just 720 })



runInstagram :: Text -> ParseEff HTMLView_
runInstagram url = do
  pure $ simpleRenderInstagram url (defaultIFrame { iframeHeight = Just 710
                                                  , iframeWidth = Just 612
                                                  , iframeScrolling = Just ScrollingNo
                                                  })



runEmoticon :: Text -> ParseEff HTMLView_
runEmoticon emot_key = do
  emoticons <- asks emoticons
  case emoticons of

    -- This shouldn't happen .. getting an emoticon without having an emoticon Bimap.
    Nothing -> pure $ elemText emot_key

    -- Our beautiful emoticon
    Just (emoticons_map, emoticons_route) ->
      -- emoticons are .gif's!!
      pure $ img_ ["className" $= "bbcode-emoticon", "src" $= textToJSString' (emoticons_route <> "/" <> emot_key <> ".gif")] mempty



runQuote :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> [BBCode] -> ParseEff HTMLView_
runQuote m_author m_avatar m_link m_date xs = do

  ParseReader{..} <- ask
  let
    link = case (m_link, linkResource) of
                (Just link, Just trfm) -> Just $ trfm link
                _                      -> Nothing
    m_title =
      case (m_author, m_date) of
        (Just author, Just date) -> Just $ Text.pack $ printf "Quote from %s at %s" author date
        _                        -> Nothing

  bb <- bbcodeToHTML xs
  pure $ do

    -- optional title, or clickable title
    --
    cldiv_ "bbcode-quote" $ do

      -- If author exists and we have an avatar url, embed a small avatar for sexyness
      case (m_author, m_avatar) of
        (Just author, Just avatar) -> img_ ["src" $= textToJSString' ("//www.gravatar.com/avatar/" <> avatar <> "?d=identicon&r=pg&s=20"), "alt" $= textToJSString' author] mempty
        _                          -> mempty

      case (link, m_title) of
        (Nothing, Just title)   -> p_ ["className" $= "bbcode-quote-title"] $ elemText title
        (Just link, Just title) -> a_ ["className" $= "bbcode-quote-title", "href" @= link, "target" $= "_blank"] $ elemText title
        _                       -> mempty
      blockquote_ ["className" $= "bbcode-quote-body"] bb



runLink :: Maybe Text -> Text -> ParseEff HTMLView_
runLink (Just name) url = pure $ a_ ["className" $= "bbcode-link", "href" @= url, "target" $= "_blank"] $ elemText name
runLink Nothing url     = pure $ a_ ["className" $= "bbcode-link", "href" @= url, "target" $= "_blank"] $ elemText url
