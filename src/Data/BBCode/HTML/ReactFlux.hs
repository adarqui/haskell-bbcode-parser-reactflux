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
import qualified Data.Map                as Map
import           Data.Monoid             ((<>))
import qualified Data.Text               as Text
import           Data.Text               (Text)
import           React.Flux

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
    Bold xs              -> strong_ <$> bbcodeToHTML xs
    Italic xs            -> em_ <$> bbcodeToHTML xs
    Underline xs         -> span_ [style [("text-decoration", "underline")]] <$> bbcodeToHTML xs
    Strike xs            -> del_ <$> bbcodeToHTML xs
    Font opts xs         -> runFont opts xs
    Size opts xs         -> runSize opts xs
    Color opts xs        -> runColor opts xs
    Center xs            -> p_ [style [("text-align", "center")]] <$> bbcodeToHTML xs
    AlignLeft xs         -> p_ [style [("text-align", "left")]] <$> bbcodeToHTML xs
    AlignRight xs        -> p_ [style [("text-align", "right")]] <$> bbcodeToHTML xs
    Quote author xs      -> blockquote_ <$> bbcodeToHTML xs
    Link (Just name) url -> pure $ a_ ["href" @= url, "target" $= "_blank"] $ elemText name
    Link Nothing url     -> pure $ a_ ["href" @= url, "target" $= "_blank"] $ elemText url
    List list            -> pure mempty
    OrdList list         -> pure mempty
    Table table          -> pure mempty
    Pre text             -> pure $ pre_ $ elemText text
    Code _ code          -> pure $ pre_ $ elemText code
    Move xs              -> pure mempty
    Text text            -> pure $ elemText text
    Image opts url       -> runImage opts url
    Youtube url          -> runYoutube url
    Vimeo url            -> pure mempty
    Facebook url         -> pure mempty
    Instagram url        -> pure mempty
    Streamable url       -> pure mempty
    Imgur url            -> pure mempty
    HR                   -> pure $ hr_ mempty
    NL                   -> pure $ br_ mempty
    _                    -> pure $ p_ $ elemText "unknown"



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
    pure $ span_ [] html
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
    pure $ span_ [style [("font-family", textToJSString' font_family)]] html
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
    pure $ span_ [style [("color", textToJSString' color')]] html
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
    pure $ img_ [style props, "src" $= textToJSString' url] mempty
  go _ = pure mempty



runYoutube :: Text -> ParseEff HTMLView_
runYoutube url = do
  pure $ simpleRenderYoutube url (defaultIFrame { iframeHeight = Just 405, iframeWidth = Just 720 })
