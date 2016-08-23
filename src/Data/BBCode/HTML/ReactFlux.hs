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
import           React.Flux

import           Data.BBCode



type HTMLView_ = ReactElementM ViewEventHandler ()



runBBCodeToHTML :: [BBCode] -> HTMLView_
runBBCodeToHTML = runBBCodeToHTMLWith defaultParseReader



runBBCodeToHTMLWith :: ParseReader -> [BBCode] -> HTMLView_
runBBCodeToHTMLWith parse_reader codes =
  fst $ evalRWS (bbcodeToHTML codes) parse_reader defaultParseState



bbcodeToHTML :: [BBCode] -> ParseEff HTMLView_
bbcodeToHTML codes = go [] codes
  where
  go acc []     = pure mempty
  go acc (x:xs) = do
    html <- codeToHTML x
    go (html : acc) xs



codeToHTML :: BBCode -> ParseEff HTMLView_
codeToHTML tag = do
  case tag of
    Bold xs              -> strong_ <$> bbcodeToHTML xs
    Italic xs            -> em_ <$> bbcodeToHTML xs
    Underline xs         -> span_ [] <$> bbcodeToHTML xs -- style underline
    Strike xs            -> del_ <$> bbcodeToHTML xs
    Font opts xs         -> pure mempty
    Size opts xs         -> runSize opts xs
    Color opts xs        -> pure mempty
    Center xs            -> p_ [] <$> bbcodeToHTML xs -- align center
    AlignLeft xs         -> pure mempty
    AlignRight xs        -> pure mempty
    Quote author xs      -> pure mempty
    Link (Just name) url -> pure mempty
    Link Nothing url     -> pure mempty
    List list            -> pure mempty
    OrdList list         -> pure mempty
    Table table          -> pure mempty
    Pre text             -> pure $ pre_ $ elemText text
    Code _ code          -> pure $ pre_ $ elemText code
    Move xs              -> pure mempty
    Text text            -> pure $ elemText text
    Image opts url       -> pure mempty
    Youtube url          -> pure mempty
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
                Nothing -> Size size_opts xs
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




