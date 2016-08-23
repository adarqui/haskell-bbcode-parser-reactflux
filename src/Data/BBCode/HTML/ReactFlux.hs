{-# LANGUAGE OverloadedStrings #-}

module Data.BBCode.HTML.ReactFlux (
    runBBCodeToHTML
  , runBBCodeToHTMLWith
  , bbcodeToHTML
  , codeToHTML
) where



import           Control.Monad.Trans.RWS
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
    Size opts xs         -> pure mempty
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
