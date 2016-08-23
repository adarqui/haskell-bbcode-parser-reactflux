module Data.BBCode.HTML.ReactFlux (
  runBBCodeToHTML
) where



import React.Flux
import Control.Monad.Trans.RWS

import Data.BBCode



type HTMLView_ = ReactElementM ViewEventHandler ()



runBBCodeToHTML :: [BBCode] -> HTMLView_
runBBCodeToHTML = runBBCodeToHTMLWith defaultParseReader



runBBCodeToHTMLWith :: ParseReader -> [BBCode] -> HTMLView_
runBBCodeToHTMLWith parse_reader codes =
  fst $ evalRWS (bbcodeToHTML codes) parse_reader defaultParseState



bbcodeToHTML :: [BBCode] -> ParseEff HTMLView_
bbcodeToHTML codes = go [] codes
  where
  go acc xs = pure mempty
