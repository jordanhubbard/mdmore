{-# LANGUAGE OverloadedStrings #-}

module MdMore.Wrap
  ( Token(..)
  , plain
  , styled
  , renderLine
  , wrapLine
  , wrapLineKeepLeading
  , wrapWithPrefixes
  , wrapWithPrefixesKeepLeading
  )
where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import MdMore.ANSI (reset)
import MdMore.Width (displayWidth)

data Token = Token
  { tokSgr :: !Text
  , tokText :: !Text
  }
  deriving (Eq, Show)

plain :: Text -> Token
plain = Token ""

styled :: Text -> Text -> Token
styled = Token

renderLine :: Bool -> [Token] -> Text
renderLine useColor = foldMap renderTok . mergeAdjacent
  where
    renderTok (Token sgr txt)
      | T.null txt = ""
      | not useColor || T.null sgr = txt
      | otherwise = sgr <> txt <> reset

    mergeAdjacent = foldr step []
    step t [] = [t]
    step (Token s1 t1) (Token s2 t2 : rest)
      | s1 == s2 = Token s1 (t1 <> t2) : rest
      | otherwise = Token s1 t1 : Token s2 t2 : rest

wrapLine :: Int -> [Token] -> [[Token]]
wrapLine width = wrapWithPrefixes width [] []

wrapLineKeepLeading :: Int -> [Token] -> [[Token]]
wrapLineKeepLeading width = wrapWithPrefixesKeepLeading width [] []

wrapWithPrefixes :: Int -> [Token] -> [Token] -> [Token] -> [[Token]]
wrapWithPrefixes = wrapWithPrefixesInternal True

wrapWithPrefixesKeepLeading :: Int -> [Token] -> [Token] -> [Token] -> [[Token]]
wrapWithPrefixesKeepLeading = wrapWithPrefixesInternal False

wrapWithPrefixesInternal :: Bool -> Int -> [Token] -> [Token] -> [Token] -> [[Token]]
wrapWithPrefixesInternal trimLeading width firstPrefix nextPrefix content0 = go True content
  where
    w = max 10 width
    content = concatMap splitSpaces content0

    go isFirst toks =
      let prefix = if isFirst then firstPrefix else nextPrefix
          maxW = max 1 (w - tokensWidth prefix)
          (line, rest) = takeLine trimLeading maxW toks
          fullLine = prefix <> line
       in if null rest
            then [fullLine]
            else fullLine : go False rest

tokensWidth :: [Token] -> Int
tokensWidth = sum . map (displayWidth . tokText)

splitSpaces :: Token -> [Token]
splitSpaces (Token sgr t)
  | T.null t = []
  | otherwise = map (Token sgr) (splitRuns t)
  where
    splitRuns txt =
      case T.uncons txt of
        Nothing -> []
        Just (c0, _) ->
          let isSp = isSpace c0
              (chunk, rest) = T.span (\c -> isSpace c == isSp) txt
           in chunk : splitRuns rest

takeLine :: Bool -> Int -> [Token] -> ([Token], [Token])
takeLine trimLeading maxW toks0 = go 0 [] Nothing toks
  where
    toks = (if trimLeading then dropWhile isSpaceTok else id) toks0

    go _ accRev _ [] = (trimTrailingSpaces (reverse accRev), [])
    go cur accRev lastBreak (t@(Token sgr txt) : rest)
      | T.null txt = go cur accRev lastBreak rest
      | cur + tw <= maxW =
          let lastBreak' = if isSpaceTok t then Just (accRev, rest) else lastBreak
           in go (cur + tw) (t : accRev) lastBreak' rest
      | otherwise =
          case lastBreak of
            Just (accAtBreak, restAtBreak) ->
              ( trimTrailingSpaces (reverse accAtBreak)
              , dropWhile isSpaceTok restAtBreak
              )
            Nothing
              | null accRev ->
                  let (a, b) = splitTextByWidth maxW txt
                   in if T.null a
                        then ([], Token sgr txt : rest)
                        else
                          ( [Token sgr a]
                          , (if T.null b then rest else Token sgr b : rest)
                          )
              | otherwise -> (trimTrailingSpaces (reverse accRev), Token sgr txt : rest)
      where
        tw = displayWidth txt

isSpaceTok :: Token -> Bool
isSpaceTok (Token _ t) = T.all isSpace t

trimTrailingSpaces :: [Token] -> [Token]
trimTrailingSpaces = reverse . dropWhile isSpaceTok . reverse

splitTextByWidth :: Int -> Text -> (Text, Text)
splitTextByWidth maxW txt = go 0 "" txt
  where
    go w acc rest =
      case T.uncons rest of
        Nothing -> (acc, "")
        Just (c, cs) ->
          let cw = charWidth c
           in if w + cw > maxW
                then (acc, rest)
                else go (w + cw) (acc <> T.singleton c) cs

    charWidth c =
      let cw = displayWidth (T.singleton c)
       in if cw <= 0 then 1 else cw
