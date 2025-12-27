{-# LANGUAGE OverloadedStrings #-}

module MdMore.Markdown
  ( renderMarkdown
  )
where

import Data.Char (isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import MdMore.ANSI (bg256, bold, dim, fg256, italic, underline)
import MdMore.Width (displayWidth)
import MdMore.Wrap (Token, plain, renderLine, styled, wrapLine, wrapWithPrefixes)

renderMarkdown :: Bool -> Int -> Text -> [Text]
renderMarkdown useColor width input =
  let blocks = parseBlocks (T.lines input)
      tokenLines = renderBlocks width blocks
   in map (renderLine useColor) tokenLines

data Block
  = BBlank
  | BRule
  | BHeading !Int !Text
  | BParagraph !Text
  | BListItem !Int !Text !Text
  | BQuote [Block]
  | BCode (Maybe Text) [Text]

parseBlocks :: [Text] -> [Block]
parseBlocks = go
  where
    go [] = []
    go (l : ls)
      | isBlank l = BBlank : go (dropWhile isBlank ls)
      | Just (fence, lang) <- fenceStart l =
          let (codeLines, rest) = span (not . fenceEnd fence) ls
              rest' = case rest of
                [] -> []
                (_end : more) -> more
           in BCode lang codeLines : go rest'
      | Just (lvl, h) <- heading l = BHeading lvl h : go ls
      | isRule l = BRule : go ls
      | isQuote l =
          let (qs, rest) = span isQuote (l : ls)
              innerLines = map stripQuote qs
           in BQuote (parseBlocks innerLines) : go rest
      | Just (indent, bullet, txt, rest) <- listItem (l : ls) =
          BListItem indent bullet txt : go rest
      | otherwise =
          let (ps, rest) = span isParaLine (l : ls)
              p = T.intercalate " " (map (T.strip . T.stripEnd) ps)
           in BParagraph p : go rest

isBlank :: Text -> Bool
isBlank = T.all isSpace

isParaLine :: Text -> Bool
isParaLine l =
  not (isBlank l)
    && fenceStart l == Nothing
    && heading l == Nothing
    && not (isRule l)
    && not (isQuote l)
    && listItemStart l == Nothing

heading :: Text -> Maybe (Int, Text)
heading t0 =
  let t = T.dropWhile isSpace t0
      (hashes, rest) = T.span (== '#') t
      lvl = T.length hashes
   in if lvl >= 1 && lvl <= 6 && (not . T.null) rest && T.head rest == ' '
        then Just (lvl, T.strip (T.drop 1 rest))
        else Nothing

isRule :: Text -> Bool
isRule t0 =
  let t = T.filter (/= ' ') (T.strip t0)
   in T.length t >= 3 &&
        (T.all (== '-') t || T.all (== '_') t || T.all (== '*') t)

fenceStart :: Text -> Maybe (Text, Maybe Text)
fenceStart t0 =
  let t = T.dropWhile isSpace t0
   in case () of
        _
          | Just rest <- T.stripPrefix "```" t -> Just ("```", lang rest)
          | Just rest <- T.stripPrefix "~~~" t -> Just ("~~~", lang rest)
          | otherwise -> Nothing
  where
    lang rest =
      let l = T.strip rest
       in if T.null l then Nothing else Just l

fenceEnd :: Text -> Text -> Bool
fenceEnd fence t0 =
  let t = T.dropWhile isSpace t0
   in fence `T.isPrefixOf` t

isQuote :: Text -> Bool
isQuote t0 =
  let t = T.dropWhile isSpace t0
   in ">" `T.isPrefixOf` t

stripQuote :: Text -> Text
stripQuote t0 =
  let t = T.dropWhile isSpace t0
   in case T.uncons t of
        Just ('>', rest) -> T.stripPrefix " " rest `orElse` rest
        _ -> t0
  where
    orElse (Just a) _ = a
    orElse Nothing b = b

listItemStart :: Text -> Maybe (Int, Text, Text)
listItemStart t0 =
  let (indentTxt, t1) = T.span (== ' ') t0
      indent = T.length indentTxt
      t = T.dropWhile (== ' ') t1
   in case T.uncons t of
        Just (c, rest)
          | c `elem` ['-', '*', '+'], Just rest' <- T.stripPrefix " " rest ->
              Just (indent, "• ", T.strip rest')
        _ ->
          let (digits, rest) = T.span isDigit t
           in if not (T.null digits) &&
                ("." `T.isPrefixOf` rest || ")" `T.isPrefixOf` rest)
                && (T.length rest >= 2 && T.index rest 1 == ' ')
                then
                  let bullet = digits <> T.take 2 rest
                      txt = T.strip (T.drop 2 rest)
                   in Just (indent, bullet, txt)
                else Nothing

listItem :: [Text] -> Maybe (Int, Text, Text, [Text])
listItem [] = Nothing
listItem (l : ls) =
  case listItemStart l of
    Nothing -> Nothing
    Just (indent, bullet, firstLine) ->
      let contIndent = indent + 2
          (cont, rest) = span (isContinuation contIndent) ls
          allLines = firstLine : map (T.drop contIndent) cont
          txt = T.intercalate " " (map (T.strip . T.stripEnd) allLines)
       in Just (indent, bullet, txt, rest)
  where
    isContinuation n t =
      (not . isBlank) t && T.length (T.takeWhile (== ' ') t) >= n

renderBlocks :: Int -> [Block] -> [[Token]]
renderBlocks width = concatMap (renderBlock width)

renderBlock :: Int -> Block -> [[Token]]
renderBlock width b =
  case b of
    BBlank -> [[plain ""]]
    BRule -> [[plain (T.replicate (max 1 width) "─")]]
    BHeading lvl t ->
      let style = headingStyle lvl
       in wrapLine width (inlineTokens style t)
    BParagraph t -> wrapLine width (inlineTokens "" t)
    BListItem indent bullet t ->
      let indentTxt = T.replicate indent " "
          bulletW = displayWidth bullet
          firstPrefix = [plain indentTxt, styled (fg256 214 <> bold) bullet]
          nextPrefix = [plain indentTxt, plain (T.replicate bulletW " ")]
       in wrapWithPrefixes width firstPrefix nextPrefix (inlineTokens "" t)
    BQuote inner ->
      let pfx = [styled (fg256 245 <> dim) "│ "]
          pfxW = 2
          innerLines = renderBlocks (max 10 (width - pfxW)) inner
       in map (pfx <>) innerLines
    BCode _lang ls ->
      let pfx = [styled (fg256 245 <> dim) "│ "]
          pfxW = 2
          maxW = max 10 (width - pfxW)
          codeStyle = bg256 235 <> fg256 81
       in concatMap (renderCodeLine pfx codeStyle maxW) ls

renderCodeLine :: [Token] -> Text -> Int -> Text -> [[Token]]
renderCodeLine pfx codeStyle maxW line =
  let chunks = hardWrap maxW line
   in if null chunks
        then [pfx]
        else map (\c -> pfx <> [styled codeStyle c]) chunks

hardWrap :: Int -> Text -> [Text]
hardWrap maxW t
  | T.null t = [""]
  | otherwise = go t
  where
    go rest
      | T.null rest = []
      | otherwise =
          let (a, b) = splitByWidth maxW rest
           in a : go b

splitByWidth :: Int -> Text -> (Text, Text)
splitByWidth maxW txt = go 0 "" txt
  where
    go w acc rest =
      case T.uncons rest of
        Nothing -> (acc, "")
        Just (c, cs) ->
          let cw = displayWidth (T.singleton c)
              cw' = if cw <= 0 then 1 else cw
           in if w + cw' > maxW
                then (acc, rest)
                else go (w + cw') (acc <> T.singleton c) cs

inlineTokens :: Text -> Text -> [Token]
inlineTokens baseStyle = go
  where
    go txt
      | T.null txt = []
      | Just rest <- T.stripPrefix "\\" txt =
          case T.uncons rest of
            Nothing -> [styled baseStyle "\\"]
            Just (c, more) -> styled baseStyle (T.singleton c) : go more
      | Just rest <- T.stripPrefix "`" txt =
          case T.breakOn "`" rest of
            (code, more)
              | Just more' <- T.stripPrefix "`" more ->
                  styled (baseStyle <> bg256 236 <> fg256 81) code : go more'
              | otherwise -> styled baseStyle "`" : go rest
      | Just rest <- T.stripPrefix "**" txt =
          case T.breakOn "**" rest of
            (inner, more)
              | Just more' <- T.stripPrefix "**" more ->
                  inlineTokens (baseStyle <> bold) inner <> go more'
              | otherwise -> styled baseStyle "**" : go rest
      | Just rest <- T.stripPrefix "__" txt =
          case T.breakOn "__" rest of
            (inner, more)
              | Just more' <- T.stripPrefix "__" more ->
                  inlineTokens (baseStyle <> bold) inner <> go more'
              | otherwise -> styled baseStyle "__" : go rest
      | Just rest <- T.stripPrefix "*" txt =
          case T.breakOn "*" rest of
            (inner, more)
              | Just more' <- T.stripPrefix "*" more ->
                  inlineTokens (baseStyle <> italic) inner <> go more'
              | otherwise -> styled baseStyle "*" : go rest
      | Just rest <- T.stripPrefix "_" txt =
          case T.breakOn "_" rest of
            (inner, more)
              | Just more' <- T.stripPrefix "_" more ->
                  inlineTokens (baseStyle <> italic) inner <> go more'
              | otherwise -> styled baseStyle "_" : go rest
      | Just rest <- T.stripPrefix "[" txt =
          case T.breakOn "]" rest of
            (label, more)
              | Just more1 <- T.stripPrefix "](" more ->
                  case T.breakOn ")" more1 of
                    (url, more2)
                      | Just more3 <- T.stripPrefix ")" more2 ->
                          inlineTokens (baseStyle <> underline <> fg256 33) label
                            <> urlSuffix url
                            <> go more3
                      | otherwise -> styled baseStyle "[" : go rest
              | otherwise -> styled baseStyle "[" : go rest
      | otherwise =
          let (chunk, rest) = T.break isSpecial txt
           in (if T.null chunk then [] else [styled baseStyle chunk]) <> go rest

    isSpecial c = c `elem` ['\\', '`', '*', '_', '[']

    urlSuffix url
      | T.null url = []
      | otherwise =
          [ styled (baseStyle <> dim <> fg256 244) " ("
          , styled (baseStyle <> dim <> fg256 244) url
          , styled (baseStyle <> dim <> fg256 244) ")"
          ]

headingStyle :: Int -> Text
headingStyle lvl =
  let c = case lvl of
        1 -> 220
        2 -> 214
        3 -> 81
        4 -> 75
        5 -> 110
        _ -> 245
   in bold <> fg256 c
