{-# LANGUAGE OverloadedStrings #-}

module MdMore.Pager
  ( main
  )
where

import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as T
import GHC.IO.Encoding (utf8)
import MdMore.ANSI (reset, reverseVideo)
import MdMore.Markdown (renderMarkdown)
import System.Console.ANSI (clearLine, setCursorColumn)
import qualified System.Console.Terminal.Size as TermSize
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO
  ( BufferMode (NoBuffering)
  , hFlush
  , hGetBuffering
  , hGetEcho
  , hSetBuffering
  , hSetEcho
  , hSetEncoding
  , stdin
  , stdout
  )
import System.Posix.IO (stdInput, stdOutput)
import System.Posix.Terminal (queryTerminal)

main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  args <- getArgs
  (_srcName, input) <-
    case args of
      ["-h"] -> help
      ["--help"] -> help
      [] -> do
        t <- T.getContents
        pure ("<stdin>", t)
      [path] -> do
        t <- T.readFile path
        pure (path, t)
      _ -> dieUsage

  (w, h) <- terminalWH
  ttyIn <- queryTerminal stdInput
  ttyOut <- queryTerminal stdOutput
  let usePager = ttyIn && ttyOut
      useColor = ttyOut
      linesOut = renderMarkdown useColor w input

  if not usePager || h <= 1
    then mapM_ T.putStrLn linesOut
    else page (w, h) linesOut

help :: IO a
help = do
  T.putStrLn "mdmore - a best-effort markdown pager (like unix more)"
  T.putStrLn ""
  T.putStrLn "Usage:"
  T.putStrLn "  mdmore [FILE]"
  T.putStrLn ""
  T.putStrLn "If FILE is omitted, mdmore reads from stdin."
  T.putStrLn ""
  T.putStrLn "Keys:"
  T.putStrLn "  Space  next page"
  T.putStrLn "  Enter  next line"
  T.putStrLn "  b      back one page"
  T.putStrLn "  q      quit"
  T.putStrLn ""
  T.putStrLn "Rendering (best-effort): headings, lists, blockquotes, code fences,"
  T.putStrLn "inline bold/italic/code, and links. Colors/styling are enabled when"
  T.putStrLn "stdout is a TTY."
  exitSuccess

dieUsage :: IO a
dieUsage = do
  T.putStrLn "Usage: mdmore [FILE]"
  T.putStrLn "Try: mdmore --help"
  exitFailure

terminalWH :: IO (Int, Int)
terminalWH = do
  mw <- TermSize.size
  pure $
    case mw of
      Nothing -> (80, 24)
      Just win -> (TermSize.width win, TermSize.height win)

page :: (Int, Int) -> [Text] -> IO ()
page (_w, h) ls = withRawInput $ loop 0
  where
    total = length ls
    pageLines = max 1 (h - 1)

    loop i
      | i >= total = pure ()
      | otherwise = do
          let chunk = take pageLines (drop i ls)
          mapM_ T.putStrLn chunk

          let i' = i + length chunk
          if i' >= total
            then pure ()
            else do
              prompt i'
              c <- getChar
              clearPrompt
              case c of
                'q' -> pure ()
                'Q' -> pure ()
                ' ' -> loop i'
                '\n' -> loop (i + 1)
                '\r' -> loop (i + 1)
                'b' -> loop (max 0 (i - pageLines))
                _ -> loop i

    prompt i = do
      let pct :: Int
          pct = if total == 0 then 100 else (i * 100) `div` total
      T.putStr $ reverseVideo <> "--More--(" <> toText pct <> "%)" <> reset
      hFlush stdout

    clearPrompt = do
      clearLine
      setCursorColumn 0
      hFlush stdout

toText :: Int -> Text
toText = Text.pack . show

withRawInput :: IO a -> IO a
withRawInput action = bracket save restore (const action)
  where
    save = do
      echo <- hGetEcho stdin
      buf <- hGetBuffering stdin
      hSetEcho stdin False
      hSetBuffering stdin NoBuffering
      pure (echo, buf)

    restore (echo, buf) = do
      hSetEcho stdin echo
      hSetBuffering stdin buf
