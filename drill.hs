#!/usr/bin/env stack
{- stack script
   --snapshot nightly-2026-07-08
   --package ansi-terminal
   --package directory
   --package filepath
   --package process
   --package random
-}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}

-- Number drill over pre-generated pronunciation mp3s. Two modes:
--   speak  - show a random number 0..LIMIT-1, wait for Enter, then play its
--            mp3 so you can compare your pronunciation.
--   listen - play a random number's mp3, ask you to type the number, then
--            show the verdict on the answer line (green tick, or red cross
--            with the correct answer).
-- Loops until Ctrl+C. Plays mp3s from ./audio (or falls back to the audio
-- directory next to the script itself).

module Main (main) where

import Control.Exception (AsyncException (UserInterrupt), catch, throwIO)
import Control.Monad (forever, when)
import Data.Char (isDigit)
import System.Console.ANSI
    ( Color (Green, Red)
    , ColorIntensity (Dull)
    , ConsoleLayer (Foreground)
    , SGR (Reset, SetColor)
    , cursorUp
    , setCursorColumn
    , setSGR
    )
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO
    ( BufferMode (NoBuffering)
    , IOMode (ReadMode, WriteMode)
    , hPutStrLn
    , hSetBuffering
    , hSetEncoding
    , isEOF
    , stderr
    , stdout
    , utf8
    , withFile
    )
import System.Process
    ( CreateProcess (std_err, std_in, std_out)
    , StdStream (UseHandle)
    , createProcess
    , proc
    , waitForProcess
    )
import System.Random (randomRIO)
import Text.Read (readMaybe)

data Mode = Speak | Listen

main :: IO ()
main = do
    (mode, limit) <- parseArgs =<< getArgs
    dir <- findMp3Dir
    hSetBuffering stdout NoBuffering
    hSetEncoding stdout utf8
    forever (oneRound dir mode limit) `catch` onInterrupt

-- Ctrl+C: newline, then clean exit (like the bash trap).
onInterrupt :: AsyncException -> IO ()
onInterrupt UserInterrupt = putStrLn "" >> exitSuccess
onInterrupt e = throwIO e

parseArgs :: [String] -> IO (Mode, Int)
parseArgs [modeStr, limitStr]
    | Just mode <- parseMode modeStr
    , Just limit <- readMaybe limitStr
    , limit >= 1 =
        if limit > 10000
            then hPutStrLn stderr "number too high" >> exitFailure
            else pure (mode, limit)
parseArgs _ = usage

parseMode :: String -> Maybe Mode
parseMode "speak" = Just Speak
parseMode "listen" = Just Listen
parseMode _ = Nothing

usage :: IO a
usage = do
    hPutStrLn stderr "Usage: drill.hs speak|listen LIMIT"
    exitFailure

-- Play files from ./audio; if it doesn't exist, fall back to the audio
-- directory next to the script itself.
findMp3Dir :: IO FilePath
findMp3Dir = do
    hereExists <- doesDirectoryExist "audio"
    pure $ if hereExists then "audio" else takeDirectory __FILE__ </> "audio"

oneRound :: FilePath -> Mode -> Int -> IO ()
oneRound dir mode limit = do
    n <- randomRIO (0, limit - 1)
    case mode of
        Speak -> do
            print n
            putStr "Enter to continue"
            _ <- getLineOrExit
            play (dir </> show n <.> "mp3")
        Listen -> do
            -- Prompt before playing so typing during playback doesn't garble the display
            putStr prompt
            play (dir </> show n <.> "mp3")
            answer <- getLineOrExit
            -- Enter moved the cursor to a new line; jump back up to just after the
            -- typed answer so the verdict appears on the same line.
            cursorUp 1
            setCursorColumn (length prompt + length answer)
            if isCorrect n answer
                then mark Green "✓" >> putStrLn ""
                else mark Red "✗" >> putStrLn (" (correct was " <> show n <> ")")
  where
    prompt = "Type the number: "

isCorrect :: Int -> String -> Bool
isCorrect n answer =
    not (null answer)
        && all isDigit answer
        && readMaybe answer == Just (toInteger n)

mark :: Color -> String -> IO ()
mark color symbol = do
    putStr " "
    setSGR [SetColor Foreground Dull color]
    putStr symbol
    setSGR [Reset]

-- Exit cleanly when stdin is exhausted, like the bash `read || exit 0`.
getLineOrExit :: IO String
getLineOrExit = do
    eof <- isEOF
    when eof exitSuccess
    getLine

-- stdin redirected so mplayer's console controls don't eat typed-ahead answers
play :: FilePath -> IO ()
play file =
    withFile "/dev/null" ReadMode $ \devIn ->
        withFile "/dev/null" WriteMode $ \devOut ->
            withFile "/dev/null" WriteMode $ \devErr -> do
                (_, _, _, ph) <-
                    createProcess
                        (proc "mplayer" ["-really-quiet", file])
                            { std_in = UseHandle devIn
                            , std_out = UseHandle devOut
                            , std_err = UseHandle devErr
                            }
                _ <- waitForProcess ph
                pure ()
