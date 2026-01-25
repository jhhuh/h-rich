{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HRich.Prompt
Description : Interactive command-line prompts.
Copyright   : (c) Ji-Haeng Huh, 2025
License     : BSD-3-Clause
Maintainer  : jhhuh.note@gmail.com

This module provides functions to solicit input from the user, including support
for default values, yes/no confirmation, and password input (masked).
-}
module HRich.Prompt
    ( ask
    , confirm
    , askSecret
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified HRich.Console as Console
import qualified HRich.Text as HText
import System.IO (hFlush, stdout, stdin, hSetEcho, hGetEcho)
import Control.Exception (bracket)

-- | Ask the user for input with an optional default value.
ask :: T.Text       -- ^ The prompt message (rich markup supported)
    -> Maybe T.Text -- ^ Optional default value
    -> IO T.Text    -- ^ The user's input
ask msg Nothing = do
    renderPrompt msg Nothing
    line <- TIO.getLine
    return $ T.strip line
ask msg (Just def) = do
    renderPrompt msg (Just def)
    line <- TIO.getLine
    let trimmed = T.strip line
    if T.null trimmed then return def else return trimmed

-- | Ask a Yes/No question, returning True for Yes.
confirm :: T.Text -> IO Bool
confirm msg = do
    renderPrompt (msg `T.append` " [y/N]") Nothing
    line <- TIO.getLine
    let lower = T.toLower (T.strip line)
    return $ lower == "y" || lower == "yes"

-- | Ask for secret input (like a password), masking the input characters.
askSecret :: T.Text -> IO T.Text
askSecret msg = do
    renderPrompt msg Nothing
    bracket
        (do
            old <- hGetEcho stdin
            hSetEcho stdin False
            return old)
        (\old -> hSetEcho stdin old)
        (\_ -> do
            line <- TIO.getLine
            TIO.putStrLn "" -- Newline after input since echo was off
            return (T.strip line))

renderPrompt :: T.Text -> Maybe T.Text -> IO ()
renderPrompt msg def = do
    console <- Console.defaultConsole
    let promptText = case def of
            Nothing -> msg `T.append` ": "
            Just d  -> msg `T.append` " [" `T.append` d `T.append` "]: "
    Console.printMarkup console promptText
    hFlush stdout
