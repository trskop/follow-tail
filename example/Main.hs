{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Main
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; Linux (Inotify).
--
-- TODO: Module description.
module Main
--  (
--  )
  where

import Control.Monad ((>>=), forever)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Data.Function (($), const)
import System.Environment (getArgs)
import System.IO (IO)

import qualified Data.ByteString.Lazy as Lazy.ByteString (putStr)
import System.IO.FollowTail (whileFollowingFile)


main :: IO ()
main = do
    fileName : _ <- getArgs

    chan <- newChan
    let writeData = const $ writeChan chan
        readData = readChan chan

    whileFollowingFile 10 fileName writeData
        $ forever (readData >>= Lazy.ByteString.putStr)
