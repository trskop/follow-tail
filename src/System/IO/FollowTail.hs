{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Haskell implementation of "tail -f" as a library.
-- Copyright:    (c) 2017-2018 Peter Trško
-- License:      BSD3
-- Maintainer:   Peter Trško <peter.trsko@gmail.com>
-- Stability:    Provisional
-- Portability:  GHC specific language extensions; Linux (Inotify).
--
-- Haskell implementation of @\"tail -f\"@ as a library.
module System.IO.FollowTail
    (
      whileFollowingFile
    , NumberOfLines
    , AreInitialData
    )
  where

import Control.Exception (bracket)
import Data.Function (($), const)
import System.IO (FilePath, IO)

import qualified Data.ByteString.Lazy as Lazy (ByteString)
import System.INotify (withINotify)

import System.IO.FollowTail.Internal
    ( AreInitialData
    , NumberOfLines
    , followFile
    , stopFollowingFile
    )

whileFollowingFile
    :: NumberOfLines
    -- ^ Number of lines to read from the end of file before waiting for it
    -- to change.
    -> FilePath
    -- ^ File to watch.
    -> (AreInitialData -> Lazy.ByteString -> IO ())
    -- ^ Action invoked whenever there are new data available.
    -> IO a
    -- ^ Stop following file when this action terminates.
    -> IO a
whileFollowingFile numberOfLines file onData action = withINotify $ \inotify ->
    followFile inotify numberOfLines file onData `bracket` stopFollowingFile
        $ const action
