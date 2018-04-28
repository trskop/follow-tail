{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
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
    -- * Follow File Changes
      whileFollowingFile
    , NumberOfLines
    , AreInitialData

    -- * Low-level Functions
    , followFile
    , stopFollowingFile

    -- * Very Low-level Functions
    , FileOffset
    , readFileTail
    , fileTailPtr
    , hGetDelta
    )
  where

import Prelude ((+), (-), fromIntegral)

import Control.Applicative ((<*>), pure)
import Control.Exception (finally, bracket)
import Control.Monad ((>>=))
import Data.Bool (Bool(False, True), (&&), otherwise)
import Data.Eq ((==))
import Data.Function (($), (.), const)
import Data.Functor ((<$>))
import Data.IORef (atomicWriteIORef, newIORef, readIORef)
import Data.Int (Int)
import Data.Maybe (Maybe(Nothing))
import Data.Monoid (mempty)
import Data.Ord ((<=))
import Data.String (fromString)
import Data.Word (Word, Word8)
import Foreign (Ptr, castPtr, peekByteOff, plusPtr)
import System.IO
    ( FilePath
    , Handle
    , IO
    , IOMode(ReadMode)
    , SeekMode(AbsoluteSeek)
    , hClose
    , hFileSize
    , hSeek
    , openBinaryFile
    )

import qualified Data.ByteString.Internal as Internal (create, memcpy)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy as Lazy.ByteString (fromChunks, hGet)
import qualified Data.ByteString.Lazy.Internal as Internal (defaultChunkSize)
import System.INotify (EventVariety(Modify), INotify, withINotify)
import qualified System.INotify as INotify
    ( WatchDescriptor
    , addWatch
    , removeWatch
    )
import System.IO.MMap (Mode(ReadOnly), mmapWithFilePtr)


type NumberOfLines = Word
type FileOffset = Int

-- | Read last 'NumberOfLines' of a file along with file offset pointing to its
-- end.
readFileTail :: FilePath -> NumberOfLines -> IO (Lazy.ByteString, FileOffset)
readFileTail file numberOfLines =
    fileTailPtr file numberOfLines peekLazyByteString
  where
    peekLazyByteString fileSize _ 0 = pure (mempty, fileSize)
    peekLazyByteString fileSize p s =
        (, fileSize) . Lazy.ByteString.fromChunks <$> loop (castPtr p) s
      where
        loop ptr size
          | size <= chunkSize = (: []) <$> createByteString ptr size
          | otherwise         =
            (:) <$> createByteString ptr chunkSize
                <*> loop (ptr `plusPtr` chunkSize) (size - chunkSize)

        createByteString src size =
            Internal.create size (\dst -> Internal.memcpy dst src size)

        chunkSize = Internal.defaultChunkSize

-- | Find a region of a file that contains last 'NumberOfLines'.
fileTailPtr
    :: FilePath
    -> NumberOfLines
    -> (Int -> Ptr () -> Int -> IO a)
    -- ^ Action invoked on a requested file region that is mmap-ed in to
    -- memory.
    --
    -- Parameter of the action are:
    --
    -- * First parameter of type 'Int' is the size of the file;
    -- * second parameter with type @'Ptr' ()@ is a pointer that points to the
    --   begining of the requested region;
    -- * and the third argument with type 'Int' contains size of the requested
    --   region. If this value is zero, then the requested region is empty.
    -> IO a
fileTailPtr file numberOfLines f =
    -- Syscall mmap is lazy, i.e. it loads pages in to memory only when they
    -- are needed. By reading bytes from the end of the file we are forcing
    -- system to load only tail of the file.
    mmapWithFilePtr file ReadOnly Nothing $ \(ptr, size) -> if
      | numberOfLines == 0 -> f size (ptr `plusPtr` size) 0
      | size <= 0          -> f 0 ptr 0
      | otherwise          -> do
            offset <- findTailRegion ptr size
            f size (ptr `plusPtr` offset) (size - offset)
  where
    findTailRegion ptr size = do
        -- Last byte of the file is located at (ptr `plusPtr` size - 1).
        let maxOffset = size - 1

        -- UNIX convention, in text files, is to put one LF character at the
        -- end of a file. Therefore, when last character in file is a LF, then
        -- we need to ignore it, otherwise we would end up counting a blank
        -- line.
        isLastCharLf <- peekAndCheckIfLf ptr maxOffset
        let startOffset = if isLastCharLf then maxOffset - 1 else maxOffset

        calculateOffset ptr startOffset numberOfLines

    -- Find offset of the first character on 'lines'-th line from the end of
    -- file, by looking for LF characters.
    calculateOffset ptr offset lines = loop offset lines
      where
        loop 0 _ = pure offset  -- Whole file can fit into requested interval.
        loop o 0 = pure o       -- Position at the beginning of the interval.
        loop o l = do
            isLf <- peekAndCheckIfLf ptr o
            let l' = if isLf then l - 1 else l
                o' = if isLf && l' == 0
                        then o + 1  -- Take in to account current LF, which
                                    -- needs to be ignored/skipped.
                        else o - 1
            loop o' l'

    peekAndCheckIfLf ptr offset = (== lf) <$> peekByteOff ptr offset
    lf = 0x0a :: Word8

-- | Read data from file starting on 'FileOffset' up to end of file.
hGetDelta
    :: Handle
    -> FileOffset
    -- ^ Offset where to start reading the file.
    -> IO (Lazy.ByteString, FileOffset)
    -- ^ File content starting from specified offset, and offset where the
    -- file currently ends.
hGetDelta h offset = do
    newOffset <- hFileSize h
    (, fromIntegral newOffset) <$> if newOffset == oldOffset
        then pure mempty
        else do
            hSeek h AbsoluteSeek oldOffset
            Lazy.ByteString.hGet h $ fromIntegral (newOffset - oldOffset)
  where
    oldOffset = fromIntegral offset

type AreInitialData = Bool

-- | Follow end of file (its tail) using provided 'INotify'.
--
-- Be aware, that this function assumes that the file already exists.
followFile
    :: INotify
    -> NumberOfLines
    -- ^ Number of lines to read from the end of file before waiting for it
    -- to change.
    -> FilePath
    -- ^ File to watch.
    -> (AreInitialData -> Lazy.ByteString -> IO ())
    -- ^ Action invoked whenever there are new data available.
    -> IO (INotify.WatchDescriptor, Handle)
followFile inotify numberOfLines file processData = do
    (initialData, initialOffset) <- readFileTail file numberOfLines
    processData True initialData
    h <- openBinaryFile file ReadMode
    ref <- newIORef initialOffset

    -- API changed in "hinotify 0.3.10" (yes, minor version change), and it now
    -- accepts 'RawFilePath' ('ByteString') instead of 'FilePath', hence the
    -- 'fromString'.
    ih <- INotify.addWatch inotify [Modify] (fromString file) $ \_event -> do
        (delta, offset) <- readIORef ref >>= hGetDelta h
        processData False delta `finally` atomicWriteIORef ref offset

    pure (ih, h)

-- | Stop following file, and perform cleanup.
stopFollowingFile :: (INotify.WatchDescriptor, Handle) -> IO ()
stopFollowingFile (wd, h) = do
    INotify.removeWatch wd
    hClose h

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
