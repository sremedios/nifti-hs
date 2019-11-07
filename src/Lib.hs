module Lib
    ( copyNIFTI
    ) where

import System.IO

getHandles :: IO (Handle, Handle)
getHandles = do
    -- TODO: make this parse cmd line args instead of prompting twice
    in_filename <- getLine
    out_filename <- getLine
    in_handle <- openBinaryFile in_filename ReadMode
    out_handle <- openBinaryFile out_filename WriteMode
    return (in_handle, out_handle)

copyNIFTI :: IO ()
copyNIFTI = do
    (inh, outh) <- getHandles
    rawData <- hGetContents inh -- read content lazily
    hPutStr outh rawData -- write content
    -- TODO: do processing here
    --close handles
    hClose inh
    hClose outh