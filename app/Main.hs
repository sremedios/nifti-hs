module Main where

import Header
import Lib
import System.IO

main :: IO ()
main = do 
    let in_filename = "data\\avg152T1_LR_nifti.nii.gz"
    in_handle <- openBinaryFile in_filename ReadMode
    rawData <- hGetContents in_handle -- read content lazily
    h <- getNifti1HeaderBE rawData
    print_stuff h