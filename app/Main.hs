module Main where

import Lib
import Header

main :: IO ()
main = do 
    h <- getNifti1HeaderLE
    putStrLn(h.sizeof_hdr)