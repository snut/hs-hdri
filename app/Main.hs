{-# Language TypeApplications #-}

module Main where

import Codec.HDRI
import Data.Function
import qualified Data.Vector.Storable as VS
import Linear

testPath :: FilePath
testPath = "./test/data/anniversary_lounge_1k.hdr"

main :: IO ()
main = do
  imgE <- hdrImageFromFile rgbeF32 testPath
  case imgE of
    Left err -> putStrLn err
    Right (HDRImage w h pixels) -> do
      putStrLn $ "Image dimensions: " ++ show w ++ " x " ++ show h
      putStrLn $ "Image min: " ++ show (VS.minimumBy (compare `on` quadrance) pixels)
      putStrLn $ "Image max: " ++ show (VS.maximumBy (compare `on` quadrance) pixels)
