
import Codec.HDRI
import Data.Function
import qualified Data.Vector.Storable as VS
import Linear
import System.Exit (exitFailure, exitSuccess)

--With thanks to HDRI Haven!
--https://hdrihaven.com/hdri/?h=anniversary_lounge
testPath :: FilePath
testPath = "./test/data/anniversary_lounge_1k.hdr"

main :: IO ()
main = do
  imgE <- hdrImageFromFile rgbeF16 testPath
  case imgE of
    Left err -> putStrLn err *> exitFailure
    Right (HDRImage w h pixels)
      | w == expW && h == expH && maxPixel == expMax && minPixel == expMin -> exitSuccess
      | otherwise -> do
        if w /= expW || h /= expH
          then do
            putStrLn $ "Image dimensions: " ++ show w ++ " x " ++ show h
            putStrLn $ "       Should be: " ++ show expW ++ " x " ++ show expH
          else do
            putStrLn $ "Image min pixel: " ++ show minPixel
            putStrLn $ "      Should be: " ++ show expMin
            putStrLn $ "Image max pixel: " ++ show maxPixel
            putStrLn $ "      Should be: " ++ show expMax
        exitFailure
      where
        expW = 1024
        expH = 512
        expMin = V3 3.982544e-3 4.3182373e-3 2.1209717e-3
        expMax = V3 210.5 93.5 5.5
        minPixel = VS.minimumBy (compare `on` quadrance) pixels
        maxPixel = VS.maximumBy (compare `on` quadrance) pixels
