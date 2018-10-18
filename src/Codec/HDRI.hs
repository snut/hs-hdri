{-# Language RankNTypes, ScopedTypeVariables #-}
{-# Language OverloadedStrings #-}
{-# Language GADTs #-}

module Codec.HDRI
  (
  -- * Basic types
    HDRImage(..)
  , Converter
  , RGBE
  -- * File loading
  , hdrImageFromFile
  , hdrImageFromBytes
  -- * Colour conversion
  , rgbeUnpack
  , rgbeF16
  , rgbeF32
  , rgbePack
  , rgbeId
  ) where

import           Control.Applicative ((<|>), Alternative, empty)

import           Control.Monad (guard, replicateM, MonadPlus)
import           Control.Monad.ST
import           Control.Monad.Trans.Class

import           Data.Bits (shiftL, (.&.), (.|.))

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)

import           Data.Foldable (forM_)

import qualified Data.Vector.Storable as VS
import           Data.Vector.Storable (Vector, Storable)
import qualified Data.Vector.Storable.Mutable as VSM
import           Data.Vector.Storable.Mutable (MVector)

import           Data.Void
import           Data.Word

import           Numeric.Half

import           Linear

import           Text.Megaparsec
import           Text.Megaparsec.Byte
import           Text.Megaparsec.Error (ParseError)

-- HDR format stores dimensions in a slightly odd way
-- the number of scanlines is always the first of the two,
-- but may correspond to X or Y.
-- In addition, the image may be mirrored in either direction.
-- Many files in the wild appear to just store +Y and +X as the two values.
-- TODO: consider exposing these transforms in the final image.
data HDRDim = PX Int | PY Int | NX Int | NY Int deriving (Eq, Show)

-- Forget the orientation and flipping
hdrDim :: HDRDim -> Int
hdrDim (PX i) = i
hdrDim (PY i) = i
hdrDim (NX i) = i
hdrDim (NY i) = i

-- | Intended to be somewhat compatible with the JuicyPixels image types.
--
-- NB: The hdr file format supports rotated (through 90 degrees) and flipped
-- orientations for pixel data, but this is not currently exposed.
data HDRImage pixel = HDRImage
  { _hdrImageWidth :: !Int
  , _hdrImageHeight :: !Int
  , _hdrImageData :: !(Vector pixel)
  } deriving (Show)

-- | Inverse of @rgbeUnpack@, converts a floating-point representation of
-- an HDR colour in RGB format, and converts to red, green, blue bytes with
-- a shared exponent.
{-# SPECIALIZE rgbePack :: V3 Float -> V4 Word8 #-}
{-# SPECIALIZE rgbePack :: V3 Half -> V4 Word8 #-}
rgbePack :: RealFloat a => V3 a -> V4 Word8
rgbePack rgb@(V3 fr fg fb)
  | fm < 1e-32 = pure 0
  | otherwise = V4 r g b (fromIntegral e + 128)
  where
    fm = max fr (max fg fb)
    sig = significand fm
    e = exponent fm
    scaling = if fm == 0 then 0 else sig * 255.9999 / fm
    V3 r g b = (truncate . (scaling *) . max 0) <$> rgb

-- | @Converter pixel@ converts HDR colour values stored as an RGBE quadruplet
-- of @Word8@s to some other representation @pixel@.
--
-- Usually, this will be done by converting to some triplet of floating point
-- values. See @rgbeUnpack@.
type Converter pixel = Word8 -> Word8 -> Word8 -> Word8 -> pixel


-- | Takes four @Word8@ representing reg, green, blue colour channel values
-- and an exponent, and converts to a triple floating point representation.
--
-- Usually, this will be @V3 Float@ or @V3 Half@. The latter is especially useful
-- for GPU consumption, although HDR block compressed formats are preferred
-- for higher performance in graphics applications.
{-# SPECIALIZE rgbeUnpack :: Word8 -> Word8 -> Word8 -> Word8 -> V3 Float #-}
{-# SPECIALIZE rgbeUnpack :: Word8 -> Word8 -> Word8 -> Word8 -> V3 Half #-}
rgbeUnpack :: forall a. RealFloat a => Converter (V3 a)
rgbeUnpack r g b e
  | e == 0 = pure 0
  | otherwise = (scl *) . (0.5 +) . fromIntegral <$> V3 r g b
  where
    scl = 2**(fromIntegral e - (128+8) :: a)

-- | Monomorphic version of @rgbeUnpack@, converting to half-precision
-- floating point RGB.
rgbeF16 :: Converter (V3 Half)
rgbeF16 = rgbeUnpack

-- | Monomorphic version of @rgbeUnpack@, converting to single-precision
-- floating point RGB.
rgbeF32 :: Converter (V3 Float)
rgbeF32 = rgbeUnpack


type Parser = Parsec Void ByteString
type ParserST s = ParsecT Void ByteString (ST s)
type Result = Either (ParseError ByteString Void)

-- | A simple type to 'convert' to, it just stores the raw bytes.
type RGBE = V4 Word8

-- | The simplest @Converter@, simple bundles the RGBE @Word8@ values together.
rgbeId :: Converter RGBE
rgbeId = V4

-- names more similar to attoparsec
word8 :: (MonadParsec e s m, Token s ~ Word8) => Token s -> m (Token s)
word8 = char

anyWord8 :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
anyWord8 = anyChar



-- old scanlines can only RLE entire pixels
oldScanline :: Storable pixel => Converter pixel -> Int -> Parser (Vector pixel)
oldScanline f len = do
    state <- getParserState
    let (state', result) = runST $ do
          mv <-  VSM.new len
          runParserT' (colours f (f 0 0 0 0) 0 0 mv) state
    case result of
      Right v -> setParserState state' *> pure v
      Left e -> empty
  where
    colours :: forall s pixel. Storable pixel => Converter pixel -> pixel -> Int -> Int -> MVector s pixel -> ParserST s (Vector pixel)
    colours f old shft l mv
      | l >= len  = lift $ VS.unsafeFreeze mv
      | otherwise = rle f old shft l mv <|> single f l mv

    rle :: forall s pixel. Storable pixel => Converter pixel -> pixel -> Int -> Int -> MVector s pixel -> ParserST s (Vector pixel)
    rle f old shft l mv = do
      n <- word8 1 *> word8 1 *> word8 1 *> anyWord8
      let n' = fromIntegral n `shiftL` shft
      lift $ forM_ [0 .. n'-1] $ \i -> VSM.write mv (i + l) old
      colours f old (shft+8) (l+n') mv

    single :: forall s pixel. Storable pixel => Converter pixel -> Int -> MVector s pixel -> ParserST s (Vector pixel)
    single f l mv = do
      px <- f <$> anyWord8 <*> anyWord8 <*> anyWord8 <*> anyWord8
      lift $ VSM.write mv l px
      colours f px 0 (l+1) mv

-- gets a vector of single components (R, G, B or Exponent)
newScanlineComponent :: Int -> Parser (Vector Word8)
newScanlineComponent len = do
    state <- getParserState
    let (state', result) = runST $ do
          mv <- VSM.new len
          runParserT' (mscanline 0 mv) state
    case result of
      Right v -> setParserState state' *> pure v
      Left e -> empty
  where
    mscanline :: forall s. Int -> MVector s Word8 -> ParserST s (Vector Word8)
    mscanline x mv
      | x >= len = lift $ VS.unsafeFreeze mv
      | otherwise = do
        code <- anyWord8
        if code > 128
          then do
            let n = fromIntegral (code .&. 127)
            val <- anyWord8
            lift $ forM_ [0..n-1] $ \i -> VSM.write mv (x+i) val
            mscanline (x + n) mv
          else do
            let n = fromIntegral code
            forM_ [0..n-1] $ \i -> anyWord8 >>= lift . VSM.write mv (x+i)
            mscanline (x + n) mv

-- gets a scanline using newer encoding method, where RLE
-- is per-component
newScanline :: Storable pixel => Converter pixel -> Int -> Parser (Vector pixel)
newScanline f len = do
  _ <- try $ do
    _ <- word8 2
    _ <- word8 2
    hi <- satisfy (\b -> b .&. 128 == 0)
    lo <- anyWord8
    let len' = fromIntegral hi `shiftL` 8 .|. fromIntegral lo
    guard (len' == len)
  rs <- newScanlineComponent len <?> "red pixel components"
  gs <- newScanlineComponent len <?> "green pixel components"
  bs <- newScanlineComponent len <?> "blue pixel components"
  es <- newScanlineComponent len <?> "exponent pixel components"
  pure $ VS.zipWith4 f rs gs bs es

parseScanlines :: Storable pixel => Converter pixel -> Int -> Int -> Parser [VS.Vector pixel]
parseScanlines f w h
  | w < 8 || w > 0x7fff = replicateM h (oldScanline f w)
  | otherwise = replicateM h (newScanline f w <|> oldScanline f w)

-- | Loads an image from a .hdr file, converting the colours from RGBE as it goes.
hdrImageFromFile :: Storable pixel => Converter pixel -> FilePath -> IO (Either String (HDRImage pixel))
hdrImageFromFile f path = do
    bytes <- B8.readFile path
    pure $ case parse (hdrParser f) path bytes of
      Right v -> Right v
      Left err -> Left $ parseErrorPretty err

-- | Loads an image from a supplied bytestring, converting the colours as it goes.
hdrImageFromBytes :: Storable pixel => Converter pixel -> ByteString -> Either String (HDRImage pixel)
hdrImageFromBytes f bytes = case parse (hdrParser f) "ByteString" bytes of
  Right i -> Right i
  Left err -> Left $ parseErrorPretty err

many1 :: MonadPlus m => m a -> m [a]
many1 p = (:) <$> p <*> many p

prefixInt :: ByteString -> Parser Int
prefixInt p = space *> string p *> space1 *> pint
  where
    pint :: Parser Int
    pint = read . B8.unpack . B.pack <$> many1 digitChar

garbage :: Parser [Word8]
garbage = manyTill asciiChar (string "\n\n")

hdrParser :: Storable pixel => Converter pixel -> Parser (HDRImage pixel)
hdrParser f = do
    string "#?RADIANCE\n"
    garbage
    height <- hdrDim <$> dim
    width <- hdrDim <$> dim
    newline
    pixels <- VS.concat <$> parseScanlines f width height
    pure $ HDRImage width height pixels
  where
    dim = px <|> py <|> nx <|> ny <?> "HDR digit"
    px = PX <$> prefixInt "+X"
    py = PY <$> prefixInt "+Y"
    nx = NX <$> prefixInt "-X"
    ny = NY <$> prefixInt "-Y"
