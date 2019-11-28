module NiftiHeader where

import GHC.TypeLits
import Data.Kind
import Data.Word
import Data.Proxy
import Data.Type.Equality
import Control.Monad (when)
import Data.String (fromString)

import GHC.TypeLits.Compare
import GHC.TypeLits.Witnesses
import Data.Bifunctor
import Conversion
import Data.Binary.Get (Get, skip, lookAhead, runGetOrFail)
import Data.Vector.Unboxed.Sized (Vector)
import qualified Data.Vector.Unboxed.Sized as V
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import qualified Endianness as E

type DimSize (n::Nat) = (KnownNat n, 1 <= n, n <= 7)

data DimSizeWitness (n::Nat) where
  DimSizeWitness :: (KnownNat n) => ((1 <=? n) :~: 'True) -> ((n <=? 7) :~: 'True) -> DimSizeWitness n

isDimSize :: (KnownNat n) => Proxy n -> Maybe (DimSizeWitness n)
isDimSize p = case (isLE p1 p, isLE p p7) of
                (Just Refl, Just Refl) -> Just $ DimSizeWitness Refl Refl
                _ -> Nothing
  where p1 = Proxy @ 1
        p7 = Proxy @ 7

data SomeSize (t :: Nat -> Type) where
  SomeSize :: (DimSize n) => t n -> SomeSize t

-- array types used in header
data Dim (n::Nat) where
  Dim :: (DimSize n) => Vector n Word8 -> Dim n

type SomeDim = SomeSize Dim

data PixDim (n::Nat) where
  PixDim :: (DimSize n) => Vector n Float -> PixDim n

-- header itself
data Nifti1Header (n::Nat) = DimSize n => Nifti1Header
    { sizeof_hdr        :: Word32      -- MUST be 348
    , dim_info          :: Word8       -- MRI slice ordering
    , dim               :: Dim n            -- Data array dimensions
    , intent_p1         :: Float            -- 1st intent parameter
    , intent_p2         :: Float            -- 2nd intent parameter
    , intent_p3         :: Float            -- 3rd intent parameter
    , intent_code       :: Word8       -- NIFTIINTENT code
    , data_type         :: Word8       -- Defines data type
    , bitpix            :: Word8       -- number of bits/voxel
    , slice_start       :: Word8       -- first slice index
    , pixdim            :: PixDim n         -- grid spacing
    , vox_offset        :: Float            -- offset into .nii file
    , scl_slope         :: Float            -- Data scaling: slope
    , scl_inter         :: Float            -- data scaling: offset
    , slice_end         :: Word16      -- Last slice index
    , slice_code        :: Word8       -- Slice timing order
    , xyzt_units        :: Word8       -- Units of pixdim[1..4]
    , cal_max           :: Float            -- Max display intensity
    , cal_min           :: Float            -- Min display intensity
    , slice_duration    :: Float            -- Time for 1 slice
    , toffset           :: Float            -- Time axis shift
    , descrip           :: T.Text           -- Any text description, total size is 80 bytes
    , aux_file          :: T.Text           -- auxiliary filename, total size is 24 bytes
    , qform_code        :: Word16      -- NIFTIXFORM code
    , sform_code        :: Word16      -- NIFTIXFORM code
    , quatern_b         :: Float            -- Quaternion b param
    , quatern_c         :: Float            -- Quaternion c param
    , quatern_d         :: Float            -- Quaternion d param
    , qoffset_x         :: Float            -- Quaternion x shift
    , qoffset_y         :: Float            -- Quaternion y shift
    , qoffset_z         :: Float            -- Quaternion z shift
    , srow_x            :: Float            -- 1st row affine transform TODO: as array of floats
    , srow_y            :: Float            -- 2nd row affine transform TODO: as array of floats
    , srow_z            :: Float            -- 3rd row affine transform TODO: as array of floats
    , intent_name       :: T.Text           -- Name or meaning of data
    , magic             :: T.Text           -- MUST be "ni1\0" or "n+1\0"
    }

type SomeNifti1Header = SomeSize Nifti1Header

snatToProxy :: SNat n -> Proxy n
snatToProxy SNat = Proxy

isSomeNatDimSize :: SomeNat -> Maybe (SomeSize DimSizeWitness)
isSomeNatDimSize (SomeNat p) = f <$> isDimSize p
  where
    f :: DimSizeWitness n -> SomeSize DimSizeWitness
    f wit@(DimSizeWitness Refl Refl) = SomeSize wit

getDim :: Get SomeDim
getDim = do
  length <- E.getWord8
  let maybeSomeLength = someNatVal $ convert length

  case isSomeNatDimSize =<< maybeSomeLength of
    Nothing -> fail "Invalid length"
    Just (SomeSize (_ :: DimSizeWitness n))-> do
      vec <- V.replicateM' (Proxy @ n) E.getWord8
      return $ SomeSize $ Dim vec

getPixDim :: (KnownNat n, DimSize n) => Proxy n -> E.Endianness -> Get (PixDim n)
getPixDim lengthP e = do
  vec <- V.replicateM' lengthP $ E.getFloat e
  return $ PixDim vec

determineNifti1HeaderEndianness :: Get E.Endianness
determineNifti1HeaderEndianness = do
  isLittle <- isEndianness E.Little
  if isLittle
    then return E.Little
    else do
      isBig <- isEndianness E.Big
      if isBig
        then return E.Big
        else fail "sizeof_hdr field invalid"
  where
    isEndianness :: E.Endianness -> Get Bool
    isEndianness e = lookAhead $ do
      size <- E.getInt32 e
      return $ size == 348

getNifti1HeaderE :: E.Endianness -> Get SomeNifti1Header
getNifti1HeaderE e = do
  sizeof_hdr <- E.getWord32 e
  when (sizeof_hdr /= 348) $ fail "Invalid sizeof_hdr"

  skip 35 -- These 35 bytes of the header are unused

  dim_info <- E.getWord8
  SomeSize (dim :: Dim n) <- getDim
  intent_p1 <- E.getFloat e
  intent_p2 <- E.getFloat e
  intent_p3 <- E.getFloat e
  intent_code <- E.getWord8
  data_type <- E.getWord8
  bitpix <- E.getWord8
  slice_start <- E.getWord8
  pixdim <- getPixDim (Proxy @ n) e
  vox_offset <- E.getFloat e
  scl_slope <- E.getFloat e
  scl_inter <- E.getFloat e
  slice_end <- E.getWord16 e
  slice_code <- E.getWord8
  xyzt_units <- E.getWord8
  cal_max <- E.getFloat e
  cal_min <- E.getFloat e
  slice_duration <- E.getFloat e
  toffset <- E.getFloat e
  --descrip <- getByteString 160 -- read exactly 160 bytes
  let descrip = fromString "descrip"
  skip 160 -- skip this for now because i don't know how to handbe it
  -- aux_file <- getByteString 40 -- read exactly 40 bytes
  let aux_file = fromString "aux_file"
  skip 40 -- skip for now
  qform_code <- E.getWord16 e
  sform_code <- E.getWord16 e
  quatern_b <- E.getFloat e
  quatern_c <- E.getFloat e
  quatern_d <- E.getFloat e
  qoffset_x <- E.getFloat e
  qoffset_y <- E.getFloat e
  qoffset_z <- E.getFloat e
  srow_x <- E.getFloat e
  srow_y <- E.getFloat e
  srow_z <- E.getFloat e
  -- intent_name <- getByteString 16 -- read exactly 16 bytes
  let intent_name = fromString "intent_name"
  skip 16 -- skip for now
  -- magic <- getByteString 4 -- read exactly 4 bytes
  let magic = fromString "magic"
  skip 4  -- skip for now
  return . SomeSize $! Nifti1Header
    { sizeof_hdr = sizeof_hdr
    , dim_info = dim_info
    , dim = dim
    , intent_p1 = intent_p1
    , intent_p2 = intent_p2
    , intent_p3 = intent_p3
    , intent_code = intent_code
    , data_type = data_type
    , bitpix = bitpix
    , slice_start = slice_start
    , pixdim = pixdim
    , vox_offset = vox_offset
    , scl_slope = scl_slope
    , scl_inter = scl_inter
    , slice_end = slice_end
    , slice_code = slice_code
    , xyzt_units = xyzt_units
    , cal_max = cal_max
    , cal_min = cal_min
    , slice_duration = slice_duration
    , toffset = toffset
    , descrip = descrip
    , aux_file = aux_file
    , qform_code = qform_code
    , sform_code = sform_code
    , quatern_b = quatern_b
    , quatern_c = quatern_c
    , quatern_d = quatern_d
    , qoffset_x = qoffset_x
    , qoffset_y = qoffset_y
    , qoffset_z = qoffset_z
    , srow_x = srow_x
    , srow_y = srow_y
    , srow_z = srow_z
    , intent_name = intent_name
    , magic = magic
    }

getNifti1Header :: Get SomeNifti1Header
getNifti1Header =
  determineNifti1HeaderEndianness >>= getNifti1HeaderE

decodeNifti1Header :: BL.ByteString -> Either String (BL.ByteString, SomeNifti1Header)
decodeNifti1Header bs = first f $ do
  (unconsumed, _, hdr) <- runGetOrFail getNifti1Header bs
  return (unconsumed, hdr)
  where f (_,_,s) = s
