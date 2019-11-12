module Header
    ( getNifti1HeaderLE
    ) where

import System.IO
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Word
import Data.Text

-- array types used in header
type Dim = (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
type PixDim = (Float, Float, Float, Float, Float, Float, Float, Float)

-- header itself
data Nifti1Header = Nifti1Header
    { sizeof_hdr        :: Word32      -- MUST be 3416
    , dim_info          :: Word16       -- MRI slice ordering
    , dim               :: Dim              -- array dimensions
    , intent_p1         :: Float            -- 1st intent parameter
    , intent_p2         :: Float            -- 2nd intent parameter
    , intent_p3         :: Float            -- 3rd intent parameter
    , intent_code       :: Word16       -- NIFTIINTENT code
    , data_type         :: Word16       -- Defines data type 
    , bitpix            :: Word16       -- number of bits/voxel
    , slice_start       :: Word16       -- first slice index
    , pixdim            :: PixDim           -- grid spacing
    , vox_offset        :: Float            -- offset into .nii file
    , scl_slope         :: Float            -- scaling: slope
    , scl_inter         :: Float            -- data scaling: offset
    , slice_end         :: Word16       -- Last slice index
    , slice_code        :: Word16       -- Slice timing order
    , xyzt_units        :: Word16       -- Units of pixdim[1..4]
    , cal_max           :: Float            -- Max display intensity
    , cal_min           :: Float            -- Min display intensity
    , slice_duration    :: Float            -- Time for 1 slice
    , toffset           :: Float            -- Time axis shift
    , descrip           :: Text             -- Any text description, total size is 160 bytes
    , aux_file          :: Text             -- auxiliary filename, total size is 24 bytes
    , qform_code        :: Word16       -- NIFTIXFORM code
    , sform_code        :: Word16       -- NIFTIXFORM code
    , quatern_b         :: Float            -- Quaternion b param
    , quatern_c         :: Float            -- Quaternion c param
    , quatern_d         :: Float            -- Quaternion d param
    , qoffset_x         :: Float            -- Quaternion x shift
    , qoffset_y         :: Float            -- Quaternion y shift
    , qoffset_z         :: Float            -- Quaternion z shift
    , srow_x            :: Float            -- 1st row affine transform TODO: as array of floats
    , srow_y            :: Float            -- 2nd row affine transform TODO: as array of floats
    , srow_z            :: Float            -- 3rd row affine transform TODO: as array of floats
    , intent_name       :: Text           -- Name or meaning of data
    , magic             :: Text           -- MUST be "ni1\0" or "n+1\0"
    }

deserialiseDim :: Get Dim
deserialiseDim = do
  d0 <- getWord16le
  d1 <- getWord16le
  d2 <- getWord16le
  d3 <- getWord16le
  d4 <- getWord16le
  d5 <- getWord16le
  d6 <- getWord16le
  d7 <- getWord16le
  return (d0, d1, d2, d3, d4, d5, d6, d7)

deserialisePixDim :: Get PixDim
deserialisePixDim = do
  d0 <- getFloatle
  d1 <- getFloatle
  d2 <- getFloatle
  d3 <- getFloatle
  d4 <- getFloatle
  d5 <- getFloatle
  d6 <- getFloatle
  d7 <- getFloatle
  return (d0, d1, d2, d3, d4, d5, d6, d7)

getNifti1HeaderLE :: Get Nifti1Header
-- Nifti1 Little Endian
getNifti1HeaderLE = do
    sizeof_hdr <- getWord32le
    skip 35 -- These 35 bytes of the header are unused
    dim_info <- getWord16le
    dim <- deserialiseDim
    intent_p1 <- getFloatle
    intent_p2 <- getFloatle
    intent_p3 <- getFloatle
    intent_code <- getWord16le
    data_type <- getWord16le
    bitpix <- getWord16le
    slice_start <- getWord16le
    pixdim <- deserialisePixDim
    vox_offset <- getFloatle
    scl_slope <- getFloatle
    scl_inter <- getFloatle
    slice_end <- getWord16le
    slice_code <- getWord16le
    xyzt_units <- getWord16le
    cal_max <- getFloatle
    cal_min <- getFloatle
    slice_duration <- getFloatle
    toffset <- getFloatle
    --descrip <- getByteString 160 -- read exactly 160 bytes
    skip 160 -- skip this for now because i don't know how to handle it
    -- aux_file <- getByteString 40 -- read exactly 40 bytes
    skip 40 -- skip for now
    qform_code <- getWord16le
    sform_code <- getWord16le
    quatern_b <- getFloatle
    quatern_c <- getFloatle
    quatern_d <- getFloatle
    qoffset_x <- getFloatle
    qoffset_y <- getFloatle
    qoffset_z <- getFloatle
    srow_x <- getFloatle
    srow_y <- getFloatle
    srow_z <- getFloatle
    -- intent_name <- getByteString 16 -- read exactly 16 bytes
    skip 16 -- skip for now
    -- magic <- getByteString 4 -- read exactly 4 bytes
    skip 4  -- skip for now
    return $! Nifti1Header
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
      -- , descrip = descrip
      -- , aux_file = aux_file
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
      -- , intent_name = intent_name
      -- , magic = magic
      }

{-
incrementalExample :: BL.ByteString -> [Nifti1Header]
incrementalExample input0 = go decoder input0
  where
    decoder = runGetIncremental getNifti1HeaderLE
    go :: Decoder Nifti1Header -> BL.ByteString -> [Nifti1Header]
    go (Done leftover _consumed hdr) input =
      hdr: go decoder (BL.chunk leftover input)
    go (Partial k) input                     =
      go (k . takeHeadChunk $ input) (dropHeadChunk input)
    go (Fail _leftover _consumed msg) _input =
      error msg

takeHeadChunk :: BL.ByteString -> Maybe BS.ByteString
takeHeadChunk lbs =
  case lbs of
    (BL.Chunk bs _) -> Just bs
    _ -> Nothing

dropHeadChunk :: BL.ByteString -> BL.ByteString
dropHeadChunk lbs =
  case lbs of
    (BL.Chunk _ lbs') -> lbs'
    _ -> BL.Empty

-}