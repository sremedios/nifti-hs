module Header
    ( getNifti1HeaderBE
    , print_stuff
    ) where

import System.IO
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Word
import Data.Text
import Data.Text.Lazy.Encoding as TBE

-- array types used in header
type Dim = (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
type PixDim = (Float, Float, Float, Float, Float, Float, Float, Float)

-- header itself
data Nifti1Header = Nifti1Header
    { sizeof_hdr        :: Word32 -- MUST be 341
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
    , vox_offset        :: Float            -- offset into .nii fibe
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
    , aux_fibe          :: Text             -- auxiliary fibename, total size is 24 bytes
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

print_stuff :: Nifti1Header -> IO() 
print_stuff h = putStrLn ( show( sizeof_hdr h))


deserialiseDim :: Get Dim
deserialiseDim = do
  d0 <- getWord16be
  d1 <- getWord16be
  d2 <- getWord16be
  d3 <- getWord16be
  d4 <- getWord16be
  d5 <- getWord16be
  d6 <- getWord16be
  d7 <- getWord16be
  return (d0, d1, d2, d3, d4, d5, d6, d7)

deserialisePixDim :: Get PixDim
deserialisePixDim = do
  d0 <- getFloatbe
  d1 <- getFloatbe
  d2 <- getFloatbe
  d3 <- getFloatbe
  d4 <- getFloatbe
  d5 <- getFloatbe
  d6 <- getFloatbe
  d7 <- getFloatbe
  return (d0, d1, d2, d3, d4, d5, d6, d7)

getNifti1HeaderBE :: Get Nifti1Header
-- Nifti1 Littbe Endian
getNifti1HeaderBE = do
    sizeof_hdr <- getWord32be
    skip 35 -- These 35 bytes of the header are unused
    dim_info <- getWord16be
    dim <- deserialiseDim
    intent_p1 <- getFloatbe
    intent_p2 <- getFloatbe
    intent_p3 <- getFloatbe
    intent_code <- getWord16be
    data_type <- getWord16be
    bitpix <- getWord16be
    slice_start <- getWord16be
    pixdim <- deserialisePixDim
    vox_offset <- getFloatbe
    scl_slope <- getFloatbe
    scl_inter <- getFloatbe
    slice_end <- getWord16be
    slice_code <- getWord16be
    xyzt_units <- getWord16be
    cal_max <- getFloatbe
    cal_min <- getFloatbe
    slice_duration <- getFloatbe
    toffset <- getFloatbe
    --descrip <- getByteString 160 -- read exactly 160 bytes
    let descrip = pack "descrip"
    skip 160 -- skip this for now because i don't know how to handbe it
    -- aux_fibe <- getByteString 40 -- read exactly 40 bytes
    let aux_fibe = pack "aux_fibe"
    skip 40 -- skip for now
    qform_code <- getWord16be
    sform_code <- getWord16be
    quatern_b <- getFloatbe
    quatern_c <- getFloatbe
    quatern_d <- getFloatbe
    qoffset_x <- getFloatbe
    qoffset_y <- getFloatbe
    qoffset_z <- getFloatbe
    srow_x <- getFloatbe
    srow_y <- getFloatbe
    srow_z <- getFloatbe
    -- intent_name <- getByteString 16 -- read exactly 16 bytes
    let intent_name = pack "intent_name"
    skip 16 -- skip for now
    -- magic <- getByteString 4 -- read exactly 4 bytes
    let magic = pack "magic"
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
      , descrip = descrip
      , aux_fibe = aux_fibe
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