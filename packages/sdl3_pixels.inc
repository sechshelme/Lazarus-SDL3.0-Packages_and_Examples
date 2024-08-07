/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 06-07-2024   17:47                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

const
  SDL_ALPHA_OPAQUE = 255;  
  SDL_ALPHA_TRANSPARENT = 0;  
type
  PSDL_PixelType = ^TSDL_PixelType;
  TSDL_PixelType =  Longint;
  Const
    SDL_PIXELTYPE_UNKNOWN = 0;
    SDL_PIXELTYPE_INDEX1 = 1;
    SDL_PIXELTYPE_INDEX4 = 2;
    SDL_PIXELTYPE_INDEX8 = 3;
    SDL_PIXELTYPE_PACKED8 = 4;
    SDL_PIXELTYPE_PACKED16 = 5;
    SDL_PIXELTYPE_PACKED32 = 6;
    SDL_PIXELTYPE_ARRAYU8 = 7;
    SDL_PIXELTYPE_ARRAYU16 = 8;
    SDL_PIXELTYPE_ARRAYU32 = 9;
    SDL_PIXELTYPE_ARRAYF16 = 10;
    SDL_PIXELTYPE_ARRAYF32 = 11;
    SDL_PIXELTYPE_INDEX2 = 12;
type
  PSDL_BitmapOrder = ^TSDL_BitmapOrder;
  TSDL_BitmapOrder =  Longint;
  Const
    SDL_BITMAPORDER_NONE = 0;
    SDL_BITMAPORDER_4321 = 1;
    SDL_BITMAPORDER_1234 = 2;
type
  PSDL_PackedOrder = ^TSDL_PackedOrder;
  TSDL_PackedOrder =  Longint;
  Const
    SDL_PACKEDORDER_NONE = 0;
    SDL_PACKEDORDER_XRGB = 1;
    SDL_PACKEDORDER_RGBX = 2;
    SDL_PACKEDORDER_ARGB = 3;
    SDL_PACKEDORDER_RGBA = 4;
    SDL_PACKEDORDER_XBGR = 5;
    SDL_PACKEDORDER_BGRX = 6;
    SDL_PACKEDORDER_ABGR = 7;
    SDL_PACKEDORDER_BGRA = 8;
type
  PSDL_ArrayOrder = ^TSDL_ArrayOrder;
  TSDL_ArrayOrder =  Longint;
  Const
    SDL_ARRAYORDER_NONE = 0;
    SDL_ARRAYORDER_RGB = 1;
    SDL_ARRAYORDER_RGBA = 2;
    SDL_ARRAYORDER_ARGB = 3;
    SDL_ARRAYORDER_BGR = 4;
    SDL_ARRAYORDER_BGRA = 5;
    SDL_ARRAYORDER_ABGR = 6;
type
  PSDL_PackedLayout = ^TSDL_PackedLayout;
  TSDL_PackedLayout =  Longint;
  Const
    SDL_PACKEDLAYOUT_NONE = 0;
    SDL_PACKEDLAYOUT_332 = 1;
    SDL_PACKEDLAYOUT_4444 = 2;
    SDL_PACKEDLAYOUT_1555 = 3;
    SDL_PACKEDLAYOUT_5551 = 4;
    SDL_PACKEDLAYOUT_565 = 5;
    SDL_PACKEDLAYOUT_8888 = 6;
    SDL_PACKEDLAYOUT_2101010 = 7;
    SDL_PACKEDLAYOUT_1010102 = 8;

type
  PSDL_PixelFormatEnum = ^TSDL_PixelFormatEnum;
  TSDL_PixelFormatEnum =  Longint;
  Const
    SDL_PIXELFORMAT_UNKNOWN = 0;
    SDL_PIXELFORMAT_INDEX1LSB = (1 shl 28) or (SDL_PIXELTYPE_INDEX1 shl 24) or (SDL_BITMAPORDER_4321 shl 20) or (0 shl 16) or (1 shl 8) or (0 shl 0);
    SDL_PIXELFORMAT_INDEX1MSB = (1 shl 28) or (SDL_PIXELTYPE_INDEX1 shl 24) or (SDL_BITMAPORDER_1234 shl 20) or (0 shl 16) or (1 shl 8) or (0 shl 0);
    SDL_PIXELFORMAT_INDEX2LSB = (1 shl 28) or (SDL_PIXELTYPE_INDEX2 shl 24) or (SDL_BITMAPORDER_4321 shl 20) or (0 shl 16) or (2 shl 8) or (0 shl 0);
    SDL_PIXELFORMAT_INDEX2MSB = (1 shl 28) or (SDL_PIXELTYPE_INDEX2 shl 24) or (SDL_BITMAPORDER_1234 shl 20) or (0 shl 16) or (2 shl 8) or (0 shl 0);
    SDL_PIXELFORMAT_INDEX4LSB = (1 shl 28) or (SDL_PIXELTYPE_INDEX4 shl 24) or (SDL_BITMAPORDER_4321 shl 20) or (0 shl 16) or (4 shl 8) or (0 shl 0);
    SDL_PIXELFORMAT_INDEX4MSB = (1 shl 28) or (SDL_PIXELTYPE_INDEX4 shl 24) or (SDL_BITMAPORDER_1234 shl 20) or (0 shl 16) or (4 shl 8) or (0 shl 0);
    SDL_PIXELFORMAT_INDEX8 = (1 shl 28) or (SDL_PIXELTYPE_INDEX8 shl 24) or (0 shl 20) or (0 shl 16) or (8 shl 8) or (1 shl 0);
    SDL_PIXELFORMAT_RGB332 = (1 shl 28) or (SDL_PIXELTYPE_PACKED8 shl 24) or (SDL_PACKEDORDER_XRGB shl 20) or (SDL_PACKEDLAYOUT_332 shl 16) or (8 shl 8) or (1 shl 0);
    SDL_PIXELFORMAT_XRGB4444 = (1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_XRGB shl 20) or (SDL_PACKEDLAYOUT_4444 shl 16) or (12 shl 8) or (2 shl 0);
    SDL_PIXELFORMAT_RGB444 = SDL_PIXELFORMAT_XRGB4444;
    SDL_PIXELFORMAT_XBGR4444 = (1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_XBGR shl 20) or (SDL_PACKEDLAYOUT_4444 shl 16) or (12 shl 8) or (2 shl 0);
    SDL_PIXELFORMAT_BGR444 = SDL_PIXELFORMAT_XBGR4444;
    SDL_PIXELFORMAT_XRGB1555 = (1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_XRGB shl 20) or (SDL_PACKEDLAYOUT_1555 shl 16) or (15 shl 8) or (2 shl 0);
    SDL_PIXELFORMAT_RGB555 = SDL_PIXELFORMAT_XRGB1555;
    SDL_PIXELFORMAT_XBGR1555 = (1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_XBGR shl 20) or (SDL_PACKEDLAYOUT_1555 shl 16) or (15 shl 8) or (2 shl 0);
    SDL_PIXELFORMAT_BGR555 = SDL_PIXELFORMAT_XBGR1555;
    SDL_PIXELFORMAT_ARGB4444 = (1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_ARGB shl 20) or (SDL_PACKEDLAYOUT_4444 shl 16) or (16 shl 8) or (2 shl 0);
    SDL_PIXELFORMAT_RGBA4444 = (1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_RGBA shl 20) or (SDL_PACKEDLAYOUT_4444 shl 16) or (16 shl 8) or (2 shl 0);
    SDL_PIXELFORMAT_ABGR4444 = (1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_ABGR shl 20) or (SDL_PACKEDLAYOUT_4444 shl 16) or (16 shl 8) or (2 shl 0);
    SDL_PIXELFORMAT_BGRA4444 = (1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_BGRA shl 20) or (SDL_PACKEDLAYOUT_4444 shl 16) or (16 shl 8) or (2 shl 0);
    SDL_PIXELFORMAT_ARGB1555 = (1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_ARGB shl 20) or (SDL_PACKEDLAYOUT_1555 shl 16) or (16 shl 8) or (2 shl 0);
    SDL_PIXELFORMAT_RGBA5551 = (1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_RGBA shl 20) or (SDL_PACKEDLAYOUT_5551 shl 16) or (16 shl 8) or (2 shl 0);
    SDL_PIXELFORMAT_ABGR1555 = (1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_ABGR shl 20) or (SDL_PACKEDLAYOUT_1555 shl 16) or (16 shl 8) or (2 shl 0);
    SDL_PIXELFORMAT_BGRA5551 = (1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_BGRA shl 20) or (SDL_PACKEDLAYOUT_5551 shl 16) or (16 shl 8) or (2 shl 0);
    SDL_PIXELFORMAT_RGB565 = (1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_XRGB shl 20) or (SDL_PACKEDLAYOUT_565 shl 16) or (16 shl 8) or (2 shl 0);
    SDL_PIXELFORMAT_BGR565 = (1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_XBGR shl 20) or (SDL_PACKEDLAYOUT_565 shl 16) or (16 shl 8) or (2 shl 0);
    SDL_PIXELFORMAT_RGB24 = (1 shl 28) or (SDL_PIXELTYPE_ARRAYU8 shl 24) or (SDL_ARRAYORDER_RGB shl 20) or (0 shl 16) or (24 shl 8) or (3 shl 0);
    SDL_PIXELFORMAT_BGR24 = (1 shl 28) or (SDL_PIXELTYPE_ARRAYU8 shl 24) or (SDL_ARRAYORDER_BGR shl 20) or (0 shl 16) or (24 shl 8) or (3 shl 0);
    SDL_PIXELFORMAT_XRGB8888 = (1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_XRGB shl 20) or (SDL_PACKEDLAYOUT_8888 shl 16) or (24 shl 8) or (4 shl 0);
    SDL_PIXELFORMAT_RGBX8888 = (1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_RGBX shl 20) or (SDL_PACKEDLAYOUT_8888 shl 16) or (24 shl 8) or (4 shl 0);
    SDL_PIXELFORMAT_XBGR8888 = (1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_XBGR shl 20) or (SDL_PACKEDLAYOUT_8888 shl 16) or (24 shl 8) or (4 shl 0);
    SDL_PIXELFORMAT_BGRX8888 = (1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_BGRX shl 20) or (SDL_PACKEDLAYOUT_8888 shl 16) or (24 shl 8) or (4 shl 0);
    SDL_PIXELFORMAT_ARGB8888 = (1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_ARGB shl 20) or (SDL_PACKEDLAYOUT_8888 shl 16) or (32 shl 8) or (4 shl 0);
    SDL_PIXELFORMAT_RGBA8888 = (1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_RGBA shl 20) or (SDL_PACKEDLAYOUT_8888 shl 16) or (32 shl 8) or (4 shl 0);
    SDL_PIXELFORMAT_ABGR8888 = (1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_ABGR shl 20) or (SDL_PACKEDLAYOUT_8888 shl 16) or (32 shl 8) or (4 shl 0);
    SDL_PIXELFORMAT_BGRA8888 = (1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_BGRA shl 20) or (SDL_PACKEDLAYOUT_8888 shl 16) or (32 shl 8) or (4 shl 0);
    SDL_PIXELFORMAT_XRGB2101010 = (1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_XRGB shl 20) or (SDL_PACKEDLAYOUT_2101010 shl 16) or (32 shl 8) or (4 shl 0);
    SDL_PIXELFORMAT_XBGR2101010 = (1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_XBGR shl 20) or (SDL_PACKEDLAYOUT_2101010 shl 16) or (32 shl 8) or (4 shl 0);
    SDL_PIXELFORMAT_ARGB2101010 = (1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_ARGB shl 20) or (SDL_PACKEDLAYOUT_2101010 shl 16) or (32 shl 8) or (4 shl 0);
    SDL_PIXELFORMAT_ABGR2101010 = (1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_ABGR shl 20) or (SDL_PACKEDLAYOUT_2101010 shl 16) or (32 shl 8) or (4 shl 0);
    SDL_PIXELFORMAT_RGB48 = (1 shl 28) or (SDL_PIXELTYPE_ARRAYU16 shl 24) or (SDL_ARRAYORDER_RGB shl 20) or (0 shl 16) or (48 shl 8) or (6 shl 0);
    SDL_PIXELFORMAT_BGR48 = (1 shl 28) or (SDL_PIXELTYPE_ARRAYU16 shl 24) or (SDL_ARRAYORDER_BGR shl 20) or (0 shl 16) or (48 shl 8) or (6 shl 0);
    SDL_PIXELFORMAT_RGBA64 = (1 shl 28) or (SDL_PIXELTYPE_ARRAYU16 shl 24) or (SDL_ARRAYORDER_RGBA shl 20) or (0 shl 16) or (64 shl 8) or (8 shl 0);
    SDL_PIXELFORMAT_ARGB64 = (1 shl 28) or (SDL_PIXELTYPE_ARRAYU16 shl 24) or (SDL_ARRAYORDER_ARGB shl 20) or (0 shl 16) or (64 shl 8) or (8 shl 0);
    SDL_PIXELFORMAT_BGRA64 = (1 shl 28) or (SDL_PIXELTYPE_ARRAYU16 shl 24) or (SDL_ARRAYORDER_BGRA shl 20) or (0 shl 16) or (64 shl 8) or (8 shl 0);
    SDL_PIXELFORMAT_ABGR64 = (1 shl 28) or (SDL_PIXELTYPE_ARRAYU16 shl 24) or (SDL_ARRAYORDER_ABGR shl 20) or (0 shl 16) or (64 shl 8) or (8 shl 0);
    SDL_PIXELFORMAT_RGB48_FLOAT = (1 shl 28) or (SDL_PIXELTYPE_ARRAYF16 shl 24) or (SDL_ARRAYORDER_RGB shl 20) or (0 shl 16) or (48 shl 8) or (6 shl 0);
    SDL_PIXELFORMAT_BGR48_FLOAT = (1 shl 28) or (SDL_PIXELTYPE_ARRAYF16 shl 24) or (SDL_ARRAYORDER_BGR shl 20) or (0 shl 16) or (48 shl 8) or (6 shl 0);
    SDL_PIXELFORMAT_RGBA64_FLOAT = (1 shl 28) or (SDL_PIXELTYPE_ARRAYF16 shl 24) or (SDL_ARRAYORDER_RGBA shl 20) or (0 shl 16) or (64 shl 8) or (8 shl 0);
    SDL_PIXELFORMAT_ARGB64_FLOAT = (1 shl 28) or (SDL_PIXELTYPE_ARRAYF16 shl 24) or (SDL_ARRAYORDER_ARGB shl 20) or (0 shl 16) or (64 shl 8) or (8 shl 0);
    SDL_PIXELFORMAT_BGRA64_FLOAT = (1 shl 28) or (SDL_PIXELTYPE_ARRAYF16 shl 24) or (SDL_ARRAYORDER_BGRA shl 20) or (0 shl 16) or (64 shl 8) or (8 shl 0);
    SDL_PIXELFORMAT_ABGR64_FLOAT = (1 shl 28) or (SDL_PIXELTYPE_ARRAYF16 shl 24) or (SDL_ARRAYORDER_ABGR shl 20) or (0 shl 16) or (64 shl 8) or (8 shl 0);
    SDL_PIXELFORMAT_RGB96_FLOAT = (1 shl 28) or (SDL_PIXELTYPE_ARRAYF32 shl 24) or (SDL_ARRAYORDER_RGB shl 20) or (0 shl 16) or (96 shl 8) or (12 shl 0);
    SDL_PIXELFORMAT_BGR96_FLOAT = (1 shl 28) or (SDL_PIXELTYPE_ARRAYF32 shl 24) or (SDL_ARRAYORDER_BGR shl 20) or (0 shl 16) or (96 shl 8) or (12 shl 0);
    SDL_PIXELFORMAT_RGBA128_FLOAT = (1 shl 28) or (SDL_PIXELTYPE_ARRAYF32 shl 24) or (SDL_ARRAYORDER_RGBA shl 20) or (0 shl 16) or (128 shl 8) or (16 shl 0);
    SDL_PIXELFORMAT_ARGB128_FLOAT = (1 shl 28) or (SDL_PIXELTYPE_ARRAYF32 shl 24) or (SDL_ARRAYORDER_ARGB shl 20) or (0 shl 16) or (128 shl 8) or (16 shl 0);
    SDL_PIXELFORMAT_BGRA128_FLOAT = (1 shl 28) or (SDL_PIXELTYPE_ARRAYF32 shl 24) or (SDL_ARRAYORDER_BGRA shl 20) or (0 shl 16) or (128 shl 8) or (16 shl 0);
    SDL_PIXELFORMAT_ABGR128_FLOAT = (1 shl 28) or (SDL_PIXELTYPE_ARRAYF32 shl 24) or (SDL_ARRAYORDER_ABGR shl 20) or (0 shl 16) or (128 shl 8) or (16 shl 0);
// === Big Endian
//    SDL_PIXELFORMAT_RGBA32 = SDL_PIXELFORMAT_RGBA8888;
//    SDL_PIXELFORMAT_ARGB32 = SDL_PIXELFORMAT_ARGB8888;
//    SDL_PIXELFORMAT_BGRA32 = SDL_PIXELFORMAT_BGRA8888;
//    SDL_PIXELFORMAT_ABGR32 = SDL_PIXELFORMAT_ABGR8888;
//    SDL_PIXELFORMAT_RGBX32 = SDL_PIXELFORMAT_RGBX8888;
//    SDL_PIXELFORMAT_XRGB32 = SDL_PIXELFORMAT_XRGB8888;
//    SDL_PIXELFORMAT_BGRX32 = SDL_PIXELFORMAT_BGRX8888;
//    SDL_PIXELFORMAT_XBGR32 = SDL_PIXELFORMAT_XBGR8888;
// === Little Endian
    SDL_PIXELFORMAT_RGBA32 = SDL_PIXELFORMAT_ABGR8888;
    SDL_PIXELFORMAT_ARGB32 = SDL_PIXELFORMAT_BGRA8888;
    SDL_PIXELFORMAT_BGRA32 = SDL_PIXELFORMAT_ARGB8888;
    SDL_PIXELFORMAT_ABGR32 = SDL_PIXELFORMAT_RGBA8888;
    SDL_PIXELFORMAT_RGBX32 = SDL_PIXELFORMAT_XBGR8888;
    SDL_PIXELFORMAT_XRGB32 = SDL_PIXELFORMAT_BGRX8888;
    SDL_PIXELFORMAT_BGRX32 = SDL_PIXELFORMAT_XRGB8888;
    SDL_PIXELFORMAT_XBGR32 = SDL_PIXELFORMAT_RGBX8888;
    SDL_PIXELFORMAT_YV12 = (byte('Y') shl 0) or (byte('V') shl 8) or (byte('1') shl 16) or (byte('2') shl 24);
    SDL_PIXELFORMAT_IYUV = (byte('I') shl 0) or (byte('Y') shl 8) or (byte('U') shl 16) or (byte('V') shl 24);
    SDL_PIXELFORMAT_YUY2 = (byte('Y') shl 0) or (byte('U') shl 8) or (byte('Y') shl 16) or (byte('2') shl 24);
    SDL_PIXELFORMAT_UYVY = (byte('U') shl 0) or (byte('Y') shl 8) or (byte('V') shl 16) or (byte('Y') shl 24);
    SDL_PIXELFORMAT_YVYU = (byte('Y') shl 0) or (byte('V') shl 8) or (byte('Y') shl 16) or (byte('U') shl 24);
    SDL_PIXELFORMAT_NV12 = (byte('N') shl 0) or (byte('V') shl 8) or (byte('1') shl 16) or (byte('2') shl 24);
    SDL_PIXELFORMAT_NV21 = (byte('N') shl 0) or (byte('V') shl 8) or (byte('2') shl 16) or (byte('1') shl 24);
    SDL_PIXELFORMAT_P010 = (byte('P') shl 0) or (byte('0') shl 8) or (byte('1') shl 16) or (byte('0') shl 24);
    SDL_PIXELFORMAT_EXTERNAL_OES = (byte('O') shl 0) or (byte('E') shl 8) or (byte('S') shl 16) or (byte(' ') shl 24);
type
  PSDL_ColorType = ^TSDL_ColorType;
  TSDL_ColorType =  Longint;
  Const
    SDL_COLOR_TYPE_UNKNOWN = 0;
    SDL_COLOR_TYPE_RGB = 1;
    SDL_COLOR_TYPE_YCBCR = 2;
type
  PSDL_ColorRange = ^TSDL_ColorRange;
  TSDL_ColorRange =  Longint;
  Const
    SDL_COLOR_RANGE_UNKNOWN = 0;
    SDL_COLOR_RANGE_LIMITED = 1;
    SDL_COLOR_RANGE_FULL = 2;
type
  PSDL_ColorPrimaries = ^TSDL_ColorPrimaries;
  TSDL_ColorPrimaries =  Longint;
  Const
    SDL_COLOR_PRIMARIES_UNKNOWN = 0;
    SDL_COLOR_PRIMARIES_BT709 = 1;
    SDL_COLOR_PRIMARIES_UNSPECIFIED = 2;
    SDL_COLOR_PRIMARIES_BT470M = 4;
    SDL_COLOR_PRIMARIES_BT470BG = 5;
    SDL_COLOR_PRIMARIES_BT601 = 6;
    SDL_COLOR_PRIMARIES_SMPTE240 = 7;
    SDL_COLOR_PRIMARIES_GENERIC_FILM = 8;
    SDL_COLOR_PRIMARIES_BT2020 = 9;
    SDL_COLOR_PRIMARIES_XYZ = 10;
    SDL_COLOR_PRIMARIES_SMPTE431 = 11;
    SDL_COLOR_PRIMARIES_SMPTE432 = 12;
    SDL_COLOR_PRIMARIES_EBU3213 = 22;
    SDL_COLOR_PRIMARIES_CUSTOM = 31;
type
  PSDL_TransferCharacteristics = ^TSDL_TransferCharacteristics;
  TSDL_TransferCharacteristics =  Longint;
  Const
    SDL_TRANSFER_CHARACTERISTICS_UNKNOWN = 0;
    SDL_TRANSFER_CHARACTERISTICS_BT709 = 1;
    SDL_TRANSFER_CHARACTERISTICS_UNSPECIFIED = 2;
    SDL_TRANSFER_CHARACTERISTICS_GAMMA22 = 4;
    SDL_TRANSFER_CHARACTERISTICS_GAMMA28 = 5;
    SDL_TRANSFER_CHARACTERISTICS_BT601 = 6;
    SDL_TRANSFER_CHARACTERISTICS_SMPTE240 = 7;
    SDL_TRANSFER_CHARACTERISTICS_LINEAR = 8;
    SDL_TRANSFER_CHARACTERISTICS_LOG100 = 9;
    SDL_TRANSFER_CHARACTERISTICS_LOG100_SQRT10 = 10;
    SDL_TRANSFER_CHARACTERISTICS_IEC61966 = 11;
    SDL_TRANSFER_CHARACTERISTICS_BT1361 = 12;
    SDL_TRANSFER_CHARACTERISTICS_SRGB = 13;
    SDL_TRANSFER_CHARACTERISTICS_BT2020_10BIT = 14;
    SDL_TRANSFER_CHARACTERISTICS_BT2020_12BIT = 15;
    SDL_TRANSFER_CHARACTERISTICS_PQ = 16;
    SDL_TRANSFER_CHARACTERISTICS_SMPTE428 = 17;
    SDL_TRANSFER_CHARACTERISTICS_HLG = 18;
    SDL_TRANSFER_CHARACTERISTICS_CUSTOM = 31;
type
  PSDL_MatrixCoefficients = ^TSDL_MatrixCoefficients;
  TSDL_MatrixCoefficients =  Longint;
  Const
    SDL_MATRIX_COEFFICIENTS_IDENTITY = 0;
    SDL_MATRIX_COEFFICIENTS_BT709 = 1;
    SDL_MATRIX_COEFFICIENTS_UNSPECIFIED = 2;
    SDL_MATRIX_COEFFICIENTS_FCC = 4;
    SDL_MATRIX_COEFFICIENTS_BT470BG = 5;
    SDL_MATRIX_COEFFICIENTS_BT601 = 6;
    SDL_MATRIX_COEFFICIENTS_SMPTE240 = 7;
    SDL_MATRIX_COEFFICIENTS_YCGCO = 8;
    SDL_MATRIX_COEFFICIENTS_BT2020_NCL = 9;
    SDL_MATRIX_COEFFICIENTS_BT2020_CL = 10;
    SDL_MATRIX_COEFFICIENTS_SMPTE2085 = 11;
    SDL_MATRIX_COEFFICIENTS_CHROMA_DERIVED_NCL = 12;
    SDL_MATRIX_COEFFICIENTS_CHROMA_DERIVED_CL = 13;
    SDL_MATRIX_COEFFICIENTS_ICTCP = 14;
    SDL_MATRIX_COEFFICIENTS_CUSTOM = 31;
type
  PSDL_ChromaLocation = ^TSDL_ChromaLocation;
  TSDL_ChromaLocation =  Longint;
  Const
    SDL_CHROMA_LOCATION_NONE = 0;
    SDL_CHROMA_LOCATION_LEFT = 1;
    SDL_CHROMA_LOCATION_CENTER = 2;
    SDL_CHROMA_LOCATION_TOPLEFT = 3;

type
  PSDL_Colorspace = ^TSDL_Colorspace;
  TSDL_Colorspace =  Longint;
  Const
    SDL_COLORSPACE_UNKNOWN = 0;
    SDL_COLORSPACE_SRGB = (uint32(SDL_COLOR_TYPE_RGB) shl 28) or (uint32(SDL_COLOR_RANGE_FULL) shl 24) or (uint32(SDL_COLOR_PRIMARIES_BT709) shl 20) or (uint32(SDL_TRANSFER_CHARACTERISTICS_SRGB) shl 16) or (uint32(SDL_MATRIX_COEFFICIENTS_IDENTITY) shl 8) or (uint32(SDL_CHROMA_LOCATION_NONE) shl 0);
    SDL_COLORSPACE_SRGB_LINEAR = (uint32(SDL_COLOR_TYPE_RGB) shl 28) or (uint32(SDL_COLOR_RANGE_FULL) shl 24) or (uint32(SDL_COLOR_PRIMARIES_BT709) shl 20) or (uint32(SDL_TRANSFER_CHARACTERISTICS_LINEAR) shl 16) or (uint32(SDL_MATRIX_COEFFICIENTS_IDENTITY) shl 8) or (uint32(SDL_CHROMA_LOCATION_NONE) shl 0);
    SDL_COLORSPACE_HDR10 = (uint32(SDL_COLOR_TYPE_RGB) shl 28) or (uint32(SDL_COLOR_RANGE_FULL) shl 24) or (uint32(SDL_COLOR_PRIMARIES_BT2020) shl 20) or (uint32(SDL_TRANSFER_CHARACTERISTICS_PQ) shl 16) or (uint32(SDL_MATRIX_COEFFICIENTS_IDENTITY) shl 8) or (uint32(SDL_CHROMA_LOCATION_NONE) shl 0);
    SDL_COLORSPACE_JPEG = (uint32(SDL_COLOR_TYPE_YCBCR) shl 28) or (uint32(SDL_COLOR_RANGE_FULL) shl 24) or (uint32(SDL_COLOR_PRIMARIES_BT709) shl 20) or (uint32(SDL_TRANSFER_CHARACTERISTICS_BT601) shl 16) or (uint32(SDL_MATRIX_COEFFICIENTS_BT601) shl 8) or (uint32(SDL_CHROMA_LOCATION_NONE) shl 0);
    SDL_COLORSPACE_BT601_LIMITED = (uint32(SDL_COLOR_TYPE_YCBCR) shl 28) or (uint32(SDL_COLOR_RANGE_LIMITED) shl 24) or (uint32(SDL_COLOR_PRIMARIES_BT601) shl 20) or (uint32(SDL_TRANSFER_CHARACTERISTICS_BT601) shl 16) or (uint32(SDL_MATRIX_COEFFICIENTS_BT601) shl 8) or (uint32(SDL_CHROMA_LOCATION_LEFT) shl 0);
    SDL_COLORSPACE_BT601_FULL = (uint32(SDL_COLOR_TYPE_YCBCR) shl 28) or (uint32(SDL_COLOR_RANGE_FULL) shl 24) or (uint32(SDL_COLOR_PRIMARIES_BT601) shl 20) or (uint32(SDL_TRANSFER_CHARACTERISTICS_BT601) shl 16) or (uint32(SDL_MATRIX_COEFFICIENTS_BT601) shl 8) or (uint32(SDL_CHROMA_LOCATION_LEFT) shl 0);
    SDL_COLORSPACE_BT709_LIMITED = (uint32(SDL_COLOR_TYPE_YCBCR) shl 28) or (uint32(SDL_COLOR_RANGE_LIMITED) shl 24) or (uint32(SDL_COLOR_PRIMARIES_BT709) shl 20) or (uint32(SDL_TRANSFER_CHARACTERISTICS_BT709) shl 16) or (uint32(SDL_MATRIX_COEFFICIENTS_BT709) shl 8) or (uint32(SDL_CHROMA_LOCATION_LEFT) shl 0);
    SDL_COLORSPACE_BT709_FULL = (uint32(SDL_COLOR_TYPE_YCBCR) shl 28) or (uint32(SDL_COLOR_RANGE_FULL) shl 24) or (uint32(SDL_COLOR_PRIMARIES_BT709) shl 20) or (uint32(SDL_TRANSFER_CHARACTERISTICS_BT709) shl 16) or (uint32(SDL_MATRIX_COEFFICIENTS_BT709) shl 8) or (uint32(SDL_CHROMA_LOCATION_LEFT) shl 0);
    SDL_COLORSPACE_BT2020_LIMITED = (uint32(SDL_COLOR_TYPE_YCBCR) shl 28) or (uint32(SDL_COLOR_RANGE_LIMITED) shl 24) or (uint32(SDL_COLOR_PRIMARIES_BT2020) shl 20) or (uint32(SDL_TRANSFER_CHARACTERISTICS_PQ) shl 16) or (uint32(SDL_MATRIX_COEFFICIENTS_BT2020_NCL) shl 8) or (uint32(SDL_CHROMA_LOCATION_LEFT) shl 0);
    SDL_COLORSPACE_BT2020_FULL = (uint32(SDL_COLOR_TYPE_YCBCR) shl 28) or (uint32(SDL_COLOR_RANGE_FULL) shl 24) or (uint32(SDL_COLOR_PRIMARIES_BT2020) shl 20) or (uint32(SDL_TRANSFER_CHARACTERISTICS_PQ) shl 16) or (uint32(SDL_MATRIX_COEFFICIENTS_BT2020_NCL) shl 8) or (uint32(SDL_CHROMA_LOCATION_LEFT) shl 0);
    SDL_COLORSPACE_RGB_DEFAULT = SDL_COLORSPACE_SRGB;
    SDL_COLORSPACE_YUV_DEFAULT = SDL_COLORSPACE_JPEG;

type
  PSDL_Color = ^TSDL_Color;
  TSDL_Color = record
    case byte of
      1: (r, g, b, a: uint8);
      2: (items: array[0..3] of uint8);
  end;   

  TSDL_Colour = TSDL_Color;

type
  PSDL_FColor = ^TSDL_FColor;
  TSDL_FColor = record
    case byte of
      1: (r, g, b, a: single);
      2: (items: array[0..3] of single);
    end;

  TSDL_FColour = TSDL_FColor;
type
  PSDL_Palette = ^TSDL_Palette;
  TSDL_Palette = record
      ncolors : longint;
      colors : PSDL_Color;
      version : Uint32;
      refcount : longint;
    end;

  PSDL_PixelFormat = ^TSDL_PixelFormat;
  TSDL_PixelFormat = record
      format : Uint32;
      palette : PSDL_Palette;
      bits_per_pixel : Uint8;
      bytes_per_pixel : Uint8;
      padding : array[0..1] of Uint8;
      Rmask : Uint32;
      Gmask : Uint32;
      Bmask : Uint32;
      Amask : Uint32;
      Rloss : Uint8;
      Gloss : Uint8;
      Bloss : Uint8;
      Aloss : Uint8;
      Rshift : Uint8;
      Gshift : Uint8;
      Bshift : Uint8;
      Ashift : Uint8;
      refcount : longint;
      next : PSDL_PixelFormat;
    end;

function SDL_GetPixelFormatName(format:Uint32):Pchar;cdecl;external sdl3_lib;
function SDL_GetMasksForPixelFormatEnum(format:Uint32; bpp:Plongint; Rmask:PUint32; Gmask:PUint32; Bmask:PUint32;           Amask:PUint32):TSDL_bool;cdecl;external sdl3_lib;
function SDL_GetPixelFormatEnumForMasks(bpp:longint; Rmask:Uint32; Gmask:Uint32; Bmask:Uint32; Amask:Uint32):Uint32;cdecl;external sdl3_lib;
function SDL_CreatePixelFormat(pixel_format:Uint32):PSDL_PixelFormat;cdecl;external sdl3_lib;
procedure SDL_DestroyPixelFormat(format:PSDL_PixelFormat);cdecl;external sdl3_lib;
function SDL_CreatePalette(ncolors:longint):PSDL_Palette;cdecl;external sdl3_lib;
function SDL_SetPixelFormatPalette(format:PSDL_PixelFormat; palette:PSDL_Palette):longint;cdecl;external sdl3_lib;
function SDL_SetPaletteColors(palette:PSDL_Palette; colors:PSDL_Color; firstcolor:longint; ncolors:longint):longint;cdecl;external sdl3_lib;
procedure SDL_DestroyPalette(palette:PSDL_Palette);cdecl;external sdl3_lib;
function SDL_MapRGB(format:PSDL_PixelFormat; r:Uint8; g:Uint8; b:Uint8):Uint32;cdecl;external sdl3_lib;
function SDL_MapRGBA(format:PSDL_PixelFormat; r:Uint8; g:Uint8; b:Uint8; a:Uint8):Uint32;cdecl;external sdl3_lib;
procedure SDL_GetRGB(pixel:Uint32; format:PSDL_PixelFormat; r:PUint8; g:PUint8; b:PUint8);cdecl;external sdl3_lib;
procedure SDL_GetRGBA(pixel:Uint32; format:PSDL_PixelFormat; r:PUint8; g:PUint8; b:PUint8;            a:PUint8);cdecl;external sdl3_lib;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}
