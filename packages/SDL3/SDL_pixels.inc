/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   17:58                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

const
  SDL_ALPHA_OPAQUE = 255;
  SDL_ALPHA_OPAQUE_FLOAT = 1.0;
  SDL_ALPHA_TRANSPARENT = 0;
  SDL_ALPHA_TRANSPARENT_FLOAT = 0.0;

type
  PSDL_PixelType = ^TSDL_PixelType;
  TSDL_PixelType = longint;

const
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
  TSDL_BitmapOrder = longint;

const
  SDL_BITMAPORDER_NONE = 0;
  SDL_BITMAPORDER_4321 = 1;
  SDL_BITMAPORDER_1234 = 2;

type
  PSDL_PackedOrder = ^TSDL_PackedOrder;
  TSDL_PackedOrder = longint;

const
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
  TSDL_ArrayOrder = longint;

const
  SDL_ARRAYORDER_NONE = 0;
  SDL_ARRAYORDER_RGB = 1;
  SDL_ARRAYORDER_RGBA = 2;
  SDL_ARRAYORDER_ARGB = 3;
  SDL_ARRAYORDER_BGR = 4;
  SDL_ARRAYORDER_BGRA = 5;
  SDL_ARRAYORDER_ABGR = 6;

type
  PSDL_PackedLayout = ^TSDL_PackedLayout;
  TSDL_PackedLayout = longint;

const
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
  PSDL_PixelFormat = ^TSDL_PixelFormat;
  TSDL_PixelFormat = longint;

const
  SDL_PIXELFORMAT_UNKNOWN = 0;
  SDL_PIXELFORMAT_INDEX1LSB = $11100100;
  SDL_PIXELFORMAT_INDEX1MSB = $11200100;
  SDL_PIXELFORMAT_INDEX2LSB = $1c100200;
  SDL_PIXELFORMAT_INDEX2MSB = $1c200200;
  SDL_PIXELFORMAT_INDEX4LSB = $12100400;
  SDL_PIXELFORMAT_INDEX4MSB = $12200400;
  SDL_PIXELFORMAT_INDEX8 = $13000801;
  SDL_PIXELFORMAT_RGB332 = $14110801;
  SDL_PIXELFORMAT_XRGB4444 = $15120c02;
  SDL_PIXELFORMAT_XBGR4444 = $15520c02;
  SDL_PIXELFORMAT_XRGB1555 = $15130f02;
  SDL_PIXELFORMAT_XBGR1555 = $15530f02;
  SDL_PIXELFORMAT_ARGB4444 = $15321002;
  SDL_PIXELFORMAT_RGBA4444 = $15421002;
  SDL_PIXELFORMAT_ABGR4444 = $15721002;
  SDL_PIXELFORMAT_BGRA4444 = $15821002;
  SDL_PIXELFORMAT_ARGB1555 = $15331002;
  SDL_PIXELFORMAT_RGBA5551 = $15441002;
  SDL_PIXELFORMAT_ABGR1555 = $15731002;
  SDL_PIXELFORMAT_BGRA5551 = $15841002;
  SDL_PIXELFORMAT_RGB565 = $15151002;
  SDL_PIXELFORMAT_BGR565 = $15551002;
  SDL_PIXELFORMAT_RGB24 = $17101803;
  SDL_PIXELFORMAT_BGR24 = $17401803;
  SDL_PIXELFORMAT_XRGB8888 = $16161804;
  SDL_PIXELFORMAT_RGBX8888 = $16261804;
  SDL_PIXELFORMAT_XBGR8888 = $16561804;
  SDL_PIXELFORMAT_BGRX8888 = $16661804;
  SDL_PIXELFORMAT_ARGB8888 = $16362004;
  SDL_PIXELFORMAT_RGBA8888 = $16462004;
  SDL_PIXELFORMAT_ABGR8888 = $16762004;
  SDL_PIXELFORMAT_BGRA8888 = $16862004;
  SDL_PIXELFORMAT_XRGB2101010 = $16172004;
  SDL_PIXELFORMAT_XBGR2101010 = $16572004;
  SDL_PIXELFORMAT_ARGB2101010 = $16372004;
  SDL_PIXELFORMAT_ABGR2101010 = $16772004;
  SDL_PIXELFORMAT_RGB48 = $18103006;
  SDL_PIXELFORMAT_BGR48 = $18403006;
  SDL_PIXELFORMAT_RGBA64 = $18204008;
  SDL_PIXELFORMAT_ARGB64 = $18304008;
  SDL_PIXELFORMAT_BGRA64 = $18504008;
  SDL_PIXELFORMAT_ABGR64 = $18604008;
  SDL_PIXELFORMAT_RGB48_FLOAT = $1a103006;
  SDL_PIXELFORMAT_BGR48_FLOAT = $1a403006;
  SDL_PIXELFORMAT_RGBA64_FLOAT = $1a204008;
  SDL_PIXELFORMAT_ARGB64_FLOAT = $1a304008;
  SDL_PIXELFORMAT_BGRA64_FLOAT = $1a504008;
  SDL_PIXELFORMAT_ABGR64_FLOAT = $1a604008;
  SDL_PIXELFORMAT_RGB96_FLOAT = $1b10600c;
  SDL_PIXELFORMAT_BGR96_FLOAT = $1b40600c;
  SDL_PIXELFORMAT_RGBA128_FLOAT = $1b208010;
  SDL_PIXELFORMAT_ARGB128_FLOAT = $1b308010;
  SDL_PIXELFORMAT_BGRA128_FLOAT = $1b508010;
  SDL_PIXELFORMAT_ABGR128_FLOAT = $1b608010;
  SDL_PIXELFORMAT_YV12 = $32315659;
  SDL_PIXELFORMAT_IYUV = $56555949;
  SDL_PIXELFORMAT_YUY2 = $32595559;
  SDL_PIXELFORMAT_UYVY = $59565955;
  SDL_PIXELFORMAT_YVYU = $55595659;
  SDL_PIXELFORMAT_NV12 = $3231564e;
  SDL_PIXELFORMAT_NV21 = $3132564e;
  SDL_PIXELFORMAT_P010 = $30313050;
  SDL_PIXELFORMAT_EXTERNAL_OES = $2053454f;
  SDL_PIXELFORMAT_RGBA32 = SDL_PIXELFORMAT_ABGR8888;
  SDL_PIXELFORMAT_ARGB32 = SDL_PIXELFORMAT_BGRA8888;
  SDL_PIXELFORMAT_BGRA32 = SDL_PIXELFORMAT_ARGB8888;
  SDL_PIXELFORMAT_ABGR32 = SDL_PIXELFORMAT_RGBA8888;
  SDL_PIXELFORMAT_RGBX32 = SDL_PIXELFORMAT_XBGR8888;
  SDL_PIXELFORMAT_XRGB32 = SDL_PIXELFORMAT_BGRX8888;
  SDL_PIXELFORMAT_BGRX32 = SDL_PIXELFORMAT_XRGB8888;
  SDL_PIXELFORMAT_XBGR32 = SDL_PIXELFORMAT_RGBX8888;

type
  PSDL_ColorType = ^TSDL_ColorType;
  TSDL_ColorType = longint;

const
  SDL_COLOR_TYPE_UNKNOWN = 0;
  SDL_COLOR_TYPE_RGB = 1;
  SDL_COLOR_TYPE_YCBCR = 2;

type
  PSDL_ColorRange = ^TSDL_ColorRange;
  TSDL_ColorRange = longint;

const
  SDL_COLOR_RANGE_UNKNOWN = 0;
  SDL_COLOR_RANGE_LIMITED = 1;
  SDL_COLOR_RANGE_FULL = 2;

type
  PSDL_ColorPrimaries = ^TSDL_ColorPrimaries;
  TSDL_ColorPrimaries = longint;

const
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
  TSDL_TransferCharacteristics = longint;

const
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
  TSDL_MatrixCoefficients = longint;

const
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
  TSDL_ChromaLocation = longint;

const
  SDL_CHROMA_LOCATION_NONE = 0;
  SDL_CHROMA_LOCATION_LEFT = 1;
  SDL_CHROMA_LOCATION_CENTER = 2;
  SDL_CHROMA_LOCATION_TOPLEFT = 3;

type
  PSDL_Colorspace = ^TSDL_Colorspace;
  TSDL_Colorspace = longint;

const
  SDL_COLORSPACE_UNKNOWN = 0;
  SDL_COLORSPACE_SRGB = $120005a0;
  SDL_COLORSPACE_SRGB_LINEAR = $12000500;
  SDL_COLORSPACE_HDR10 = $12002600;
  SDL_COLORSPACE_JPEG = $220004c6;
  SDL_COLORSPACE_BT601_LIMITED = $211018c6;
  SDL_COLORSPACE_BT601_FULL = $221018c6;
  SDL_COLORSPACE_BT709_LIMITED = $21100421;
  SDL_COLORSPACE_BT709_FULL = $22100421;
  SDL_COLORSPACE_BT2020_LIMITED = $21102609;
  SDL_COLORSPACE_BT2020_FULL = $22102609;
  SDL_COLORSPACE_RGB_DEFAULT = SDL_COLORSPACE_SRGB;
  SDL_COLORSPACE_YUV_DEFAULT = SDL_COLORSPACE_JPEG;

type
  PSDL_Color = ^TSDL_Color;
  TSDL_Color = record
    case byte of
      1: (r, g, b, a: uint8);
      2: (items: array[0..3] of uint8);
  end;

  PSDL_FColor = ^TSDL_FColor;
  TSDL_FColor = record
    case byte of
      1: (r, g, b, a: single);
      2: (items: array[0..3] of single);
    end;

  TSDL_Palette = record
    ncolors: longint;
    colors: PSDL_Color;
    version: TUint32;
    refcount: longint;
  end;
  PSDL_Palette = ^TSDL_Palette;

  TSDL_PixelFormatDetails = record
    format: TSDL_PixelFormat;
    bits_per_pixel: TUint8;
    bytes_per_pixel: TUint8;
    padding: array[0..1] of TUint8;
    Rmask: TUint32;
    Gmask: TUint32;
    Bmask: TUint32;
    Amask: TUint32;
    Rbits: TUint8;
    Gbits: TUint8;
    Bbits: TUint8;
    Abits: TUint8;
    Rshift: TUint8;
    Gshift: TUint8;
    Bshift: TUint8;
    Ashift: TUint8;
  end;
  PSDL_PixelFormatDetails = ^TSDL_PixelFormatDetails;


function SDL_GetPixelFormatName(format: TSDL_PixelFormat): pansichar; cdecl; external libSDL3;
function SDL_GetMasksForPixelFormat(format: TSDL_PixelFormat; bpp: Plongint; Rmask: PUint32; Gmask: PUint32; Bmask: PUint32;
  Amask: PUint32): Tbool; cdecl; external libSDL3;
function SDL_GetPixelFormatForMasks(bpp: longint; Rmask: TUint32; Gmask: TUint32; Bmask: TUint32; Amask: TUint32): TSDL_PixelFormat; cdecl; external libSDL3;
function SDL_GetPixelFormatDetails(format: TSDL_PixelFormat): PSDL_PixelFormatDetails; cdecl; external libSDL3;
function SDL_CreatePalette(ncolors: longint): PSDL_Palette; cdecl; external libSDL3;
function SDL_SetPaletteColors(palette: PSDL_Palette; colors: PSDL_Color; firstcolor: longint; ncolors: longint): Tbool; cdecl; external libSDL3;
procedure SDL_DestroyPalette(palette: PSDL_Palette); cdecl; external libSDL3;
function SDL_MapRGB(format: PSDL_PixelFormatDetails; palette: PSDL_Palette; r: TUint8; g: TUint8; b: TUint8): TUint32; cdecl; external libSDL3;
function SDL_MapRGBA(format: PSDL_PixelFormatDetails; palette: PSDL_Palette; r: TUint8; g: TUint8; b: TUint8;
  a: TUint8): TUint32; cdecl; external libSDL3;
procedure SDL_GetRGB(pixel: TUint32; format: PSDL_PixelFormatDetails; palette: PSDL_Palette; r: PUint8; g: PUint8;
  b: PUint8); cdecl; external libSDL3;
procedure SDL_GetRGBA(pixel: TUint32; format: PSDL_PixelFormatDetails; palette: PSDL_Palette; r: PUint8; g: PUint8;
  b: PUint8; a: PUint8); cdecl; external libSDL3;

function SDL_DEFINE_PIXELFOURCC(A, B, C, D: byte): uint32;

function SDL_DEFINE_PIXELFORMAT(_type, order, layout, bits, bytes: longint): longint;

function SDL_PIXELFLAG(X: longint): longint;
function SDL_PIXELTYPE(X: longint): longint;
function SDL_PIXELORDER(X: longint): longint;
function SDL_PIXELLAYOUT(X: longint): longint;
function SDL_BITSPERPIXEL(X: longint): longint;

function SDL_BYTESPERPIXEL(X: longint): longint;
function SDL_ISPIXELFORMAT_INDEXED(format: longint): boolean;
function SDL_ISPIXELFORMAT_PACKED(format: longint): boolean;
function SDL_ISPIXELFORMAT_ARRAY(format: longint): boolean;
function SDL_ISPIXELFORMAT_ALPHA(format: longint): boolean;
function SDL_ISPIXELFORMAT_10BIT(format: longint): boolean;
function SDL_ISPIXELFORMAT_FLOAT(format: longint): boolean;

function SDL_ISPIXELFORMAT_FOURCC(format: TSDL_PixelFormat): boolean;

function SDL_DEFINE_COLORSPACE(_type, range, primaries, transfer, matrix, chroma: longint): longint;
function SDL_COLORSPACETYPE(X: longint): TSDL_ColorType;
function SDL_COLORSPACERANGE(X: longint): TSDL_ColorRange;
function SDL_COLORSPACECHROMA(X: longint): TSDL_ChromaLocation;
function SDL_COLORSPACEPRIMARIES(X: longint): TSDL_ColorPrimaries;
function SDL_COLORSPACETRANSFER(X: longint): TSDL_TransferCharacteristics;
function SDL_COLORSPACEMATRIX(X: longint): TSDL_MatrixCoefficients;

function SDL_ISCOLORSPACE_MATRIX_BT601(X: longint): boolean;
function SDL_ISCOLORSPACE_MATRIX_BT709(X: longint): boolean;
function SDL_ISCOLORSPACE_MATRIX_BT2020_NCL(X: longint): boolean;
function SDL_ISCOLORSPACE_LIMITED_RANGE(X: longint): boolean;
function SDL_ISCOLORSPACE_FULL_RANGE(X: longint): boolean;


{$ENDIF read_interface}


{$IFDEF read_implementation}

function SDL_DEFINE_PIXELFOURCC(A, B, C, D: byte): uint32;
begin
  SDL_DEFINE_PIXELFOURCC := SDL_FOURCC(A, B, C, D);
end;

function SDL_DEFINE_PIXELFORMAT(_type, order, layout, bits, bytes: longint): longint;
begin
  SDL_DEFINE_PIXELFORMAT := (((((1 shl 28) or (_type shl 24)) or (order shl 20)) or (layout shl 16)) or (bits shl 8)) or (bytes shl 0);
end;

function SDL_ISPIXELFORMAT_FOURCC(format: TSDL_PixelFormat): boolean;
begin
  Result := (format <> 0) and (SDL_PIXELFLAG(format) <> 1);
end;

function SDL_PIXELFLAG(X: longint): longint;
begin
  SDL_PIXELFLAG := (X shr 28) and $0F;
end;

function SDL_PIXELTYPE(X: longint): longint;
begin
  SDL_PIXELTYPE := (X shr 24) and $0F;
end;

function SDL_PIXELORDER(X: longint): longint;
begin
  SDL_PIXELORDER := (X shr 20) and $0F;
end;

function SDL_PIXELLAYOUT(X: longint): longint;
begin
  SDL_PIXELLAYOUT := (X shr 16) and $0F;
end;

function SDL_BITSPERPIXEL(X: longint): longint;
begin
  if SDL_ISPIXELFORMAT_FOURCC(X) then begin
    Result := 0;
  end else begin
    Result := (X shr 8) and $FF;
  end;
end;

function SDL_DEFINE_COLORSPACE(_type, range, primaries, transfer, matrix, chroma: longint): longint;
begin
  SDL_DEFINE_COLORSPACE := ((((((TUint32(_type)) shl 28) or ((TUint32(range)) shl 24)) or ((TUint32(chroma)) shl 20)) or ((TUint32(primaries)) shl 10)) or ((TUint32(transfer)) shl 5)) or ((TUint32(matrix)) shl 0);
end;

function SDL_COLORSPACETYPE(X: longint): TSDL_ColorType;
begin
  SDL_COLORSPACETYPE := TSDL_ColorType((X shr 28) and $0F);
end;

function SDL_COLORSPACERANGE(X: longint): TSDL_ColorRange;
begin
  SDL_COLORSPACERANGE := TSDL_ColorRange((X shr 24) and $0F);
end;

function SDL_COLORSPACECHROMA(X: longint): TSDL_ChromaLocation;
begin
  SDL_COLORSPACECHROMA := TSDL_ChromaLocation((X shr 20) and $0F);
end;

function SDL_COLORSPACEPRIMARIES(X: longint): TSDL_ColorPrimaries;
begin
  SDL_COLORSPACEPRIMARIES := TSDL_ColorPrimaries((X shr 10) and $1F);
end;

function SDL_COLORSPACETRANSFER(X: longint): TSDL_TransferCharacteristics;
begin
  SDL_COLORSPACETRANSFER := TSDL_TransferCharacteristics((X shr 5) and $1F);
end;

function SDL_COLORSPACEMATRIX(X: longint): TSDL_MatrixCoefficients;
begin
  SDL_COLORSPACEMATRIX := TSDL_MatrixCoefficients(X and $1F);
end;

function SDL_ISCOLORSPACE_MATRIX_BT601(X: longint): boolean;
begin
  SDL_ISCOLORSPACE_MATRIX_BT601 := (SDL_COLORSPACEMATRIX(X) = SDL_MATRIX_COEFFICIENTS_BT601) or (SDL_COLORSPACEMATRIX(X) = SDL_MATRIX_COEFFICIENTS_BT470BG);
end;

function SDL_ISCOLORSPACE_MATRIX_BT709(X: longint): boolean;
begin
  SDL_ISCOLORSPACE_MATRIX_BT709 := (SDL_COLORSPACEMATRIX(X)) = SDL_MATRIX_COEFFICIENTS_BT709;
end;

function SDL_ISCOLORSPACE_MATRIX_BT2020_NCL(X: longint): boolean;
begin
  SDL_ISCOLORSPACE_MATRIX_BT2020_NCL := (SDL_COLORSPACEMATRIX(X)) = SDL_MATRIX_COEFFICIENTS_BT2020_NCL;
end;

function SDL_ISCOLORSPACE_LIMITED_RANGE(X: longint): boolean;
begin
  SDL_ISCOLORSPACE_LIMITED_RANGE := (SDL_COLORSPACERANGE(X)) <> SDL_COLOR_RANGE_FULL;
end;

function SDL_ISCOLORSPACE_FULL_RANGE(X: longint): boolean;
begin
  SDL_ISCOLORSPACE_FULL_RANGE := (SDL_COLORSPACERANGE(X)) = SDL_COLOR_RANGE_FULL;
end;

function SDL_BYTESPERPIXEL(X: longint): longint;
begin
  if SDL_ISPIXELFORMAT_FOURCC(X) then begin
    if (X = SDL_PIXELFORMAT_YUY2) or (X = SDL_PIXELFORMAT_UYVY) or
      (X = SDL_PIXELFORMAT_YVYU) or (X = SDL_PIXELFORMAT_P010) then begin
      Result := 2;
    end else begin
      Result := 1;
    end;
  end else begin
    Result := (X shr 0) and $FF;
  end;
end;

function SDL_ISPIXELFORMAT_INDEXED(format: longint): boolean;
begin
  Result := not SDL_ISPIXELFORMAT_FOURCC(format) and
    ((SDL_PIXELTYPE(format) = SDL_PIXELTYPE_INDEX1) or
    (SDL_PIXELTYPE(format) = SDL_PIXELTYPE_INDEX2) or
    (SDL_PIXELTYPE(format) = SDL_PIXELTYPE_INDEX4) or
    (SDL_PIXELTYPE(format) = SDL_PIXELTYPE_INDEX8));
end;

function SDL_ISPIXELFORMAT_PACKED(format: longint): boolean;
begin
  Result := not SDL_ISPIXELFORMAT_FOURCC(format) and
    ((SDL_PIXELTYPE(format) = SDL_PIXELTYPE_PACKED8) or
    (SDL_PIXELTYPE(format) = SDL_PIXELTYPE_PACKED16) or
    (SDL_PIXELTYPE(format) = SDL_PIXELTYPE_PACKED32));
end;

function SDL_ISPIXELFORMAT_ARRAY(format: longint): boolean;
begin
  Result := not SDL_ISPIXELFORMAT_FOURCC(format) and
    ((SDL_PIXELTYPE(format) = SDL_PIXELTYPE_ARRAYU8) or
    (SDL_PIXELTYPE(format) = SDL_PIXELTYPE_ARRAYU16) or
    (SDL_PIXELTYPE(format) = SDL_PIXELTYPE_ARRAYU32) or
    (SDL_PIXELTYPE(format) = SDL_PIXELTYPE_ARRAYF16) or
    (SDL_PIXELTYPE(format) = SDL_PIXELTYPE_ARRAYF32));
end;

function SDL_ISPIXELFORMAT_ALPHA(format: longint): boolean;
begin
  Result := SDL_ISPIXELFORMAT_PACKED(format) and
    ((SDL_PIXELORDER(format) = SDL_PACKEDORDER_ARGB) or
    (SDL_PIXELORDER(format) = SDL_PACKEDORDER_RGBA) or
    (SDL_PIXELORDER(format) = SDL_PACKEDORDER_ABGR) or
    (SDL_PIXELORDER(format) = SDL_PACKEDORDER_BGRA));
end;

function SDL_ISPIXELFORMAT_10BIT(format: longint): boolean;
begin
  Result := not SDL_ISPIXELFORMAT_FOURCC(format) and
    ((SDL_PIXELTYPE(format) = SDL_PIXELTYPE_PACKED32) and
    (SDL_PIXELLAYOUT(format) = SDL_PACKEDLAYOUT_2101010));
end;

function SDL_ISPIXELFORMAT_FLOAT(format: longint): boolean;
begin
  Result := not SDL_ISPIXELFORMAT_FOURCC(format) and
    ((SDL_PIXELTYPE(format) = SDL_PIXELTYPE_ARRAYF16) or
    (SDL_PIXELTYPE(format) = SDL_PIXELTYPE_ARRAYF32));
end;


{$ENDIF read_implementation}
