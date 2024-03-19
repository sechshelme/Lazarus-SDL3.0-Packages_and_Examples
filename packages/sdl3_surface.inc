/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  TSDL_BlitMap = Pointer;
  PSDL_BlitMap = ^TSDL_BlitMap;

const
  SDL_SWSURFACE = 0;
  SDL_PREALLOC = $00000001;
  SDL_RLEACCEL = $00000002;
  SDL_DONTFREE = $00000004;
  SDL_SIMD_ALIGNED = $00000008;
  SDL_SURFACE_USES_PROPERTIES = $00000010;

type
  PSDL_ScaleMode = ^TSDL_ScaleMode;
  TSDL_ScaleMode = longint;

const
  SDL_SCALEMODE_NEAREST = 0;
  SDL_SCALEMODE_LINEAR = 1;
  SDL_SCALEMODE_BEST = 2;

type
  PSDL_FlipMode = ^TSDL_FlipMode;
  TSDL_FlipMode = longint;

const
  SDL_FLIP_NONE = 0;
  SDL_FLIP_HORIZONTAL = 1;
  SDL_FLIP_VERTICAL = 2;

type
  PPSDL_Surface = ^PSDL_Surface;
  PSDL_Surface = ^TSDL_Surface;

  TSDL_Surface = record
    flags: uint32;
    format: PSDL_PixelFormat;
    w: longint;
    h: longint;
    pitch: longint;
    pixels: pointer;
    reserved: pointer;
    locked: longint;
    list_blitmap: pointer;
    clip_rect: TSDL_Rect;
    map: PSDL_BlitMap;
    refcount: longint;
  end;

  TSDL_blit = function(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): longint; cdecl;

function SDL_CreateSurface(Width: longint; Height: longint; format: uint32): PSDL_Surface; cdecl; external;
function SDL_CreateSurfaceFrom(pixels: pointer; Width: longint; Height: longint; pitch: longint; format: uint32): PSDL_Surface; cdecl; external;
procedure SDL_DestroySurface(surface: PSDL_Surface); cdecl; external;
function SDL_GetSurfaceProperties(surface: PSDL_Surface): TSDL_PropertiesID; cdecl; external;

const
  SDL_PROP_SURFACE_COLORSPACE_NUMBER = 'SDL.surface.colorspace';
  SDL_PROP_SURFACE_MAXCLL_NUMBER = 'SDL.surface.maxCLL';
  SDL_PROP_SURFACE_MAXFALL_NUMBER = 'SDL.surface.maxFALL';
  SDL_PROP_SURFACE_SDR_WHITE_POINT_FLOAT = 'SDL.surface.SDR_white_point';
  SDL_PROP_SURFACE_HDR_HEADROOM_FLOAT = 'SDL.surface.HDR_headroom';
  SDL_PROP_SURFACE_TONEMAP_OPERATOR_STRING = 'SDL.surface.tonemap';

function SDL_SetSurfaceColorspace(surface: PSDL_Surface; colorspace: TSDL_Colorspace): longint; cdecl; external;
function SDL_GetSurfaceColorspace(surface: PSDL_Surface; colorspace: PSDL_Colorspace): longint; cdecl; external;
function SDL_SetSurfacePalette(surface: PSDL_Surface; palette: PSDL_Palette): longint; cdecl; external;
function SDL_LockSurface(surface: PSDL_Surface): longint; cdecl; external;
procedure SDL_UnlockSurface(surface: PSDL_Surface); cdecl; external;
function SDL_LoadBMP_RW(src: PSDL_RWops; freesrc: TSDL_bool): PSDL_Surface; cdecl; external;
function SDL_LoadBMP(file_: PChar): PSDL_Surface; cdecl; external;
function SDL_SaveBMP_RW(surface: PSDL_Surface; dst: PSDL_RWops; freedst: TSDL_bool): longint; cdecl; external;
function SDL_SaveBMP(surface: PSDL_Surface; file_: PChar): longint; cdecl; external;
function SDL_SetSurfaceRLE(surface: PSDL_Surface; flag: longint): longint; cdecl; external;
function SDL_SurfaceHasRLE(surface: PSDL_Surface): TSDL_bool; cdecl; external;
function SDL_SetSurfaceColorKey(surface: PSDL_Surface; flag: longint; key: Uint32): longint; cdecl; external;
function SDL_SurfaceHasColorKey(surface: PSDL_Surface): TSDL_bool; cdecl; external;
function SDL_GetSurfaceColorKey(surface: PSDL_Surface; key: PUint32): longint; cdecl; external;
function SDL_SetSurfaceColorMod(surface: PSDL_Surface; r: Uint8; g: Uint8; b: Uint8): longint; cdecl; external;
function SDL_GetSurfaceColorMod(surface: PSDL_Surface; r: PUint8; g: PUint8; b: PUint8): longint; cdecl; external;
function SDL_SetSurfaceAlphaMod(surface: PSDL_Surface; alpha: Uint8): longint; cdecl; external;
function SDL_GetSurfaceAlphaMod(surface: PSDL_Surface; alpha: PUint8): longint; cdecl; external;
function SDL_SetSurfaceBlendMode(surface: PSDL_Surface; blendMode: TSDL_BlendMode): longint; cdecl; external;
function SDL_GetSurfaceBlendMode(surface: PSDL_Surface; blendMode: PSDL_BlendMode): longint; cdecl; external;
function SDL_SetSurfaceClipRect(surface: PSDL_Surface; rect: PSDL_Rect): TSDL_bool; cdecl; external;
function SDL_GetSurfaceClipRect(surface: PSDL_Surface; rect: PSDL_Rect): longint; cdecl; external;
function SDL_FlipSurface(surface: PSDL_Surface; flip: TSDL_FlipMode): longint; cdecl; external;
function SDL_DuplicateSurface(surface: PSDL_Surface): PSDL_Surface; cdecl; external;
function SDL_ConvertSurface(surface: PSDL_Surface; format: PSDL_PixelFormat): PSDL_Surface; cdecl; external;
function SDL_ConvertSurfaceFormat(surface: PSDL_Surface; pixel_format: Uint32): PSDL_Surface; cdecl; external;
function SDL_ConvertSurfaceFormatAndColorspace(surface: PSDL_Surface; pixel_format: Uint32; colorspace: TSDL_Colorspace; props: TSDL_PropertiesID): PSDL_Surface; cdecl; external;
function SDL_ConvertPixels(Width: longint; Height: longint; src_format:Uint32; src: pointer; src_pitch: longint;
  dst_format: Uint32; dst: pointer; dst_pitch: longint): longint; cdecl; external;
function SDL_ConvertPixelsAndColorspace(Width: longint; Height: longint; src_format: Uint32; src_colorspace: TSDL_Colorspace; src_properties: TSDL_PropertiesID;
  src: pointer; src_pitch: longint; dst_format: Uint32; dst_colorspace: TSDL_Colorspace; dst_properties: TSDL_PropertiesID;
  dst: pointer; dst_pitch: longint): longint; cdecl; external;
function SDL_PremultiplyAlpha(Width: longint; Height: longint; src_format: Uint32; src: pointer; src_pitch: longint;
  dst_format: Uint32; dst: pointer; dst_pitch: longint): longint; cdecl; external;
function SDL_FillSurfaceRect(dst: PSDL_Surface; rect: PSDL_Rect; color: Uint32): longint; cdecl; external;
function SDL_FillSurfaceRects(dst: PSDL_Surface; rects: PSDL_Rect; Count: longint; color: Uint32): longint; cdecl; external;
function SDL_BlitSurface(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): longint; cdecl; external;
function SDL_BlitSurfaceUnchecked(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): longint; cdecl; external;
function SDL_SoftStretch(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect; scaleMode: TSDL_ScaleMode): longint; cdecl; external;
function SDL_BlitSurfaceScaled(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect; scaleMode: TSDL_ScaleMode): longint; cdecl; external;
function SDL_BlitSurfaceUncheckedScaled(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect; scaleMode: TSDL_ScaleMode): longint; cdecl; external;
function SDL_ReadSurfacePixel(surface: PSDL_Surface; x: longint; y: longint; r: PUint8; g: PUint8;
  b: PUint8; a: PUint8): longint; cdecl; external;

function SDL_MUSTLOCK(S: PSDL_Surface): TSDL_bool;

{$ENDIF read_interface}


{$IFDEF read_implementation}

function SDL_MUSTLOCK(S: PSDL_Surface): TSDL_bool;
begin
  SDL_MUSTLOCK := TSDL_bool(((S^.flags) and SDL_RLEACCEL) <> 0);
end;

{$ENDIF read_implementation}
