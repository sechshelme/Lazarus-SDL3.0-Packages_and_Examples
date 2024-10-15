unit SDL_surface;

interface

uses
  ctypes, SDL_stdinc, SDL_properties, SDL_pixels, SDL_iostream, SDL_blendmode, SDL_rect;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  PSDL_SurfaceFlags = ^TSDL_SurfaceFlags;
  TSDL_SurfaceFlags = TUint32;

const
  SDL_SURFACE_PREALLOCATED = $00000001;
  SDL_SURFACE_LOCK_NEEDED = $00000002;
  SDL_SURFACE_LOCKED = $00000004;
  SDL_SURFACE_SIMD_ALIGNED = $00000008;

type
  PSDL_ScaleMode = ^TSDL_ScaleMode;
  TSDL_ScaleMode = longint;

const
  SDL_SCALEMODE_NEAREST = 0;
  SDL_SCALEMODE_LINEAR = 1;

type
  PSDL_FlipMode = ^TSDL_FlipMode;
  TSDL_FlipMode = longint;

const
  SDL_FLIP_NONE = 0;
  SDL_FLIP_HORIZONTAL = 1;
  SDL_FLIP_VERTICAL = 2;

type
  TSDL_Surface = record
    flags: TSDL_SurfaceFlags;
    format: TSDL_PixelFormat;
    w: longint;
    h: longint;
    pitch: longint;
    pixels: pointer;
    refcount: longint;
    reserved: pointer;
  end;
  PSDL_Surface = ^TSDL_Surface;
  PPSDL_Surface = ^PSDL_Surface;

function SDL_CreateSurface(Width: longint; Height: longint; format: TSDL_PixelFormat): PSDL_Surface; cdecl; external libSDL3;
function SDL_CreateSurfaceFrom(Width: longint; Height: longint; format: TSDL_PixelFormat; pixels: pointer; pitch: longint): PSDL_Surface; cdecl; external libSDL3;
procedure SDL_DestroySurface(surface: PSDL_Surface); cdecl; external libSDL3;
function SDL_GetSurfaceProperties(surface: PSDL_Surface): TSDL_PropertiesID; cdecl; external libSDL3;

const
  SDL_PROP_SURFACE_SDR_WHITE_POINT_FLOAT = 'SDL.surface.SDR_white_point';
  SDL_PROP_SURFACE_HDR_HEADROOM_FLOAT = 'SDL.surface.HDR_headroom';
  SDL_PROP_SURFACE_TONEMAP_OPERATOR_STRING = 'SDL.surface.tonemap';

function SDL_SetSurfaceColorspace(surface: PSDL_Surface; colorspace: TSDL_Colorspace): Tbool; cdecl; external libSDL3;
function SDL_GetSurfaceColorspace(surface: PSDL_Surface): TSDL_Colorspace; cdecl; external libSDL3;
function SDL_CreateSurfacePalette(surface: PSDL_Surface): PSDL_Palette; cdecl; external libSDL3;
function SDL_SetSurfacePalette(surface: PSDL_Surface; palette: PSDL_Palette): Tbool; cdecl; external libSDL3;
function SDL_GetSurfacePalette(surface: PSDL_Surface): PSDL_Palette; cdecl; external libSDL3;
function SDL_AddSurfaceAlternateImage(surface: PSDL_Surface; image: PSDL_Surface): Tbool; cdecl; external libSDL3;
function SDL_SurfaceHasAlternateImages(surface: PSDL_Surface): Tbool; cdecl; external libSDL3;
function SDL_GetSurfaceImages(surface: PSDL_Surface; Count: Plongint): PPSDL_Surface; cdecl; external libSDL3;
procedure SDL_RemoveSurfaceAlternateImages(surface: PSDL_Surface); cdecl; external libSDL3;
function SDL_LockSurface(surface: PSDL_Surface): Tbool; cdecl; external libSDL3;
procedure SDL_UnlockSurface(surface: PSDL_Surface); cdecl; external libSDL3;
function SDL_LoadBMP_IO(src: PSDL_IOStream; closeio: Tbool): PSDL_Surface; cdecl; external libSDL3;
function SDL_LoadBMP(file_: pansichar): PSDL_Surface; cdecl; external libSDL3;
function SDL_SaveBMP_IO(surface: PSDL_Surface; dst: PSDL_IOStream; closeio: Tbool): Tbool; cdecl; external libSDL3;
function SDL_SaveBMP(surface: PSDL_Surface; file_: pansichar): Tbool; cdecl; external libSDL3;
function SDL_SetSurfaceRLE(surface: PSDL_Surface; Enabled: Tbool): Tbool; cdecl; external libSDL3;
function SDL_SurfaceHasRLE(surface: PSDL_Surface): Tbool; cdecl; external libSDL3;
function SDL_SetSurfaceColorKey(surface: PSDL_Surface; Enabled: Tbool; key: TUint32): Tbool; cdecl; external libSDL3;
function SDL_SurfaceHasColorKey(surface: PSDL_Surface): Tbool; cdecl; external libSDL3;
function SDL_GetSurfaceColorKey(surface: PSDL_Surface; key: PUint32): Tbool; cdecl; external libSDL3;
function SDL_SetSurfaceColorMod(surface: PSDL_Surface; r: TUint8; g: TUint8; b: TUint8): Tbool; cdecl; external libSDL3;
function SDL_GetSurfaceColorMod(surface: PSDL_Surface; r: PUint8; g: PUint8; b: PUint8): Tbool; cdecl; external libSDL3;
function SDL_SetSurfaceAlphaMod(surface: PSDL_Surface; alpha: TUint8): Tbool; cdecl; external libSDL3;
function SDL_GetSurfaceAlphaMod(surface: PSDL_Surface; alpha: PUint8): Tbool; cdecl; external libSDL3;
function SDL_SetSurfaceBlendMode(surface: PSDL_Surface; blendMode: TSDL_BlendMode): Tbool; cdecl; external libSDL3;
function SDL_GetSurfaceBlendMode(surface: PSDL_Surface; blendMode: PSDL_BlendMode): Tbool; cdecl; external libSDL3;
function SDL_SetSurfaceClipRect(surface: PSDL_Surface; rect: PSDL_Rect): Tbool; cdecl; external libSDL3;
function SDL_GetSurfaceClipRect(surface: PSDL_Surface; rect: PSDL_Rect): Tbool; cdecl; external libSDL3;
function SDL_FlipSurface(surface: PSDL_Surface; flip: TSDL_FlipMode): Tbool; cdecl; external libSDL3;
function SDL_DuplicateSurface(surface: PSDL_Surface): PSDL_Surface; cdecl; external libSDL3;
function SDL_ScaleSurface(surface: PSDL_Surface; Width: longint; Height: longint; scaleMode: TSDL_ScaleMode): PSDL_Surface; cdecl; external libSDL3;
function SDL_ConvertSurface(surface: PSDL_Surface; format: TSDL_PixelFormat): PSDL_Surface; cdecl; external libSDL3;
function SDL_ConvertSurfaceAndColorspace(surface: PSDL_Surface; format: TSDL_PixelFormat; palette: PSDL_Palette; colorspace: TSDL_Colorspace; props: TSDL_PropertiesID): PSDL_Surface; cdecl; external libSDL3;
function SDL_ConvertPixels(Width: longint; Height: longint; src_format: TSDL_PixelFormat; src: pointer; src_pitch: longint;
  dst_format: TSDL_PixelFormat; dst: pointer; dst_pitch: longint): Tbool; cdecl; external libSDL3;
function SDL_ConvertPixelsAndColorspace(Width: longint; Height: longint; src_format: TSDL_PixelFormat; src_colorspace: TSDL_Colorspace; src_properties: TSDL_PropertiesID;
  src: pointer; src_pitch: longint; dst_format: TSDL_PixelFormat; dst_colorspace: TSDL_Colorspace; dst_properties: TSDL_PropertiesID;
  dst: pointer; dst_pitch: longint): Tbool; cdecl; external libSDL3;
function SDL_PremultiplyAlpha(Width: longint; Height: longint; src_format: TSDL_PixelFormat; src: pointer; src_pitch: longint;
  dst_format: TSDL_PixelFormat; dst: pointer; dst_pitch: longint; linear: Tbool): Tbool; cdecl; external libSDL3;
function SDL_PremultiplySurfaceAlpha(surface: PSDL_Surface; linear: Tbool): Tbool; cdecl; external libSDL3;
function SDL_ClearSurface(surface: PSDL_Surface; r: single; g: single; b: single; a: single): Tbool; cdecl; external libSDL3;
function SDL_FillSurfaceRect(dst: PSDL_Surface; rect: PSDL_Rect; color: TUint32): Tbool; cdecl; external libSDL3;
function SDL_FillSurfaceRects(dst: PSDL_Surface; rects: PSDL_Rect; Count: longint; color: TUint32): Tbool; cdecl; external libSDL3;
function SDL_BlitSurface(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): Tbool; cdecl; external libSDL3;
function SDL_BlitSurfaceUnchecked(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): Tbool; cdecl; external libSDL3;
function SDL_BlitSurfaceScaled(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect; scaleMode: TSDL_ScaleMode): Tbool; cdecl; external libSDL3;
function SDL_BlitSurfaceUncheckedScaled(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect; scaleMode: TSDL_ScaleMode): Tbool; cdecl; external libSDL3;
function SDL_BlitSurfaceTiled(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): Tbool; cdecl; external libSDL3;
function SDL_BlitSurfaceTiledWithScale(src: PSDL_Surface; srcrect: PSDL_Rect; scale: single; scaleMode: TSDL_ScaleMode; dst: PSDL_Surface;
  dstrect: PSDL_Rect): Tbool; cdecl; external libSDL3;
function SDL_BlitSurface9Grid(src: PSDL_Surface; srcrect: PSDL_Rect; left_width: longint; right_width: longint; top_height: longint;
  bottom_height: longint; scale: single; scaleMode: TSDL_ScaleMode; dst: PSDL_Surface; dstrect: PSDL_Rect): Tbool; cdecl; external libSDL3;
function SDL_MapSurfaceRGB(surface: PSDL_Surface; r: TUint8; g: TUint8; b: TUint8): TUint32; cdecl; external libSDL3;
function SDL_MapSurfaceRGBA(surface: PSDL_Surface; r: TUint8; g: TUint8; b: TUint8; a: TUint8): TUint32; cdecl; external libSDL3;
function SDL_ReadSurfacePixel(surface: PSDL_Surface; x: longint; y: longint; r: PUint8; g: PUint8;
  b: PUint8; a: PUint8): Tbool; cdecl; external libSDL3;
function SDL_ReadSurfacePixelFloat(surface: PSDL_Surface; x: longint; y: longint; r: Psingle; g: Psingle;
  b: Psingle; a: Psingle): Tbool; cdecl; external libSDL3;
function SDL_WriteSurfacePixel(surface: PSDL_Surface; x: longint; y: longint; r: TUint8; g: TUint8;
  b: TUint8; a: TUint8): Tbool; cdecl; external libSDL3;
function SDL_WriteSurfacePixelFloat(surface: PSDL_Surface; x: longint; y: longint; r: single; g: single;
  b: single; a: single): Tbool; cdecl; external libSDL3;

function SDL_MUSTLOCK(S: PSDL_Surface): Boolean;

implementation

function SDL_MUSTLOCK(S: PSDL_Surface): Boolean;
begin
  SDL_MUSTLOCK := ((S^.flags) and SDL_SURFACE_LOCK_NEEDED) = SDL_SURFACE_LOCK_NEEDED;
end;


end.
