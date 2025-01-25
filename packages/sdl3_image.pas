unit SDL3_image;

interface

uses
  SDL3;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

const
  {$IFDEF Linux}
  libSDL3_image = 'libSDL3_image.so';
  {$ENDIF}

  {$IFDEF Windows}
  libSDL3_image = 'SDL3_image.dll';
  {$ENDIF}

  {$IFDEF Darwin}
  libSDL3_image = 'libSDL3_image.dylib';
  {$ENDIF}

const
  SDL_IMAGE_MAJOR_VERSION = 3;
  SDL_IMAGE_MINOR_VERSION = 1;
  SDL_IMAGE_MICRO_VERSION = 1;

function IMG_Version: longint; cdecl; external libSDL3_image;
function IMG_LoadTyped_IO(src: PSDL_IOStream; closeio: Tbool; _type: pchar): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_Load(file_: pchar): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_Load_IO(src: PSDL_IOStream; closeio: Tbool): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_LoadTexture(renderer: PSDL_Renderer; file_: pchar): PSDL_Texture; cdecl; external libSDL3_image;
function IMG_LoadTexture_IO(renderer: PSDL_Renderer; src: PSDL_IOStream; closeio: Tbool): PSDL_Texture; cdecl; external libSDL3_image;
function IMG_LoadTextureTyped_IO(renderer: PSDL_Renderer; src: PSDL_IOStream; closeio: Tbool; _type: pchar): PSDL_Texture; cdecl; external libSDL3_image;
function IMG_isAVIF(src: PSDL_IOStream): Tbool; cdecl; external libSDL3_image;
function IMG_isICO(src: PSDL_IOStream): Tbool; cdecl; external libSDL3_image;
function IMG_isCUR(src: PSDL_IOStream): Tbool; cdecl; external libSDL3_image;
function IMG_isBMP(src: PSDL_IOStream): Tbool; cdecl; external libSDL3_image;
function IMG_isGIF(src: PSDL_IOStream): Tbool; cdecl; external libSDL3_image;
function IMG_isJPG(src: PSDL_IOStream): Tbool; cdecl; external libSDL3_image;
function IMG_isJXL(src: PSDL_IOStream): Tbool; cdecl; external libSDL3_image;
function IMG_isLBM(src: PSDL_IOStream): Tbool; cdecl; external libSDL3_image;
function IMG_isPCX(src: PSDL_IOStream): Tbool; cdecl; external libSDL3_image;
function IMG_isPNG(src: PSDL_IOStream): Tbool; cdecl; external libSDL3_image;
function IMG_isPNM(src: PSDL_IOStream): Tbool; cdecl; external libSDL3_image;
function IMG_isSVG(src: PSDL_IOStream): Tbool; cdecl; external libSDL3_image;
function IMG_isQOI(src: PSDL_IOStream): Tbool; cdecl; external libSDL3_image;
function IMG_isTIF(src: PSDL_IOStream): Tbool; cdecl; external libSDL3_image;
function IMG_isXCF(src: PSDL_IOStream): Tbool; cdecl; external libSDL3_image;
function IMG_isXPM(src: PSDL_IOStream): Tbool; cdecl; external libSDL3_image;
function IMG_isXV(src: PSDL_IOStream): Tbool; cdecl; external libSDL3_image;
function IMG_isWEBP(src: PSDL_IOStream): Tbool; cdecl; external libSDL3_image;

function IMG_LoadAVIF_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_LoadICO_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_LoadCUR_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_LoadBMP_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_LoadGIF_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_LoadJPG_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_LoadJXL_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_LoadLBM_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_LoadPCX_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_LoadPNG_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_LoadPNM_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_LoadSVG_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_LoadQOI_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_LoadTGA_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_LoadTIF_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_LoadXCF_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_LoadXPM_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_LoadXV_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_LoadWEBP_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external libSDL3_image;

function IMG_LoadSizedSVG_IO(src: PSDL_IOStream; Width: longint; Height: longint): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_ReadXPMFromArray(xpm: PPchar): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_ReadXPMFromArrayToRGB888(xpm: PPchar): PSDL_Surface; cdecl; external libSDL3_image;
function IMG_SaveAVIF(surface: PSDL_Surface; file_: pchar; quality: longint): Tbool; cdecl; external libSDL3_image;
function IMG_SaveAVIF_IO(surface: PSDL_Surface; dst: PSDL_IOStream; closeio: Tbool; quality: longint): Tbool; cdecl; external libSDL3_image;
function IMG_SavePNG(surface: PSDL_Surface; file_: pchar): Tbool; cdecl; external libSDL3_image;
function IMG_SavePNG_IO(surface: PSDL_Surface; dst: PSDL_IOStream; closeio: Tbool): Tbool; cdecl; external libSDL3_image;
function IMG_SaveJPG(surface: PSDL_Surface; file_: pchar; quality: longint): Tbool; cdecl; external libSDL3_image;
function IMG_SaveJPG_IO(surface: PSDL_Surface; dst: PSDL_IOStream; closeio: Tbool; quality: longint): Tbool; cdecl; external libSDL3_image;

type
  TIMG_Animation = record
    w: longint;
    h: longint;
    Count: longint;
    frames: ^PSDL_Surface;
    delays: Plongint;
  end;
  PIMG_Animation = ^TIMG_Animation;

function IMG_LoadAnimation(file_: pchar): PIMG_Animation; cdecl; external libSDL3_image;
function IMG_LoadAnimation_IO(src: PSDL_IOStream; closeio: Tbool): PIMG_Animation; cdecl; external libSDL3_image;
function IMG_LoadAnimationTyped_IO(src: PSDL_IOStream; closeio: Tbool; _type: pchar): PIMG_Animation; cdecl; external libSDL3_image;
procedure IMG_FreeAnimation(anim: PIMG_Animation); cdecl; external libSDL3_image;
function IMG_LoadGIFAnimation_IO(src: PSDL_IOStream): PIMG_Animation; cdecl; external libSDL3_image;
function IMG_LoadWEBPAnimation_IO(src: PSDL_IOStream): PIMG_Animation; cdecl; external libSDL3_image;

function SDL_IMAGE_VERSION: longint;
function SDL_IMAGE_VERSION_ATLEAST(X, Y, Z: longint): TSDL_bool;


implementation

function SDL_IMAGE_VERSION: longint;
begin
  SDL_IMAGE_VERSION := SDL_VERSIONNUM(SDL_IMAGE_MAJOR_VERSION, SDL_IMAGE_MINOR_VERSION, SDL_IMAGE_MICRO_VERSION);
end;


function SDL_IMAGE_VERSION_ATLEAST(X, Y, Z: longint): TSDL_bool;
begin
  SDL_IMAGE_VERSION_ATLEAST :=
    (SDL_IMAGE_MAJOR_VERSION >= X) and
    ((SDL_IMAGE_MAJOR_VERSION > X) or (SDL_IMAGE_MINOR_VERSION >= Y)) and
    ((SDL_IMAGE_MAJOR_VERSION > X) or (SDL_IMAGE_MINOR_VERSION > Y) or (SDL_IMAGE_MICRO_VERSION >= Z));
end;

end.
