unit SDL3_image;

interface

uses
  SDL3;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

const
  {$IFDEF Linux}
  sdl3_image_lib = 'libSDL3_image.so';
  {$ENDIF}

  {$IFDEF Windows}
  sdl3_image_lib = 'SDL3_image.dll';
  {$ENDIF}

const
  SDL_IMAGE_MAJOR_VERSION = 3;
  SDL_IMAGE_MINOR_VERSION = 0;
  SDL_IMAGE_PATCHLEVEL = 0;

//procedure SDL_IMAGE_VERSION(X: PSDL_Version);

function SDL_IMAGE_COMPILEDVERSION: longint; { return type might be wrong }
function SDL_IMAGE_VERSION_ATLEAST(X, Y, Z: longint): TSDL_bool;

//function IMG_Linked_Version: PSDL_Version; cdecl; external sdl3_lib;

type
  PIMG_InitFlags = ^TIMG_InitFlags;
  TIMG_InitFlags = longint;

const
  IMG_INIT_JPG = $00000001;
  IMG_INIT_PNG = $00000002;
  IMG_INIT_TIF = $00000004;
  IMG_INIT_WEBP = $00000008;
  IMG_INIT_JXL = $00000010;
  IMG_INIT_AVIF = $00000020;

function IMG_Init(flags: longint): longint; cdecl; external sdl3_image_lib;
procedure IMG_Quit; cdecl; external sdl3_image_lib;
function IMG_LoadTyped_IO(src: PSDL_IOStream; closeio: TSDL_bool; _type: PChar): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_Load(file_: PChar): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_Load_IO(src: PSDL_IOStream; closeio: TSDL_bool): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_LoadTexture(renderer: PSDL_Renderer; file_: PChar): PSDL_Texture; cdecl; external sdl3_image_lib;
function IMG_LoadTexture_IO(renderer: PSDL_Renderer; src: PSDL_IOStream; closeio: TSDL_bool): PSDL_Texture; cdecl; external sdl3_image_lib;
function IMG_LoadTextureTyped_IO(renderer: PSDL_Renderer; src: PSDL_IOStream; closeio: TSDL_bool; _type: PChar): PSDL_Texture; cdecl; external sdl3_image_lib;
function IMG_isAVIF(src: PSDL_IOStream): longint; cdecl; external sdl3_image_lib;
function IMG_isICO(src: PSDL_IOStream): longint; cdecl; external sdl3_image_lib;
function IMG_isCUR(src: PSDL_IOStream): longint; cdecl; external sdl3_image_lib;
function IMG_isBMP(src: PSDL_IOStream): longint; cdecl; external sdl3_image_lib;
function IMG_isGIF(src: PSDL_IOStream): longint; cdecl; external sdl3_image_lib;
function IMG_isJPG(src: PSDL_IOStream): longint; cdecl; external sdl3_image_lib;
function IMG_isJXL(src: PSDL_IOStream): longint; cdecl; external sdl3_image_lib;
function IMG_isLBM(src: PSDL_IOStream): longint; cdecl; external sdl3_image_lib;
function IMG_isPCX(src: PSDL_IOStream): longint; cdecl; external sdl3_image_lib;
function IMG_isPNG(src: PSDL_IOStream): longint; cdecl; external sdl3_image_lib;
function IMG_isPNM(src: PSDL_IOStream): longint; cdecl; external sdl3_image_lib;
function IMG_isSVG(src: PSDL_IOStream): longint; cdecl; external sdl3_image_lib;
function IMG_isQOI(src: PSDL_IOStream): longint; cdecl; external sdl3_image_lib;
function IMG_isTIF(src: PSDL_IOStream): longint; cdecl; external sdl3_image_lib;
function IMG_isXCF(src: PSDL_IOStream): longint; cdecl; external sdl3_image_lib;
function IMG_isXPM(src: PSDL_IOStream): longint; cdecl; external sdl3_image_lib;
function IMG_isXV(src: PSDL_IOStream): longint; cdecl; external sdl3_image_lib;
function IMG_isWEBP(src: PSDL_IOStream): longint; cdecl; external sdl3_image_lib;
function IMG_LoadAVIF_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_LoadICO_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_LoadCUR_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_LoadBMP_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_LoadGIF_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_LoadJPG_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_LoadJXL_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_LoadLBM_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_LoadPCX_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_LoadPNG_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_LoadPNM_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_LoadSVG_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_LoadQOI_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_LoadTGA_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_LoadTIF_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_LoadXCF_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_LoadXPM_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_LoadXV_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_LoadWEBP_IO(src: PSDL_IOStream): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_LoadSizedSVG_IO(src: PSDL_IOStream; Width: longint; Height: longint): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_ReadXPMFromArray(xpm: PPchar): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_ReadXPMFromArrayToRGB888(xpm: PPchar): PSDL_Surface; cdecl; external sdl3_image_lib;
function IMG_SaveAVIF(surface: PSDL_Surface; file_: PChar; quality: longint): longint; cdecl; external sdl3_image_lib;
function IMG_SaveAVIF_IO(surface: PSDL_Surface; dst: PSDL_IOStream; closeio: longint; quality: longint): longint; cdecl; external sdl3_image_lib;
function IMG_SavePNG(surface: PSDL_Surface; file_: PChar): longint; cdecl; external sdl3_image_lib;
function IMG_SavePNG_IO(surface: PSDL_Surface; dst: PSDL_IOStream; closeio: longint): longint; cdecl; external sdl3_image_lib;
function IMG_SaveJPG(surface: PSDL_Surface; file_: PChar; quality: longint): longint; cdecl; external sdl3_image_lib;
function IMG_SaveJPG_IO(surface: PSDL_Surface; dst: PSDL_IOStream; closeio: longint; quality: longint): longint; cdecl; external sdl3_image_lib;

type
  PIMG_Animation = ^TIMG_Animation;

  TIMG_Animation = record
    w: longint;
    h: longint;
    Count: longint;
    frames: ^PSDL_Surface;
    delays: Plongint;
  end;

function IMG_LoadAnimation(file_: PChar): PIMG_Animation; cdecl; external sdl3_image_lib;
function IMG_LoadAnimation_IO(src: PSDL_IOStream; closeio: TSDL_bool): PIMG_Animation; cdecl; external sdl3_image_lib;
function IMG_LoadAnimationTyped_IO(src: PSDL_IOStream; closeio: TSDL_bool; _type: PChar): PIMG_Animation; cdecl; external sdl3_image_lib;
procedure IMG_FreeAnimation(anim: PIMG_Animation); cdecl; external sdl3_image_lib;
function IMG_LoadGIFAnimation_IO(src: PSDL_IOStream): PIMG_Animation; cdecl; external sdl3_image_lib;
function IMG_LoadWEBPAnimation_IO(src: PSDL_IOStream): PIMG_Animation; cdecl; external sdl3_image_lib;
//const
//  IMG_SetError = SDL_SetError;  
//IMG_GetError = SDL_GetError;
function IMG_SetError(fmt: PChar): longint; varargs; cdecl; external libSDL3 Name 'SDL_SetError';
function IMG_GetError: PChar; cdecl; external libSDL3 Name 'SDL_GetError';

implementation

//procedure SDL_IMAGE_VERSION(X: PSDL_Version);
//begin
//  X^.major := SDL_IMAGE_MAJOR_VERSION;
//  X^.minor := SDL_IMAGE_MINOR_VERSION;
//  X^.patch := SDL_IMAGE_PATCHLEVEL;
//end;

function SDL_IMAGE_COMPILEDVERSION: longint;
begin
  SDL_IMAGE_COMPILEDVERSION := SDL_VERSIONNUM(SDL_IMAGE_MAJOR_VERSION, SDL_IMAGE_MINOR_VERSION, SDL_IMAGE_PATCHLEVEL);
end;

function SDL_IMAGE_VERSION_ATLEAST(X, Y, Z: longint): TSDL_bool;
begin
  SDL_IMAGE_VERSION_ATLEAST :=
    (SDL_IMAGE_MAJOR_VERSION >= X) and
    ((SDL_IMAGE_MAJOR_VERSION > X) or (SDL_IMAGE_MINOR_VERSION >= Y)) and
    ((SDL_IMAGE_MAJOR_VERSION > X) or (SDL_IMAGE_MINOR_VERSION > Y) or (SDL_IMAGE_PATCHLEVEL >= Z));
end;

end.
