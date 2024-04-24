unit SDL_rtf;

interface

uses
  SDL3;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

const
  {$IFDEF Linux}
  sdl3_rtf_lib = 'libSDL3_rtf.so';
  {$ENDIF}

  {$IFDEF Windows}
  sdl3_rtf_lib = 'SDL3_rtf.dll';
  {$ENDIF}


const
  SDL_RTF_MAJOR_VERSION = 3;
  SDL_RTF_MINOR_VERSION = 0;
  SDL_RTF_PATCHLEVEL = 0;

  RTF_MAJOR_VERSION = SDL_RTF_MAJOR_VERSION;
  RTF_MINOR_VERSION = SDL_RTF_MINOR_VERSION;
  RTF_PATCHLEVEL = SDL_RTF_PATCHLEVEL;

procedure SDL_RTF_VERSION(X: PSDL_Version);
procedure RTF_VERSION(X: PSDL_Version);

type
  TRTF_Context = Pointer;
  PRTF_Context = ^TRTF_Context;

function RTF_Linked_Version: PSDL_Version; cdecl; external sdl3_rtf_lib;

type

  PRTF_FontFamily = ^TRTF_FontFamily;
  TRTF_FontFamily = longint;

const
  RTF_FontDefault = 0;
  RTF_FontRoman = 1;
  RTF_FontSwiss = 2;
  RTF_FontModern = 3;
  RTF_FontScript = 4;
  RTF_FontDecor = 5;
  RTF_FontTech = 6;
  RTF_FontBidi = 7;

type
  PRTF_FontStyle = ^TRTF_FontStyle;
  TRTF_FontStyle = longint;

const
  RTF_FontNormal = $00;
  RTF_FontBold = $01;
  RTF_FontItalic = $02;
  RTF_FontUnderline = $04;

  RTF_FONT_ENGINE_VERSION = 1;

type
  PRTF_FontEngine = ^TRTF_FontEngine;

  TRTF_FontEngine = record
    version: longint;
    CreateFont: function(Name: PChar; family: TRTF_FontFamily; charset: longint; size: longint; style: longint): pointer; cdecl;
    GetLineSpacing: function(font: pointer): longint; cdecl;
    GetCharacterOffsets: function(font: pointer; Text: PChar; byteOffsets: Plongint; pixelOffsets: Plongint; maxOffsets: longint): longint; cdecl;
    RenderText: function(font: pointer; renderer: PSDL_Renderer; Text: PChar; fg: TSDL_Color): PSDL_Texture; cdecl;
    FreeFont: procedure(font: pointer); cdecl;
  end;

function RTF_CreateContext(renderer: PSDL_Renderer; fontEngine: PRTF_FontEngine): PRTF_Context; cdecl; external sdl3_rtf_lib;
function RTF_Load(ctx: PRTF_Context; file_: PChar): longint; cdecl; external sdl3_rtf_lib;
function RTF_Load_IO(ctx: PRTF_Context; src: PSDL_IOStream; closeio: longint): longint; cdecl; external sdl3_rtf_lib;
function RTF_GetTitle(ctx: PRTF_Context): PChar; cdecl; external sdl3_rtf_lib;
function RTF_GetSubject(ctx: PRTF_Context): PChar; cdecl; external sdl3_rtf_lib;
function RTF_GetAuthor(ctx: PRTF_Context): PChar; cdecl; external sdl3_rtf_lib;
function RTF_GetHeight(ctx: PRTF_Context; Width: longint): longint; cdecl; external sdl3_rtf_lib;
procedure RTF_Render(ctx: PRTF_Context; rect: PSDL_Rect; yOffset: longint); cdecl; external sdl3_rtf_lib;
procedure RTF_FreeContext(ctx: PRTF_Context); cdecl; external sdl3_rtf_lib;

function RTF_SetError(fmt: PChar): longint; varargs; cdecl; external sdl3_lib Name 'SDL_SetError';
function RTF_GetError: PChar; cdecl; external sdl3_lib Name 'SDL_GetError';

implementation

procedure SDL_RTF_VERSION(X: PSDL_Version);
begin
  X^.major := SDL_RTF_MAJOR_VERSION;
  X^.minor := SDL_RTF_MINOR_VERSION;
  X^.patch := SDL_RTF_PATCHLEVEL;
end;

procedure RTF_VERSION(X: PSDL_Version);
begin
  SDL_RTF_VERSION(X);
end;

end.
