unit SDL3_ttf;

interface

uses
  SDL3;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

const
  {$IFDEF Linux}
  libSLD3_TTF = 'libSDL3_ttf.so';
  {$ENDIF}

  {$IFDEF Windows}
  libSLD3_TTF = 'SDL3_ttf.dll';
  {$ENDIF}

  {$IFDEF Darwin}
  libSLD3_TTF = 'libSDL3_ttf.dylib';
  {$ENDIF}

type
  TTTF_Font = Pointer;
  PTTF_Font = ^TTTF_Font;


const
  SDL_TTF_MAJOR_VERSION = 3;
  SDL_TTF_MINOR_VERSION = 0;
  SDL_TTF_PATCHLEVEL = 0;

  TTF_MAJOR_VERSION = SDL_TTF_MAJOR_VERSION;
  TTF_MINOR_VERSION = SDL_TTF_MINOR_VERSION;
  TTF_PATCHLEVEL = SDL_TTF_PATCHLEVEL;

//procedure SDL_TTF_VERSION(X: PSDL_Version);
//procedure TTF_VERSION(X: PSDL_Version);

function SDL_TTF_VERSION_ATLEAST(X, Y, Z: longint): TSDL_bool;

//function TTF_Linked_Version: PSDL_Version; cdecl; external sdl3_ttf_lib;
procedure TTF_GetFreeTypeVersion(major: Plongint; minor: Plongint; patch: Plongint); cdecl; external libSLD3_TTF;
procedure TTF_GetHarfBuzzVersion(major: Plongint; minor: Plongint; patch: Plongint); cdecl; external libSLD3_TTF;

const
  UNICODE_BOM_NATIVE = $FEFF;
  UNICODE_BOM_SWAPPED = $FFFE;

procedure TTF_ByteSwappedUNICODE(swapped: TSDL_bool); cdecl; external libSLD3_TTF;
function TTF_Init: longint; cdecl; external libSLD3_TTF;
function TTF_OpenFont(file_: PChar; ptsize: longint): PTTF_Font; cdecl; external libSLD3_TTF;
function TTF_OpenFontIndex(file_: PChar; ptsize: longint; index: longint): PTTF_Font; cdecl; external libSLD3_TTF;
function TTF_OpenFontIO(src: PSDL_IOStream; closeio: TSDL_bool; ptsize: longint): PTTF_Font; cdecl; external libSLD3_TTF;
function TTF_OpenFontIndexIO(src: PSDL_IOStream; closeio: TSDL_bool; ptsize: longint; index: longint): PTTF_Font; cdecl; external libSLD3_TTF;
function TTF_OpenFontDPI(file_: PChar; ptsize: longint; hdpi: dword; vdpi: dword): PTTF_Font; cdecl; external libSLD3_TTF;
function TTF_OpenFontIndexDPI(file_: PChar; ptsize: longint; index: longint; hdpi: dword; vdpi: dword): PTTF_Font; cdecl; external libSLD3_TTF;
function TTF_OpenFontDPIIO(src: PSDL_IOStream; closeio: TSDL_bool; ptsize: longint; hdpi: dword; vdpi: dword): PTTF_Font; cdecl; external libSLD3_TTF;
function TTF_OpenFontIndexDPIIO(src: PSDL_IOStream; closeio: TSDL_bool; ptsize: longint; index: longint; hdpi: dword;
  vdpi: dword): PTTF_Font; cdecl; external libSLD3_TTF;
function TTF_SetFontSize(font: PTTF_Font; ptsize: longint): longint; cdecl; external libSLD3_TTF;
function TTF_SetFontSizeDPI(font: PTTF_Font; ptsize: longint; hdpi: dword; vdpi: dword): longint; cdecl; external libSLD3_TTF;

const
  TTF_STYLE_NORMAL = $00;
  TTF_STYLE_BOLD = $01;
  TTF_STYLE_ITALIC = $02;
  TTF_STYLE_UNDERLINE = $04;
  TTF_STYLE_STRIKETHROUGH = $08;

function TTF_GetFontStyle(font: PTTF_Font): longint; cdecl; external libSLD3_TTF;
procedure TTF_SetFontStyle(font: PTTF_Font; style: longint); cdecl; external libSLD3_TTF;
function TTF_GetFontOutline(font: PTTF_Font): longint; cdecl; external libSLD3_TTF;
procedure TTF_SetFontOutline(font: PTTF_Font; outline: longint); cdecl; external libSLD3_TTF;

const
  TTF_HINTING_NORMAL = 0;
  TTF_HINTING_LIGHT = 1;
  TTF_HINTING_MONO = 2;
  TTF_HINTING_NONE = 3;
  TTF_HINTING_LIGHT_SUBPIXEL = 4;

function TTF_GetFontHinting(font: PTTF_Font): longint; cdecl; external libSLD3_TTF;
procedure TTF_SetFontHinting(font: PTTF_Font; hinting: longint); cdecl; external libSLD3_TTF;

const
  TTF_WRAPPED_ALIGN_LEFT = 0;
  TTF_WRAPPED_ALIGN_CENTER = 1;
  TTF_WRAPPED_ALIGN_RIGHT = 2;

function TTF_GetFontWrappedAlign(font: PTTF_Font): longint; cdecl; external libSLD3_TTF;
procedure TTF_SetFontWrappedAlign(font: PTTF_Font; align: longint); cdecl; external libSLD3_TTF;
function TTF_FontHeight(font: PTTF_Font): longint; cdecl; external libSLD3_TTF;
function TTF_FontAscent(font: PTTF_Font): longint; cdecl; external libSLD3_TTF;
function TTF_FontDescent(font: PTTF_Font): longint; cdecl; external libSLD3_TTF;
function TTF_FontLineSkip(font: PTTF_Font): longint; cdecl; external libSLD3_TTF;
function TTF_GetFontKerning(font: PTTF_Font): longint; cdecl; external libSLD3_TTF;
procedure TTF_SetFontKerning(font: PTTF_Font; allowed: longint); cdecl; external libSLD3_TTF;
function TTF_FontFaces(font: PTTF_Font): longint; cdecl; external libSLD3_TTF;
function TTF_FontFaceIsFixedWidth(font: PTTF_Font): longint; cdecl; external libSLD3_TTF;
function TTF_FontFaceFamilyName(font: PTTF_Font): PChar; cdecl; external libSLD3_TTF;
function TTF_FontFaceStyleName(font: PTTF_Font): PChar; cdecl; external libSLD3_TTF;
function TTF_GlyphIsProvided(font: PTTF_Font; ch: TUint16): longint; cdecl; external libSLD3_TTF;
function TTF_GlyphIsProvided32(font: PTTF_Font; ch: TUint32): longint; cdecl; external libSLD3_TTF;
function TTF_GlyphMetrics(font: PTTF_Font; ch: TUint16; minx: Plongint; maxx: Plongint; miny: Plongint;
  maxy: Plongint; advance: Plongint): longint; cdecl; external libSLD3_TTF;
function TTF_GlyphMetrics32(font: PTTF_Font; ch: TUint32; minx: Plongint; maxx: Plongint; miny: Plongint;
  maxy: Plongint; advance: Plongint): longint; cdecl; external libSLD3_TTF;
function TTF_SizeText(font: PTTF_Font; Text: PChar; w: Plongint; h: Plongint): longint; cdecl; external libSLD3_TTF;
function TTF_SizeUTF8(font: PTTF_Font; Text: PChar; w: Plongint; h: Plongint): longint; cdecl; external libSLD3_TTF;
function TTF_SizeUNICODE(font: PTTF_Font; Text: PUint16; w: Plongint; h: Plongint): longint; cdecl; external libSLD3_TTF;
function TTF_MeasureText(font: PTTF_Font; Text: PChar; measure_width: longint; extent: Plongint; Count: Plongint): longint; cdecl; external libSLD3_TTF;
function TTF_MeasureUTF8(font: PTTF_Font; Text: PChar; measure_width: longint; extent: Plongint; Count: Plongint): longint; cdecl; external libSLD3_TTF;
function TTF_MeasureUNICODE(font: PTTF_Font; Text: PUint16; measure_width: longint; extent: Plongint; Count: Plongint): longint; cdecl; external libSLD3_TTF;
function TTF_RenderText_Solid(font: PTTF_Font; Text: PChar; fg: TSDL_Color): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderUTF8_Solid(font: PTTF_Font; Text: PChar; fg: TSDL_Color): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderUNICODE_Solid(font: PTTF_Font; Text: PUint16; fg: TSDL_Color): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderText_Solid_Wrapped(font: PTTF_Font; Text: PChar; fg: TSDL_Color; wrapLength: TUint32): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderUTF8_Solid_Wrapped(font: PTTF_Font; Text: PChar; fg: TSDL_Color; wrapLength: TUint32): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderUNICODE_Solid_Wrapped(font: PTTF_Font; Text: PUint16; fg: TSDL_Color; wrapLength: TUint32): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderGlyph_Solid(font: PTTF_Font; ch: TUint16; fg: TSDL_Color): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderGlyph32_Solid(font: PTTF_Font; ch: TUint32; fg: TSDL_Color): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderText_Shaded(font: PTTF_Font; Text: PChar; fg: TSDL_Color; bg: TSDL_Color): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderUTF8_Shaded(font: PTTF_Font; Text: PChar; fg: TSDL_Color; bg: TSDL_Color): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderUNICODE_Shaded(font: PTTF_Font; Text: PUint16; fg: TSDL_Color; bg: TSDL_Color): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderText_Shaded_Wrapped(font: PTTF_Font; Text: PChar; fg: TSDL_Color; bg: TSDL_Color; wrapLength: TUint32): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderUTF8_Shaded_Wrapped(font: PTTF_Font; Text: PChar; fg: TSDL_Color; bg: TSDL_Color; wrapLength: TUint32): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderUNICODE_Shaded_Wrapped(font: PTTF_Font; Text: PUint16; fg: TSDL_Color; bg: TSDL_Color; wrapLength: TUint32): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderGlyph_Shaded(font: PTTF_Font; ch: TUint16; fg: TSDL_Color; bg: TSDL_Color): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderGlyph32_Shaded(font: PTTF_Font; ch: TUint32; fg: TSDL_Color; bg: TSDL_Color): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderText_Blended(font: PTTF_Font; Text: PChar; fg: TSDL_Color): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderUTF8_Blended(font: PTTF_Font; Text: PChar; fg: TSDL_Color): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderUNICODE_Blended(font: PTTF_Font; Text: PUint16; fg: TSDL_Color): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderText_Blended_Wrapped(font: PTTF_Font; Text: PChar; fg: TSDL_Color; wrapLength: TUint32): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderUTF8_Blended_Wrapped(font: PTTF_Font; Text: PChar; fg: TSDL_Color; wrapLength: TUint32): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderUNICODE_Blended_Wrapped(font: PTTF_Font; Text: PUint16; fg: TSDL_Color; wrapLength: TUint32): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderGlyph_Blended(font: PTTF_Font; ch: TUint16; fg: TSDL_Color): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderGlyph32_Blended(font: PTTF_Font; ch: TUint32; fg: TSDL_Color): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderText_LCD(font: PTTF_Font; Text: PChar; fg: TSDL_Color; bg: TSDL_Color): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderUTF8_LCD(font: PTTF_Font; Text: PChar; fg: TSDL_Color; bg: TSDL_Color): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderUNICODE_LCD(font: PTTF_Font; Text: PUint16; fg: TSDL_Color; bg: TSDL_Color): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderText_LCD_Wrapped(font: PTTF_Font; Text: PChar; fg: TSDL_Color; bg: TSDL_Color; wrapLength: TUint32): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderUTF8_LCD_Wrapped(font: PTTF_Font; Text: PChar; fg: TSDL_Color; bg: TSDL_Color; wrapLength: TUint32): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderUNICODE_LCD_Wrapped(font: PTTF_Font; Text: PUint16; fg: TSDL_Color; bg: TSDL_Color; wrapLength: TUint32): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderGlyph_LCD(font: PTTF_Font; ch: TUint16; fg: TSDL_Color; bg: TSDL_Color): PSDL_Surface; cdecl; external libSLD3_TTF;
function TTF_RenderGlyph32_LCD(font: PTTF_Font; ch: TUint32; fg: TSDL_Color; bg: TSDL_Color): PSDL_Surface; cdecl; external libSLD3_TTF;

function TTF_RenderText(font: PTTF_Font; Text: PChar; fg: TSDL_Color; bg: TSDL_Color): PSDL_Surface;
function TTF_RenderUTF8(font: PTTF_Font; Text: PChar; fg: TSDL_Color; bg: TSDL_Color): PSDL_Surface;
function TTF_RenderUNICODE(font: PTTF_Font; Text: PUint16; fg: TSDL_Color; bg: TSDL_Color): PSDL_Surface;

procedure TTF_CloseFont(font: PTTF_Font); cdecl; external libSLD3_TTF;
procedure TTF_Quit; cdecl; external libSLD3_TTF;
function TTF_WasInit: longint; cdecl; external libSLD3_TTF;
function TTF_GetFontKerningSizeGlyphs(font: PTTF_Font; previous_ch: TUint16; ch: TUint16): longint; cdecl; external libSLD3_TTF;
function TTF_GetFontKerningSizeGlyphs32(font: PTTF_Font; previous_ch: TUint32; ch: TUint32): longint; cdecl; external libSLD3_TTF;
function TTF_SetFontSDF(font: PTTF_Font; on_off: TSDL_bool): longint; cdecl; external libSLD3_TTF;
function TTF_GetFontSDF(font: PTTF_Font): TSDL_bool; cdecl; external libSLD3_TTF;

function TTF_SetError(fmt: PChar): longint; varargs; cdecl; external libSDL3 Name 'SDL_SetError';
function TTF_GetError: PChar; cdecl; external libSDL3 Name 'SDL_GetError';

type
  PTTF_Direction = ^TTTF_Direction;
  TTTF_Direction = longint;

const
  TTF_DIRECTION_LTR = 0;
  TTF_DIRECTION_RTL = 1;
  TTF_DIRECTION_TTB = 2;
  TTF_DIRECTION_BTT = 3;

function TTF_SetFontDirection(font: PTTF_Font; direction: TTTF_Direction): longint; cdecl; external libSLD3_TTF;
function TTF_SetFontScriptName(font: PTTF_Font; script: PChar): longint; cdecl; external libSLD3_TTF;
function TTF_SetFontLanguage(font: PTTF_Font; language_bcp47: PChar): longint; cdecl; external libSLD3_TTF;
function TTF_IsFontScalable(font: PTTF_Font): TSDL_bool; cdecl; external libSLD3_TTF;

implementation

//procedure SDL_TTF_VERSION(X: PSDL_Version);
//begin
//  X^.major := SDL_TTF_MAJOR_VERSION;
//  X^.minor := SDL_TTF_MINOR_VERSION;
//  X^.patch := SDL_TTF_PATCHLEVEL;
//end;
//
//procedure TTF_VERSION(X: PSDL_Version);
//begin
//  SDL_TTF_VERSION(X);
//end;

function SDL_TTF_VERSION_ATLEAST(X, Y, Z: longint): TSDL_bool;
begin
  SDL_TTF_VERSION_ATLEAST :=
    (SDL_TTF_MAJOR_VERSION >= X) and
    ((SDL_TTF_MAJOR_VERSION > X) or (SDL_TTF_MINOR_VERSION >= Y)) and
    ((SDL_TTF_MAJOR_VERSION > X) or (SDL_TTF_MINOR_VERSION > Y) or (SDL_TTF_PATCHLEVEL >= Z));
end;

function TTF_RenderText(font: PTTF_Font; Text: PChar; fg: TSDL_Color; bg: TSDL_Color): PSDL_Surface;
begin
  TTF_RenderText := TTF_RenderText_Shaded(font, Text, fg, bg);
end;

function TTF_RenderUTF8(font: PTTF_Font; Text: PChar; fg: TSDL_Color; bg: TSDL_Color): PSDL_Surface;
begin
  TTF_RenderUTF8 := TTF_RenderUTF8_Shaded(font, Text, fg, bg);
end;

function TTF_RenderUNICODE(font: PTTF_Font; Text: PUint16; fg: TSDL_Color; bg: TSDL_Color): PSDL_Surface;
begin
  TTF_RenderUNICODE := TTF_RenderUNICODE_Shaded(font, Text, fg, bg);
end;

end.
