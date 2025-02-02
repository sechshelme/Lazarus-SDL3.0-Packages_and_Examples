unit SDL3_ttf;

interface

uses
  SDL3;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

const
  {$IFDEF Linux}
  libSDL3_TTF = 'libSDL3_ttf.so';
  {$ENDIF}

  {$IFDEF Windows}
  libSDL3_TTF = 'SDL3_ttf.dll';
  {$ENDIF}

  {$IFDEF Darwin}
  libSDL3_TTF = 'libSDL3_ttf.dylib';
  {$ENDIF}

type
  PTTF_Font = ^TTTF_Font;
  TTTF_Font = record
  end;

  PTTF_SubStringFlags = ^TTTF_SubStringFlags;
  TTTF_SubStringFlags = TUint32;

const
  TTF_SUBSTRING_DIRECTION_MASK = $000000FF;
  TTF_SUBSTRING_TEXT_START = $00000100;
  TTF_SUBSTRING_LINE_START = $00000200;
  TTF_SUBSTRING_LINE_END = $00000400;
  TTF_SUBSTRING_TEXT_END = $00000800;

type
  TTTF_SubString = record
    flags: TTTF_SubStringFlags;
    offset: longint;
    length: longint;
    line_index: longint;
    cluster_index: longint;
    rect: TSDL_Rect;
  end;
  PTTF_SubString = ^TTTF_SubString;
  PPTTF_SubString = ^PTTF_SubString;

type
  PTTF_DrawCommand = ^TTTF_DrawCommand;
  TTTF_DrawCommand = longint;

  // ===== begin - SDL_textenfgine.h =====
const
  TTF_DRAW_COMMAND_NOOP = 0;
  TTF_DRAW_COMMAND_FILL = 1;
  TTF_DRAW_COMMAND_COPY = 2;

type
  PTTF_TextData = ^TTTF_TextData;
  // ===== end - SDL_textenfgine.h =====

  TTTF_Text = record
    Text: pchar;
    num_lines: longint;
    refcount: longint;
    internal: PTTF_TextData;
  end;
  PTTF_Text = ^TTTF_Text;

  // ===== begin - SDL_textenfgine.h =====

  TTTF_TextLayout = record
  end;
  PTTF_TextLayout = ^TTTF_TextLayout;

  TTTF_FillOperation = record
    cmd: TTTF_DrawCommand;
    rect: TSDL_Rect;
  end;
  PTTF_FillOperation = ^TTTF_FillOperation;

  TTTF_CopyOperation = record
    cmd: TTTF_DrawCommand;
    text_offset: longint;
    glyph_font: PTTF_Font;
    glyph_index: TUint32;
    src: TSDL_Rect;
    dst: TSDL_Rect;
    reserved: pointer;
  end;
  PTTF_CopyOperation = ^TTTF_CopyOperation;

  TTTF_DrawOperation = record
    case longint of
      0: (cmd: TTTF_DrawCommand);
      1: (fill: TTTF_FillOperation);
      2: (copy: TTTF_CopyOperation);
  end;
  PTTF_DrawOperation = ^TTTF_DrawOperation;

  TTTF_TextEngine = record
    version: TUint32;
    userdata: pointer;
    CreateText: function(userdata: pointer; Text: PTTF_Text): Tbool; cdecl;
    DestroyText: procedure(userdata: pointer; Text: PTTF_Text); cdecl;
  end;
  PTTF_TextEngine = ^TTTF_TextEngine;

  TTTF_TextData = record
    font: PTTF_Font;
    color: TSDL_FColor;
    needs_layout_update: Tbool;
    layout: PTTF_TextLayout;
    x: longint;
    y: longint;
    w: longint;
    h: longint;
    num_ops: longint;
    ops: PTTF_DrawOperation;
    num_clusters: longint;
    clusters: PTTF_SubString;
    props: TSDL_PropertiesID;
    needs_engine_update: Tbool;
    engine: PTTF_TextEngine;
    engine_text: pointer;
  end;

  // ===== end - SDL_textenfgine.h =====

const
  SDL_TTF_MAJOR_VERSION = 3;
  SDL_TTF_MINOR_VERSION = 1;
  SDL_TTF_MICRO_VERSION = 0;

function TTF_Version: longint; cdecl; external libSDL3_ttf;
procedure TTF_GetFreeTypeVersion(major: Plongint; minor: Plongint; patch: Plongint); cdecl; external libSDL3_ttf;
procedure TTF_GetHarfBuzzVersion(major: Plongint; minor: Plongint; patch: Plongint); cdecl; external libSDL3_ttf;

function TTF_Init: Tbool; cdecl; external libSDL3_ttf;
function TTF_OpenFont(file_: pchar; ptsize: single): PTTF_Font; cdecl; external libSDL3_ttf;
function TTF_OpenFontIO(src: PSDL_IOStream; closeio: Tbool; ptsize: single): PTTF_Font; cdecl; external libSDL3_ttf;
function TTF_OpenFontWithProperties(props: TSDL_PropertiesID): PTTF_Font; cdecl; external libSDL3_ttf;

const
  TTF_PROP_FONT_CREATE_FILENAME_STRING = 'SDL_ttf.font.create.filename';
  TTF_PROP_FONT_CREATE_IOSTREAM_POINTER = 'SDL_ttf.font.create.iostream';
  TTF_PROP_FONT_CREATE_IOSTREAM_OFFSET_NUMBER = 'SDL_ttf.font.create.iostream.offset';
  TTF_PROP_FONT_CREATE_IOSTREAM_AUTOCLOSE_BOOLEAN = 'SDL_ttf.font.create.iostream.autoclose';
  TTF_PROP_FONT_CREATE_SIZE_FLOAT = 'SDL_ttf.font.create.size';
  TTF_PROP_FONT_CREATE_FACE_NUMBER = 'SDL_ttf.font.create.face';
  TTF_PROP_FONT_CREATE_HORIZONTAL_DPI_NUMBER = 'SDL_ttf.font.create.hdpi';
  TTF_PROP_FONT_CREATE_VERTICAL_DPI_NUMBER = 'SDL_ttf.font.create.vdpi';
  TTF_PROP_FONT_CREATE_EXISTING_FONT = 'SDL_ttf.font.create.existing_font';

function TTF_CopyFont(existing_font: PTTF_Font): PTTF_Font; cdecl; external libSDL3_ttf;
function TTF_GetFontProperties(font: PTTF_Font): TSDL_PropertiesID; cdecl; external libSDL3_ttf;

const
  TTF_PROP_FONT_OUTLINE_LINE_CAP_NUMBER = 'SDL_ttf.font.outline.line_cap';
  TTF_PROP_FONT_OUTLINE_LINE_JOIN_NUMBER = 'SDL_ttf.font.outline.line_join';
  TTF_PROP_FONT_OUTLINE_MITER_LIMIT_NUMBER = 'SDL_ttf.font.outline.miter_limit';

function TTF_GetFontGeneration(font: PTTF_Font): TUint32; cdecl; external libSDL3_ttf;
function TTF_AddFallbackFont(font: PTTF_Font; fallback: PTTF_Font): Tbool; cdecl; external libSDL3_ttf;
procedure TTF_RemoveFallbackFont(font: PTTF_Font; fallback: PTTF_Font); cdecl; external libSDL3_ttf;
procedure TTF_ClearFallbackFonts(font: PTTF_Font); cdecl; external libSDL3_ttf;
function TTF_SetFontSize(font: PTTF_Font; ptsize: single): Tbool; cdecl; external libSDL3_ttf;
function TTF_SetFontSizeDPI(font: PTTF_Font; ptsize: single; hdpi: longint; vdpi: longint): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetFontSize(font: PTTF_Font): single; cdecl; external libSDL3_ttf;
function TTF_GetFontDPI(font: PTTF_Font; hdpi: Plongint; vdpi: Plongint): Tbool; cdecl; external libSDL3_ttf;

type
  PTTF_FontStyleFlags = ^TTTF_FontStyleFlags;
  TTTF_FontStyleFlags = TUint32;

const
  TTF_STYLE_NORMAL = $00;
  TTF_STYLE_BOLD = $01;
  TTF_STYLE_ITALIC = $02;
  TTF_STYLE_UNDERLINE = $04;
  TTF_STYLE_STRIKETHROUGH = $08;

procedure TTF_SetFontStyle(font: PTTF_Font; style: TTTF_FontStyleFlags); cdecl; external libSDL3_ttf;
function TTF_GetFontStyle(font: PTTF_Font): TTTF_FontStyleFlags; cdecl; external libSDL3_ttf;
function TTF_SetFontOutline(font: PTTF_Font; outline: longint): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetFontOutline(font: PTTF_Font): longint; cdecl; external libSDL3_ttf;

type
  PTTF_HintingFlags = ^TTTF_HintingFlags;
  TTTF_HintingFlags = longint;

const
  TTF_HINTING_NORMAL = 0;
  TTF_HINTING_LIGHT = 1;
  TTF_HINTING_MONO = 2;
  TTF_HINTING_NONE = 3;
  TTF_HINTING_LIGHT_SUBPIXEL = 4;

procedure TTF_SetFontHinting(font: PTTF_Font; hinting: TTTF_HintingFlags); cdecl; external libSDL3_ttf;
function TTF_GetNumFontFaces(font: PTTF_Font): longint; cdecl; external libSDL3_ttf;
function TTF_GetFontHinting(font: PTTF_Font): TTTF_HintingFlags; cdecl; external libSDL3_ttf;
function TTF_SetFontSDF(font: PTTF_Font; Enabled: Tbool): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetFontSDF(font: PTTF_Font): Tbool; cdecl; external libSDL3_ttf;

type
  PTTF_HorizontalAlignment = ^TTTF_HorizontalAlignment;
  TTTF_HorizontalAlignment = longint;

const
  TTF_HORIZONTAL_ALIGN_INVALID = -(1);
  TTF_HORIZONTAL_ALIGN_LEFT = (-(1)) + 1;
  TTF_HORIZONTAL_ALIGN_CENTER = (-(1)) + 2;
  TTF_HORIZONTAL_ALIGN_RIGHT = (-(1)) + 3;

procedure TTF_SetFontWrapAlignment(font: PTTF_Font; align: TTTF_HorizontalAlignment); cdecl; external libSDL3_ttf;
function TTF_GetFontWrapAlignment(font: PTTF_Font): TTTF_HorizontalAlignment; cdecl; external libSDL3_ttf;
function TTF_GetFontHeight(font: PTTF_Font): longint; cdecl; external libSDL3_ttf;
function TTF_GetFontAscent(font: PTTF_Font): longint; cdecl; external libSDL3_ttf;
function TTF_GetFontDescent(font: PTTF_Font): longint; cdecl; external libSDL3_ttf;
procedure TTF_SetFontLineSkip(font: PTTF_Font; lineskip: longint); cdecl; external libSDL3_ttf;
function TTF_GetFontLineSkip(font: PTTF_Font): longint; cdecl; external libSDL3_ttf;
procedure TTF_SetFontKerning(font: PTTF_Font; Enabled: Tbool); cdecl; external libSDL3_ttf;
function TTF_GetFontKerning(font: PTTF_Font): Tbool; cdecl; external libSDL3_ttf;
function TTF_FontIsFixedWidth(font: PTTF_Font): Tbool; cdecl; external libSDL3_ttf;
function TTF_FontIsScalable(font: PTTF_Font): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetFontFamilyName(font: PTTF_Font): pchar; cdecl; external libSDL3_ttf;
function TTF_GetFontStyleName(font: PTTF_Font): pchar; cdecl; external libSDL3_ttf;

type
  PTTF_Direction = ^TTTF_Direction;
  TTTF_Direction = longint;

const
  TTF_DIRECTION_INVALID = 0;
  TTF_DIRECTION_LTR = 4;
  TTF_DIRECTION_RTL = 5;
  TTF_DIRECTION_TTB = 6;
  TTF_DIRECTION_BTT = 7;

function TTF_SetFontDirection(font: PTTF_Font; direction: TTTF_Direction): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetFontDirection(font: PTTF_Font): TTTF_Direction; cdecl; external libSDL3_ttf;
function TTF_SetFontScript(font: PTTF_Font; script: TUint32): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetFontScript(font: PTTF_Font): TUint32; cdecl; external libSDL3_ttf;
function TTF_GetGlyphScript(ch: TUint32): TUint32; cdecl; external libSDL3_ttf;
function TTF_SetFontLanguage(font: PTTF_Font; language_bcp47: pchar): Tbool; cdecl; external libSDL3_ttf;
function TTF_FontHasGlyph(font: PTTF_Font; ch: TUint32): Tbool; cdecl; external libSDL3_ttf;

type
  PTTF_ImageType = ^TTTF_ImageType;
  TTTF_ImageType = longint;

const
  TTF_IMAGE_INVALID = 0;
  TTF_IMAGE_ALPHA = 1;
  TTF_IMAGE_COLOR = 2;
  TTF_IMAGE_SDF = 3;

function TTF_GetGlyphImage(font: PTTF_Font; ch: TUint32; image_type: PTTF_ImageType): PSDL_Surface; cdecl; external libSDL3_ttf;
function TTF_GetGlyphImageForIndex(font: PTTF_Font; glyph_index: TUint32; image_type: PTTF_ImageType): PSDL_Surface; cdecl; external libSDL3_ttf;
function TTF_GetGlyphMetrics(font: PTTF_Font; ch: TUint32; minx: Plongint; maxx: Plongint; miny: Plongint;
  maxy: Plongint; advance: Plongint): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetGlyphKerning(font: PTTF_Font; previous_ch: TUint32; ch: TUint32; kerning: Plongint): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetStringSize(font: PTTF_Font; Text: pchar; length: Tsize_t; w: Plongint; h: Plongint): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetStringSizeWrapped(font: PTTF_Font; Text: pchar; length: Tsize_t; wrap_width: longint; w: Plongint;
  h: Plongint): Tbool; cdecl; external libSDL3_ttf;
function TTF_MeasureString(font: PTTF_Font; Text: pchar; length: Tsize_t; max_width: longint; measured_width: Plongint;
  measured_length: Psize_t): Tbool; cdecl; external libSDL3_ttf;
function TTF_RenderText_Solid(font: PTTF_Font; Text: pchar; length: Tsize_t; fg: TSDL_Color): PSDL_Surface; cdecl; external libSDL3_ttf;
function TTF_RenderText_Solid_Wrapped(font: PTTF_Font; Text: pchar; length: Tsize_t; fg: TSDL_Color; wrapLength: longint): PSDL_Surface; cdecl; external libSDL3_ttf;
function TTF_RenderGlyph_Solid(font: PTTF_Font; ch: TUint32; fg: TSDL_Color): PSDL_Surface; cdecl; external libSDL3_ttf;
function TTF_RenderText_Shaded(font: PTTF_Font; Text: pchar; length: Tsize_t; fg: TSDL_Color; bg: TSDL_Color): PSDL_Surface; cdecl; external libSDL3_ttf;
function TTF_RenderText_Shaded_Wrapped(font: PTTF_Font; Text: pchar; length: Tsize_t; fg: TSDL_Color; bg: TSDL_Color;
  wrap_width: longint): PSDL_Surface; cdecl; external libSDL3_ttf;
function TTF_RenderGlyph_Shaded(font: PTTF_Font; ch: TUint32; fg: TSDL_Color; bg: TSDL_Color): PSDL_Surface; cdecl; external libSDL3_ttf;
function TTF_RenderText_Blended(font: PTTF_Font; Text: pchar; length: Tsize_t; fg: TSDL_Color): PSDL_Surface; cdecl; external libSDL3_ttf;
function TTF_RenderText_Blended_Wrapped(font: PTTF_Font; Text: pchar; length: Tsize_t; fg: TSDL_Color; wrap_width: longint): PSDL_Surface; cdecl; external libSDL3_ttf;
function TTF_RenderGlyph_Blended(font: PTTF_Font; ch: TUint32; fg: TSDL_Color): PSDL_Surface; cdecl; external libSDL3_ttf;
function TTF_RenderText_LCD(font: PTTF_Font; Text: pchar; length: Tsize_t; fg: TSDL_Color; bg: TSDL_Color): PSDL_Surface; cdecl; external libSDL3_ttf;
function TTF_RenderText_LCD_Wrapped(font: PTTF_Font; Text: pchar; length: Tsize_t; fg: TSDL_Color; bg: TSDL_Color;
  wrap_width: longint): PSDL_Surface; cdecl; external libSDL3_ttf;
function TTF_RenderGlyph_LCD(font: PTTF_Font; ch: TUint32; fg: TSDL_Color; bg: TSDL_Color): PSDL_Surface; cdecl; external libSDL3_ttf;

function TTF_CreateSurfaceTextEngine: PTTF_TextEngine; cdecl; external libSDL3_ttf;
function TTF_DrawSurfaceText(Text: PTTF_Text; x: longint; y: longint; surface: PSDL_Surface): Tbool; cdecl; external libSDL3_ttf;
procedure TTF_DestroySurfaceTextEngine(engine: PTTF_TextEngine); cdecl; external libSDL3_ttf;
function TTF_CreateRendererTextEngine(renderer: PSDL_Renderer): PTTF_TextEngine; cdecl; external libSDL3_ttf;
function TTF_CreateRendererTextEngineWithProperties(props: TSDL_PropertiesID): PTTF_TextEngine; cdecl; external libSDL3_ttf;

const
  TTF_PROP_RENDERER_TEXT_ENGINE_RENDERER = 'SDL_ttf.renderer_text_engine.create.renderer';
  TTF_PROP_RENDERER_TEXT_ENGINE_ATLAS_TEXTURE_SIZE = 'SDL_ttf.renderer_text_engine.create.atlas_texture_size';

function TTF_DrawRendererText(Text: PTTF_Text; x: single; y: single): Tbool; cdecl; external libSDL3_ttf;
procedure TTF_DestroyRendererTextEngine(engine: PTTF_TextEngine); cdecl; external libSDL3_ttf;
function TTF_CreateGPUTextEngine(device: PSDL_GPUDevice): PTTF_TextEngine; cdecl; external libSDL3_ttf;
function TTF_CreateGPUTextEngineWithProperties(props: TSDL_PropertiesID): PTTF_TextEngine; cdecl; external libSDL3_ttf;

const
  TTF_PROP_GPU_TEXT_ENGINE_DEVICE = 'SDL_ttf.gpu_text_engine.create.device';
  TTF_PROP_GPU_TEXT_ENGINE_ATLAS_TEXTURE_SIZE = 'SDL_ttf.gpu_text_engine.create.atlas_texture_size';

type
  PTTF_GPUAtlasDrawSequence = ^TTTF_GPUAtlasDrawSequence;

  TTTF_GPUAtlasDrawSequence = record
    atlas_texture: PSDL_GPUTexture;
    xy: PSDL_FPoint;
    uv: PSDL_FPoint;
    num_vertices: longint;
    indices: Plongint;
    num_indices: longint;
    image_type: TTTF_ImageType;
    Next: PTTF_GPUAtlasDrawSequence;
  end;

function TTF_GetGPUTextDrawData(Text: PTTF_Text): PTTF_GPUAtlasDrawSequence; cdecl; external libSDL3_ttf;
procedure TTF_DestroyGPUTextEngine(engine: PTTF_TextEngine); cdecl; external libSDL3_ttf;

type
  PTTF_GPUTextEngineWinding = ^TTTF_GPUTextEngineWinding;
  TTTF_GPUTextEngineWinding = longint;

const
  TTF_GPU_TEXTENGINE_WINDING_INVALID = -(1);
  TTF_GPU_TEXTENGINE_WINDING_CLOCKWISE = (-(1)) + 1;
  TTF_GPU_TEXTENGINE_WINDING_COUNTER_CLOCKWISE = (-(1)) + 2;

procedure TTF_SetGPUTextEngineWinding(engine: PTTF_TextEngine; winding: TTTF_GPUTextEngineWinding); cdecl; external libSDL3_ttf;
function TTF_GetGPUTextEngineWinding(engine: PTTF_TextEngine): TTTF_GPUTextEngineWinding; cdecl; external libSDL3_ttf;
function TTF_CreateText(engine: PTTF_TextEngine; font: PTTF_Font; Text: pchar; length: Tsize_t): PTTF_Text; cdecl; external libSDL3_ttf;
function TTF_GetTextProperties(Text: PTTF_Text): TSDL_PropertiesID; cdecl; external libSDL3_ttf;
function TTF_SetTextEngine(Text: PTTF_Text; engine: PTTF_TextEngine): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetTextEngine(Text: PTTF_Text): PTTF_TextEngine; cdecl; external libSDL3_ttf;
function TTF_SetTextFont(Text: PTTF_Text; font: PTTF_Font): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetTextFont(Text: PTTF_Text): PTTF_Font; cdecl; external libSDL3_ttf;
function TTF_SetTextDirection(Text: PTTF_Text; direction: TTTF_Direction): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetTextDirection(Text: PTTF_Text): TTTF_Direction; cdecl; external libSDL3_ttf;
function TTF_SetTextScript(Text: PTTF_Text; script: TUint32): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetTextScript(Text: PTTF_Text): TUint32; cdecl; external libSDL3_ttf;
function TTF_SetTextColor(Text: PTTF_Text; r: TUint8; g: TUint8; b: TUint8; a: TUint8): Tbool; cdecl; external libSDL3_ttf;
function TTF_SetTextColorFloat(Text: PTTF_Text; r: single; g: single; b: single; a: single): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetTextColor(Text: PTTF_Text; r: PUint8; g: PUint8; b: PUint8; a: PUint8): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetTextColorFloat(Text: PTTF_Text; r: Psingle; g: Psingle; b: Psingle; a: Psingle): Tbool; cdecl; external libSDL3_ttf;
function TTF_SetTextPosition(Text: PTTF_Text; x: longint; y: longint): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetTextPosition(Text: PTTF_Text; x: Plongint; y: Plongint): Tbool; cdecl; external libSDL3_ttf;
function TTF_SetTextWrapWidth(Text: PTTF_Text; wrap_width: longint): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetTextWrapWidth(Text: PTTF_Text; wrap_width: Plongint): Tbool; cdecl; external libSDL3_ttf;
function TTF_SetTextWrapWhitespaceVisible(Text: PTTF_Text; Visible: Tbool): Tbool; cdecl; external libSDL3_ttf;
function TTF_TextWrapWhitespaceVisible(Text: PTTF_Text): Tbool; cdecl; external libSDL3_ttf;
function TTF_SetTextString(Text: PTTF_Text; _string: pchar; length: Tsize_t): Tbool; cdecl; external libSDL3_ttf;
function TTF_InsertTextString(Text: PTTF_Text; offset: longint; _string: pchar; length: Tsize_t): Tbool; cdecl; external libSDL3_ttf;
function TTF_AppendTextString(Text: PTTF_Text; _string: pchar; length: Tsize_t): Tbool; cdecl; external libSDL3_ttf;
function TTF_DeleteTextString(Text: PTTF_Text; offset: longint; length: longint): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetTextSize(Text: PTTF_Text; w: Plongint; h: Plongint): Tbool; cdecl; external libSDL3_ttf;

function TTF_GetTextSubString(Text: PTTF_Text; offset: longint; substring: PTTF_SubString): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetTextSubStringForLine(Text: PTTF_Text; line: longint; substring: PTTF_SubString): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetTextSubStringsForRange(Text: PTTF_Text; offset: longint; length: longint; Count: Plongint): PPTTF_SubString; cdecl; external libSDL3_ttf;
function TTF_GetTextSubStringForPoint(Text: PTTF_Text; x: longint; y: longint; substring: PTTF_SubString): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetPreviousTextSubString(Text: PTTF_Text; substring: PTTF_SubString; previous: PTTF_SubString): Tbool; cdecl; external libSDL3_ttf;
function TTF_GetNextTextSubString(Text: PTTF_Text; substring: PTTF_SubString; Next: PTTF_SubString): Tbool; cdecl; external libSDL3_ttf;
function TTF_UpdateText(Text: PTTF_Text): Tbool; cdecl; external libSDL3_ttf;
procedure TTF_DestroyText(Text: PTTF_Text); cdecl; external libSDL3_ttf;
procedure TTF_CloseFont(font: PTTF_Font); cdecl; external libSDL3_ttf;
procedure TTF_Quit; cdecl; external libSDL3_ttf;
function TTF_WasInit: longint; cdecl; external libSDL3_ttf;

// =====

function SDL_TTF_VERSION: longint;
function SDL_TTF_VERSION_ATLEAST(X, Y, Z: longint): TSDL_bool;

implementation

function SDL_TTF_VERSION: longint;
begin
  SDL_TTF_VERSION := SDL_VERSIONNUM(SDL_TTF_MAJOR_VERSION, SDL_TTF_MINOR_VERSION, SDL_TTF_MICRO_VERSION);
end;

function SDL_TTF_VERSION_ATLEAST(X, Y, Z: longint): TSDL_bool;
begin
  SDL_TTF_VERSION_ATLEAST :=
    (SDL_TTF_MAJOR_VERSION >= X) and
    ((SDL_TTF_MAJOR_VERSION > X) or (SDL_TTF_MINOR_VERSION >= Y)) and
    ((SDL_TTF_MAJOR_VERSION > X) or (SDL_TTF_MINOR_VERSION > Y) or (SDL_TTF_MICRO_VERSION >= Z));
end;


end.
