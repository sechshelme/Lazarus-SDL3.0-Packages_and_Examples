
unit SDL_ttf;
interface

{
  Automatically converted by H2Pas 1.0.0 from SDL_ttf.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_ttf.h
}

{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

Type
Pchar  = ^char;
Plongint  = ^longint;
PSDL_FPoint  = ^SDL_FPoint;
PSDL_GPUDevice  = ^SDL_GPUDevice;
PSDL_GPUTexture  = ^SDL_GPUTexture;
PSDL_IOStream  = ^SDL_IOStream;
PSDL_Renderer  = ^SDL_Renderer;
PSDL_Surface  = ^SDL_Surface;
Psingle  = ^single;
Psize_t  = ^size_t;
PTTF_Direction  = ^TTF_Direction;
PTTF_Font  = ^TTF_Font;
PTTF_FontStyleFlags  = ^TTF_FontStyleFlags;
PTTF_GPUAtlasDrawSequence  = ^TTF_GPUAtlasDrawSequence;
PTTF_GPUTextEngineWinding  = ^TTF_GPUTextEngineWinding;
PTTF_HintingFlags  = ^TTF_HintingFlags;
PTTF_HorizontalAlignment  = ^TTF_HorizontalAlignment;
PTTF_ImageType  = ^TTF_ImageType;
PTTF_SubString  = ^TTF_SubString;
PTTF_SubStringFlags  = ^TTF_SubStringFlags;
PTTF_Text  = ^TTF_Text;
PTTF_TextData  = ^TTF_TextData;
PTTF_TextEngine  = ^TTF_TextEngine;
PUint8  = ^Uint8;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{
  SDL_ttf:  A companion library to SDL for working with TrueType (tm) fonts
  Copyright (C) 2001-2025 Sam Lantinga <slouken@libsdl.org>

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
 }
{ WIKI CATEGORY: SDLTTF  }
{*
 * # CategorySDLTTF
 *
 * Header file for SDL_ttf library
 *
 * This library is a wrapper around the excellent FreeType 2.0 library,
 * available at: https://www.freetype.org/
  }
{$ifndef SDL_TTF_H_}
{$define SDL_TTF_H_}
{$include <SDL3/SDL.h>}
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{*
 * Printable format: "%d.%d.%d", MAJOR, MINOR, MICRO
  }

const
  SDL_TTF_MAJOR_VERSION = 3;  
  SDL_TTF_MINOR_VERSION = 1;  
  SDL_TTF_MICRO_VERSION = 0;  
{*
 * This is the version number macro for the current SDL_ttf version.
  }

{ was #define dname def_expr }
function SDL_TTF_VERSION : longint; { return type might be wrong }

{*
 * This macro will evaluate to true if compiled with SDL_ttf at least X.Y.Z.
  }
{#define SDL_TTF_VERSION_ATLEAST(X, Y, Z) \ }
{    ((SDL_TTF_MAJOR_VERSION >= X) && \ }
{     (SDL_TTF_MAJOR_VERSION > X || SDL_TTF_MINOR_VERSION >= Y) && \ }
{     (SDL_TTF_MAJOR_VERSION > X || SDL_TTF_MINOR_VERSION > Y || SDL_TTF_MICRO_VERSION >= Z)) }
{*
 * This function gets the version of the dynamically linked SDL_ttf library.
 *
 * \returns SDL_ttf version.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
function TTF_Version:longint;cdecl;external;
{*
 * Query the version of the FreeType library in use.
 *
 * TTF_Init() should be called before calling this function.
 *
 * \param major to be filled in with the major version number. Can be NULL.
 * \param minor to be filled in with the minor version number. Can be NULL.
 * \param patch to be filled in with the param version number. Can be NULL.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_Init
  }
procedure TTF_GetFreeTypeVersion(major:Plongint; minor:Plongint; patch:Plongint);cdecl;external;
{*
 * Query the version of the HarfBuzz library in use.
 *
 * If HarfBuzz is not available, the version reported is 0.0.0.
 *
 * \param major to be filled in with the major version number. Can be NULL.
 * \param minor to be filled in with the minor version number. Can be NULL.
 * \param patch to be filled in with the param version number. Can be NULL.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
procedure TTF_GetHarfBuzzVersion(major:Plongint; minor:Plongint; patch:Plongint);cdecl;external;
{*
 * The internal structure containing font information.
 *
 * Opaque data!
  }
type
{*
 * Initialize SDL_ttf.
 *
 * You must successfully call this function before it is safe to call any
 * other function in this library.
 *
 * It is safe to call this more than once, and each successful TTF_Init() call
 * should be paired with a matching TTF_Quit() call.
 *
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_Quit
  }

function TTF_Init:Tbool;cdecl;external;
{*
 * Create a font from a file, using a specified point size.
 *
 * Some .fon fonts will have several sizes embedded in the file, so the point
 * size becomes the index of choosing which size. If the value is too high,
 * the last indexed size will be the default.
 *
 * When done with the returned TTF_Font, use TTF_CloseFont() to dispose of it.
 *
 * \param file path to font file.
 * \param ptsize point size to use for the newly-opened font.
 * \returns a valid TTF_Font, or NULL on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_CloseFont
  }
(* Const before type ignored *)
function TTF_OpenFont(file:Pchar; ptsize:single):PTTF_Font;cdecl;external;
{*
 * Create a font from an SDL_IOStream, using a specified point size.
 *
 * Some .fon fonts will have several sizes embedded in the file, so the point
 * size becomes the index of choosing which size. If the value is too high,
 * the last indexed size will be the default.
 *
 * If `closeio` is true, `src` will be automatically closed once the font is
 * closed. Otherwise you should close `src` yourself after closing the font.
 *
 * When done with the returned TTF_Font, use TTF_CloseFont() to dispose of it.
 *
 * \param src an SDL_IOStream to provide a font file's data.
 * \param closeio true to close `src` when the font is closed, false to leave
 *                it open.
 * \param ptsize point size to use for the newly-opened font.
 * \returns a valid TTF_Font, or NULL on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_CloseFont
  }
function TTF_OpenFontIO(src:PSDL_IOStream; closeio:Tbool; ptsize:single):PTTF_Font;cdecl;external;
{*
 * Create a font with the specified properties.
 *
 * These are the supported properties:
 *
 * - `TTF_PROP_FONT_CREATE_FILENAME_STRING`: the font file to open, if an
 *   SDL_IOStream isn't being used. This is required if
 *   `TTF_PROP_FONT_CREATE_IOSTREAM_POINTER` and
 *   `TTF_PROP_FONT_CREATE_EXISTING_FONT` aren't set.
 * - `TTF_PROP_FONT_CREATE_IOSTREAM_POINTER`: an SDL_IOStream containing the
 *   font to be opened. This should not be closed until the font is closed.
 *   This is required if `TTF_PROP_FONT_CREATE_FILENAME_STRING` and
 *   `TTF_PROP_FONT_CREATE_EXISTING_FONT` aren't set.
 * - `TTF_PROP_FONT_CREATE_IOSTREAM_OFFSET_NUMBER`: the offset in the iostream
 *   for the beginning of the font, defaults to 0.
 * - `TTF_PROP_FONT_CREATE_IOSTREAM_AUTOCLOSE_BOOLEAN`: true if closing the
 *   font should also close the associated SDL_IOStream.
 * - `TTF_PROP_FONT_CREATE_SIZE_FLOAT`: the point size of the font. Some .fon
 *   fonts will have several sizes embedded in the file, so the point size
 *   becomes the index of choosing which size. If the value is too high, the
 *   last indexed size will be the default.
 * - `TTF_PROP_FONT_CREATE_FACE_NUMBER`: the face index of the font, if the
 *   font contains multiple font faces.
 * - `TTF_PROP_FONT_CREATE_HORIZONTAL_DPI_NUMBER`: the horizontal DPI to use
 *   for font rendering, defaults to
 *   `TTF_PROP_FONT_CREATE_VERTICAL_DPI_NUMBER` if set, or 72 otherwise.
 * - `TTF_PROP_FONT_CREATE_VERTICAL_DPI_NUMBER`: the vertical DPI to use for
 *   font rendering, defaults to `TTF_PROP_FONT_CREATE_HORIZONTAL_DPI_NUMBER`
 *   if set, or 72 otherwise.
 * - `TTF_PROP_FONT_CREATE_EXISTING_FONT`: an optional TTF_Font that, if set,
 *   will be used as the font data source and the initial size and style of
 *   the new font.
 *
 * \param props the properties to use.
 * \returns a valid TTF_Font, or NULL on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_CloseFont
  }
function TTF_OpenFontWithProperties(props:TSDL_PropertiesID):PTTF_Font;cdecl;external;
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
{*
 * Create a copy of an existing font.
 *
 * The copy will be distinct from the original, but will share the font file
 * and have the same size and style as the original.
 *
 * When done with the returned TTF_Font, use TTF_CloseFont() to dispose of it.
 *
 * \param existing_font the font to copy.
 * \returns a valid TTF_Font, or NULL on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               original font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_CloseFont
  }

function TTF_CopyFont(existing_font:PTTF_Font):PTTF_Font;cdecl;external;
{*
 * Get the properties associated with a font.
 *
 * The following read-write properties are provided by SDL:
 *
 * - `TTF_PROP_FONT_OUTLINE_LINE_CAP_NUMBER`: The FT_Stroker_LineCap value
 *   used when setting the font outline, defaults to
 *   `FT_STROKER_LINECAP_ROUND`.
 * - `TTF_PROP_FONT_OUTLINE_LINE_JOIN_NUMBER`: The FT_Stroker_LineJoin value
 *   used when setting the font outline, defaults to
 *   `FT_STROKER_LINEJOIN_ROUND`.
 * - `TTF_PROP_FONT_OUTLINE_MITER_LIMIT_NUMBER`: The FT_Fixed miter limit used
 *   when setting the font outline, defaults to 0.
 *
 * \param font the font to query.
 * \returns a valid property ID on success or 0 on failure; call
 *          SDL_GetError() for more information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
function TTF_GetFontProperties(font:PTTF_Font):TSDL_PropertiesID;cdecl;external;
const
  TTF_PROP_FONT_OUTLINE_LINE_CAP_NUMBER = 'SDL_ttf.font.outline.line_cap';  
  TTF_PROP_FONT_OUTLINE_LINE_JOIN_NUMBER = 'SDL_ttf.font.outline.line_join';  
  TTF_PROP_FONT_OUTLINE_MITER_LIMIT_NUMBER = 'SDL_ttf.font.outline.miter_limit';  
{*
 * Get the font generation.
 *
 * The generation is incremented each time font properties change that require
 * rebuilding glyphs, such as style, size, etc.
 *
 * \param font the font to query.
 * \returns the font generation or 0 on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }

function TTF_GetFontGeneration(font:PTTF_Font):TUint32;cdecl;external;
{*
 * Add a fallback font.
 *
 * Add a font that will be used for glyphs that are not in the current font.
 * The fallback font should have the same size and style as the current font.
 *
 * If there are multiple fallback fonts, they are used in the order added.
 *
 * This updates any TTF_Text objects using this font.
 *
 * \param font the font to modify.
 * \param fallback the font to add as a fallback.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created
 *               both fonts.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_ClearFallbackFonts
 * \sa TTF_RemoveFallbackFont
  }
function TTF_AddFallbackFont(font:PTTF_Font; fallback:PTTF_Font):Tbool;cdecl;external;
{*
 * Remove a fallback font.
 *
 * This updates any TTF_Text objects using this font.
 *
 * \param font the font to modify.
 * \param fallback the font to remove as a fallback.
 *
 * \threadsafety This function should be called on the thread that created
 *               both fonts.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_AddFallbackFont
 * \sa TTF_ClearFallbackFonts
  }
procedure TTF_RemoveFallbackFont(font:PTTF_Font; fallback:PTTF_Font);cdecl;external;
{*
 * Remove all fallback fonts.
 *
 * This updates any TTF_Text objects using this font.
 *
 * \param font the font to modify.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_AddFallbackFont
 * \sa TTF_RemoveFallbackFont
  }
procedure TTF_ClearFallbackFonts(font:PTTF_Font);cdecl;external;
{*
 * Set a font's size dynamically.
 *
 * This updates any TTF_Text objects using this font, and clears
 * already-generated glyphs, if any, from the cache.
 *
 * \param font the font to resize.
 * \param ptsize the new point size.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_GetFontSize
  }
function TTF_SetFontSize(font:PTTF_Font; ptsize:single):Tbool;cdecl;external;
{*
 * Set font size dynamically with target resolutions, in dots per inch.
 *
 * This updates any TTF_Text objects using this font, and clears
 * already-generated glyphs, if any, from the cache.
 *
 * \param font the font to resize.
 * \param ptsize the new point size.
 * \param hdpi the target horizontal DPI.
 * \param vdpi the target vertical DPI.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_GetFontSize
 * \sa TTF_GetFontSizeDPI
  }
function TTF_SetFontSizeDPI(font:PTTF_Font; ptsize:single; hdpi:longint; vdpi:longint):Tbool;cdecl;external;
{*
 * Get the size of a font.
 *
 * \param font the font to query.
 * \returns the size of the font, or 0.0f on failure; call SDL_GetError() for
 *          more information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_SetFontSize
 * \sa TTF_SetFontSizeDPI
  }
function TTF_GetFontSize(font:PTTF_Font):single;cdecl;external;
{*
 * Get font target resolutions, in dots per inch.
 *
 * \param font the font to query.
 * \param hdpi a pointer filled in with the target horizontal DPI.
 * \param vdpi a pointer filled in with the target vertical DPI.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_SetFontSizeDPI
  }
function TTF_GetFontDPI(font:PTTF_Font; hdpi:Plongint; vdpi:Plongint):Tbool;cdecl;external;
{*
 * Font style flags for TTF_Font
 *
 * These are the flags which can be used to set the style of a font in
 * SDL_ttf. A combination of these flags can be used with functions that set
 * or query font style, such as TTF_SetFontStyle or TTF_GetFontStyle.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_SetFontStyle
 * \sa TTF_GetFontStyle
  }
type
  PTTF_FontStyleFlags = ^TTTF_FontStyleFlags;
  TTTF_FontStyleFlags = TUint32;
{*< No special style  }

const
  TTF_STYLE_NORMAL = $00;  
{*< Bold style  }
  TTF_STYLE_BOLD = $01;  
{*< Italic style  }
  TTF_STYLE_ITALIC = $02;  
{*< Underlined text  }
  TTF_STYLE_UNDERLINE = $04;  
{*< Strikethrough text  }
  TTF_STYLE_STRIKETHROUGH = $08;  
{*
 * Set a font's current style.
 *
 * This updates any TTF_Text objects using this font, and clears
 * already-generated glyphs, if any, from the cache.
 *
 * The font styles are a set of bit flags, OR'd together:
 *
 * - `TTF_STYLE_NORMAL` (is zero)
 * - `TTF_STYLE_BOLD`
 * - `TTF_STYLE_ITALIC`
 * - `TTF_STYLE_UNDERLINE`
 * - `TTF_STYLE_STRIKETHROUGH`
 *
 * \param font the font to set a new style on.
 * \param style the new style values to set, OR'd together.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_GetFontStyle
  }

procedure TTF_SetFontStyle(font:PTTF_Font; style:TTTF_FontStyleFlags);cdecl;external;
{*
 * Query a font's current style.
 *
 * The font styles are a set of bit flags, OR'd together:
 *
 * - `TTF_STYLE_NORMAL` (is zero)
 * - `TTF_STYLE_BOLD`
 * - `TTF_STYLE_ITALIC`
 * - `TTF_STYLE_UNDERLINE`
 * - `TTF_STYLE_STRIKETHROUGH`
 *
 * \param font the font to query.
 * \returns the current font style, as a set of bit flags.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_SetFontStyle
  }
(* Const before type ignored *)
function TTF_GetFontStyle(font:PTTF_Font):TTTF_FontStyleFlags;cdecl;external;
{*
 * Set a font's current outline.
 *
 * This uses the font properties `TTF_PROP_FONT_OUTLINE_LINE_CAP_NUMBER`,
 * `TTF_PROP_FONT_OUTLINE_LINE_JOIN_NUMBER`, and
 * `TTF_PROP_FONT_OUTLINE_MITER_LIMIT_NUMBER` when setting the font outline.
 *
 * This updates any TTF_Text objects using this font, and clears
 * already-generated glyphs, if any, from the cache.
 *
 * \param font the font to set a new outline on.
 * \param outline positive outline value, 0 to default.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_GetFontOutline
  }
function TTF_SetFontOutline(font:PTTF_Font; outline:longint):Tbool;cdecl;external;
{*
 * Query a font's current outline.
 *
 * \param font the font to query.
 * \returns the font's current outline value.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_SetFontOutline
  }
(* Const before type ignored *)
function TTF_GetFontOutline(font:PTTF_Font):longint;cdecl;external;
{*
 * Hinting flags for TTF (TrueType Fonts)
 *
 * This enum specifies the level of hinting to be applied to the font
 * rendering. The hinting level determines how much the font's outlines are
 * adjusted for better alignment on the pixel grid.
 *
 * \since This enum is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_SetFontHinting
 * \sa TTF_GetFontHinting
  }
{*< Normal hinting applies standard grid-fitting.  }
{*< Light hinting applies subtle adjustments to improve rendering.  }
{*< Monochrome hinting adjusts the font for better rendering at lower resolutions.  }
{*< No hinting, the font is rendered without any grid-fitting.  }
{*< Light hinting with subpixel rendering for more precise font edges.  }
type
  PTTF_HintingFlags = ^TTTF_HintingFlags;
  TTTF_HintingFlags =  Longint;
  Const
    TTF_HINTING_NORMAL = 0;
    TTF_HINTING_LIGHT = 1;
    TTF_HINTING_MONO = 2;
    TTF_HINTING_NONE = 3;
    TTF_HINTING_LIGHT_SUBPIXEL = 4;
;
{*
 * Set a font's current hinter setting.
 *
 * This updates any TTF_Text objects using this font, and clears
 * already-generated glyphs, if any, from the cache.
 *
 * The hinter setting is a single value:
 *
 * - `TTF_HINTING_NORMAL`
 * - `TTF_HINTING_LIGHT`
 * - `TTF_HINTING_MONO`
 * - `TTF_HINTING_NONE`
 * - `TTF_HINTING_LIGHT_SUBPIXEL` (available in SDL_ttf 3.0.0 and later)
 *
 * \param font the font to set a new hinter setting on.
 * \param hinting the new hinter setting.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_GetFontHinting
  }

procedure TTF_SetFontHinting(font:PTTF_Font; hinting:TTTF_HintingFlags);cdecl;external;
{*
 * Query the number of faces of a font.
 *
 * \param font the font to query.
 * \returns the number of FreeType font faces.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
(* Const before type ignored *)
function TTF_GetNumFontFaces(font:PTTF_Font):longint;cdecl;external;
{*
 * Query a font's current FreeType hinter setting.
 *
 * The hinter setting is a single value:
 *
 * - `TTF_HINTING_NORMAL`
 * - `TTF_HINTING_LIGHT`
 * - `TTF_HINTING_MONO`
 * - `TTF_HINTING_NONE`
 * - `TTF_HINTING_LIGHT_SUBPIXEL` (available in SDL_ttf 3.0.0 and later)
 *
 * \param font the font to query.
 * \returns the font's current hinter value.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_SetFontHinting
  }
(* Const before type ignored *)
function TTF_GetFontHinting(font:PTTF_Font):TTTF_HintingFlags;cdecl;external;
{*
 * Enable Signed Distance Field rendering for a font.
 *
 * SDF is a technique that helps fonts look sharp even when scaling and rotating, and requires special shader support for display.
 *
 * This works with Blended APIs, and generates the raw signed distance values in the alpha channel of the resulting texture.
 *
 * This updates any TTF_Text objects using this font, and clears already-generated glyphs, if any, from the cache.
 *
 * \param font the font to set SDF support on.
 * \param enabled true to enable SDF, false to disable.
 * \returns true on success or false on failure; call SDL_GetError()
 *          for more information.
 *
 * \threadsafety This function should be called on the thread that created the font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_GetFontSDF
  }
function TTF_SetFontSDF(font:PTTF_Font; enabled:Tbool):Tbool;cdecl;external;
{*
 * Query whether Signed Distance Field rendering is enabled for a font.
 *
 * \param font the font to query
 *
 * \returns true if enabled, false otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_SetFontSDF
  }
(* Const before type ignored *)
function TTF_GetFontSDF(font:PTTF_Font):Tbool;cdecl;external;
{*
 * The horizontal alignment used when rendering wrapped text.
 *
 * \since This enum is available since SDL_ttf 3.0.0.
  }
type
  PTTF_HorizontalAlignment = ^TTTF_HorizontalAlignment;
  TTTF_HorizontalAlignment =  Longint;
  Const
    TTF_HORIZONTAL_ALIGN_INVALID = -(1);
    TTF_HORIZONTAL_ALIGN_LEFT = (-(1))+1;
    TTF_HORIZONTAL_ALIGN_CENTER = (-(1))+2;
    TTF_HORIZONTAL_ALIGN_RIGHT = (-(1))+3;
;
{*
 * Set a font's current wrap alignment option.
 *
 * This updates any TTF_Text objects using this font.
 *
 * \param font the font to set a new wrap alignment option on.
 * \param align the new wrap alignment option.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_GetFontWrapAlignment
  }

procedure TTF_SetFontWrapAlignment(font:PTTF_Font; align:TTTF_HorizontalAlignment);cdecl;external;
{*
 * Query a font's current wrap alignment option.
 *
 * \param font the font to query.
 * \returns the font's current wrap alignment option.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_SetFontWrapAlignment
  }
(* Const before type ignored *)
function TTF_GetFontWrapAlignment(font:PTTF_Font):TTTF_HorizontalAlignment;cdecl;external;
{*
 * Query the total height of a font.
 *
 * This is usually equal to point size.
 *
 * \param font the font to query.
 * \returns the font's height.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
(* Const before type ignored *)
function TTF_GetFontHeight(font:PTTF_Font):longint;cdecl;external;
{*
 * Query the offset from the baseline to the top of a font.
 *
 * This is a positive value, relative to the baseline.
 *
 * \param font the font to query.
 * \returns the font's ascent.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
(* Const before type ignored *)
function TTF_GetFontAscent(font:PTTF_Font):longint;cdecl;external;
{*
 * Query the offset from the baseline to the bottom of a font.
 *
 * This is a negative value, relative to the baseline.
 *
 * \param font the font to query.
 * \returns the font's descent.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
(* Const before type ignored *)
function TTF_GetFontDescent(font:PTTF_Font):longint;cdecl;external;
{*
 * Set the spacing between lines of text for a font.
 *
 * This updates any TTF_Text objects using this font.
 *
 * \param font the font to modify.
 * \param lineskip the new line spacing for the font.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_GetFontLineSkip
  }
procedure TTF_SetFontLineSkip(font:PTTF_Font; lineskip:longint);cdecl;external;
{*
 * Query the spacing between lines of text for a font.
 *
 * \param font the font to query.
 * \returns the font's recommended spacing.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_SetFontLineSkip
  }
(* Const before type ignored *)
function TTF_GetFontLineSkip(font:PTTF_Font):longint;cdecl;external;
{*
 * Set if kerning is enabled for a font.
 *
 * Newly-opened fonts default to allowing kerning. This is generally a good
 * policy unless you have a strong reason to disable it, as it tends to
 * produce better rendering (with kerning disabled, some fonts might render
 * the word `kerning` as something that looks like `keming` for example).
 *
 * This updates any TTF_Text objects using this font.
 *
 * \param font the font to set kerning on.
 * \param enabled true to enable kerning, false to disable.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_GetFontKerning
  }
procedure TTF_SetFontKerning(font:PTTF_Font; enabled:Tbool);cdecl;external;
{*
 * Query whether or not kerning is enabled for a font.
 *
 * \param font the font to query.
 * \returns true if kerning is enabled, false otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_SetFontKerning
  }
(* Const before type ignored *)
function TTF_GetFontKerning(font:PTTF_Font):Tbool;cdecl;external;
{*
 * Query whether a font is fixed-width.
 *
 * A "fixed-width" font means all glyphs are the same width across; a
 * lowercase 'i' will be the same size across as a capital 'W', for example.
 * This is common for terminals and text editors, and other apps that treat
 * text as a grid. Most other things (WYSIWYG word processors, web pages, etc)
 * are more likely to not be fixed-width in most cases.
 *
 * \param font the font to query.
 * \returns true if the font is fixed-width, false otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
(* Const before type ignored *)
function TTF_FontIsFixedWidth(font:PTTF_Font):Tbool;cdecl;external;
{*
 * Query whether a font is scalable or not.
 *
 * Scalability lets us distinguish between outline and bitmap fonts.
 *
 * \param font the font to query
 *
 * \returns true if the font is scalable, false otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_SetFontSDF
  }
(* Const before type ignored *)
function TTF_FontIsScalable(font:PTTF_Font):Tbool;cdecl;external;
{*
 * Query a font's family name.
 *
 * This string is dictated by the contents of the font file.
 *
 * Note that the returned string is to internal storage, and should not be
 * modified or free'd by the caller. The string becomes invalid, with the rest
 * of the font, when `font` is handed to TTF_CloseFont().
 *
 * \param font the font to query.
 * \returns the font's family name.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
(* Const before type ignored *)
(* Const before type ignored *)
function TTF_GetFontFamilyName(font:PTTF_Font):Pchar;cdecl;external;
{*
 * Query a font's style name.
 *
 * This string is dictated by the contents of the font file.
 *
 * Note that the returned string is to internal storage, and should not be
 * modified or free'd by the caller. The string becomes invalid, with the rest
 * of the font, when `font` is handed to TTF_CloseFont().
 *
 * \param font the font to query.
 * \returns the font's style name.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
(* Const before type ignored *)
(* Const before type ignored *)
function TTF_GetFontStyleName(font:PTTF_Font):Pchar;cdecl;external;
{*
 * Direction flags
 *
 * The values here are chosen to match
 * [hb_direction_t](https://harfbuzz.github.io/harfbuzz-hb-common.html#hb-direction-t)
 * .
 *
 * \since This enum is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_SetFontDirection
  }
{*< Left to Right  }
{*< Right to Left  }
{*< Top to Bottom  }
{*< Bottom to Top  }
type
  PTTF_Direction = ^TTTF_Direction;
  TTTF_Direction =  Longint;
  Const
    TTF_DIRECTION_INVALID = 0;
    TTF_DIRECTION_LTR = 4;
    TTF_DIRECTION_RTL = 5;
    TTF_DIRECTION_TTB = 6;
    TTF_DIRECTION_BTT = 7;
;
{*
 * Set the direction to be used for text shaping by a font.
 *
 * This function only supports left-to-right text shaping if SDL_ttf was not
 * built with HarfBuzz support.
 *
 * This updates any TTF_Text objects using this font.
 *
 * \param font the font to modify.
 * \param direction the new direction for text to flow.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }

function TTF_SetFontDirection(font:PTTF_Font; direction:TTTF_Direction):Tbool;cdecl;external;
{*
 * Get the direction to be used for text shaping by a font.
 *
 * This defaults to TTF_DIRECTION_INVALID if it hasn't been set.
 *
 * \param font the font to query.
 * \returns the direction to be used for text shaping.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
function TTF_GetFontDirection(font:PTTF_Font):TTTF_Direction;cdecl;external;
{*
 * Set the script to be used for text shaping by a font.
 *
 * This returns false if SDL_ttf isn't build with HarfBuzz support.
 *
 * This updates any TTF_Text objects using this font.
 *
 * \param font the font to modify.
 * \param script a script tag in the format used by HarfBuzz.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
function TTF_SetFontScript(font:PTTF_Font; script:TUint32):Tbool;cdecl;external;
{*
 * Get the script used for text shaping a font.
 *
 * \param font the font to query.
 * \returns a script tag in the format used by HarfBuzz.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
function TTF_GetFontScript(font:PTTF_Font):TUint32;cdecl;external;
{*
 * Get the script used by a 32-bit codepoint.
 *
 * \param ch the character code to check.
 * \returns a script tag in the format used by HarfBuzz on success, or 0 on
 *          failure; call SDL_GetError() for more information.
 *
 * \threadsafety This function is thread-safe.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
function TTF_GetGlyphScript(ch:TUint32):TUint32;cdecl;external;
{*
 * Set language to be used for text shaping by a font.
 *
 * If SDL_ttf was not built with HarfBuzz support, this function returns false.
 *
 * This updates any TTF_Text objects using this font.
 *
 * \param font the font to specify a language for.
 * \param language_bcp47 a null-terminated string containing the desired language's BCP47 code. Or null to reset the value.
 * \returns true on success or false on failure; call SDL_GetError()
 *          for more information.
 *
 * \threadsafety This function should be called on the thread that created the font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
(* Const before type ignored *)
function TTF_SetFontLanguage(font:PTTF_Font; language_bcp47:Pchar):Tbool;cdecl;external;
{*
 * Check whether a glyph is provided by the font for a UNICODE codepoint.
 *
 * \param font the font to query.
 * \param ch the codepoint to check.
 * \returns true if font provides a glyph for this character, false if not.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
function TTF_FontHasGlyph(font:PTTF_Font; ch:TUint32):Tbool;cdecl;external;
{*
 * The type of data in a glyph image
 *
 * \since This enum is available since SDL_ttf 3.0.0.
  }
{*< The color channels are white  }
{*< The color channels have image data  }
{*< The alpha channel has signed distance field information  }
type
  PTTF_ImageType = ^TTTF_ImageType;
  TTTF_ImageType =  Longint;
  Const
    TTF_IMAGE_INVALID = 0;
    TTF_IMAGE_ALPHA = 1;
    TTF_IMAGE_COLOR = 2;
    TTF_IMAGE_SDF = 3;
;
{*
 * Get the pixel image for a UNICODE codepoint.
 *
 * \param font the font to query.
 * \param ch the codepoint to check.
 * \param image_type a pointer filled in with the glyph image type, may be
 *                   NULL.
 * \returns an SDL_Surface containing the glyph, or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }

function TTF_GetGlyphImage(font:PTTF_Font; ch:TUint32; image_type:PTTF_ImageType):PSDL_Surface;cdecl;external;
{*
 * Get the pixel image for a character index.
 *
 * This is useful for text engine implementations, which can call this with
 * the `glyph_index` in a TTF_CopyOperation
 *
 * \param font the font to query.
 * \param glyph_index the index of the glyph to return.
 * \param image_type a pointer filled in with the glyph image type, may be
 *                   NULL.
 * \returns an SDL_Surface containing the glyph, or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
function TTF_GetGlyphImageForIndex(font:PTTF_Font; glyph_index:TUint32; image_type:PTTF_ImageType):PSDL_Surface;cdecl;external;
{*
 * Query the metrics (dimensions) of a font's glyph for a UNICODE codepoint.
 *
 * To understand what these metrics mean, here is a useful link:
 *
 * https://freetype.sourceforge.net/freetype2/docs/tutorial/step2.html
 *
 * \param font the font to query.
 * \param ch the codepoint to check.
 * \param minx a pointer filled in with the minimum x coordinate of the glyph
 *             from the left edge of its bounding box. This value may be
 *             negative.
 * \param maxx a pointer filled in with the maximum x coordinate of the glyph
 *             from the left edge of its bounding box.
 * \param miny a pointer filled in with the minimum y coordinate of the glyph
 *             from the bottom edge of its bounding box. This value may be
 *             negative.
 * \param maxy a pointer filled in with the maximum y coordinate of the glyph
 *             from the bottom edge of its bounding box.
 * \param advance a pointer filled in with the distance to the next glyph from
 *                the left edge of this glyph's bounding box.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
function TTF_GetGlyphMetrics(font:PTTF_Font; ch:TUint32; minx:Plongint; maxx:Plongint; miny:Plongint; 
           maxy:Plongint; advance:Plongint):Tbool;cdecl;external;
{*
 * Query the kerning size between the glyphs of two UNICODE codepoints.
 *
 * \param font the font to query.
 * \param previous_ch the previous codepoint.
 * \param ch the current codepoint.
 * \param kerning a pointer filled in with the kerning size between the two glyphs, in pixels, may be NULL.
 * \returns true on success or false on failure; call SDL_GetError()
 *          for more information.
 *
 * \threadsafety This function should be called on the thread that created the font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
function TTF_GetGlyphKerning(font:PTTF_Font; previous_ch:TUint32; ch:TUint32; kerning:Plongint):Tbool;cdecl;external;
{*
 * Calculate the dimensions of a rendered string of UTF-8 text.
 *
 * This will report the width and height, in pixels, of the space that the
 * specified string will take to fully render.
 *
 * \param font the font to query.
 * \param text text to calculate, in UTF-8 encoding.
 * \param length the length of the text, in bytes, or 0 for null terminated
 *               text.
 * \param w will be filled with width, in pixels, on return.
 * \param h will be filled with height, in pixels, on return.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
(* Const before type ignored *)
function TTF_GetStringSize(font:PTTF_Font; text:Pchar; length:Tsize_t; w:Plongint; h:Plongint):Tbool;cdecl;external;
{*
 * Calculate the dimensions of a rendered string of UTF-8 text.
 *
 * This will report the width and height, in pixels, of the space that the
 * specified string will take to fully render.
 *
 * Text is wrapped to multiple lines on line endings and on word boundaries if
 * it extends beyond `wrap_width` in pixels.
 *
 * If wrap_width is 0, this function will only wrap on newline characters.
 *
 * \param font the font to query.
 * \param text text to calculate, in UTF-8 encoding.
 * \param length the length of the text, in bytes, or 0 for null terminated
 *               text.
 * \param wrap_width the maximum width or 0 to wrap on newline characters.
 * \param w will be filled with width, in pixels, on return.
 * \param h will be filled with height, in pixels, on return.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
(* Const before type ignored *)
function TTF_GetStringSizeWrapped(font:PTTF_Font; text:Pchar; length:Tsize_t; wrap_width:longint; w:Plongint; 
           h:Plongint):Tbool;cdecl;external;
{*
 * Calculate how much of a UTF-8 string will fit in a given width.
 *
 * This reports the number of characters that can be rendered before reaching
 * `max_width`.
 *
 * This does not need to render the string to do this calculation.
 *
 * \param font the font to query.
 * \param text text to calculate, in UTF-8 encoding.
 * \param length the length of the text, in bytes, or 0 for null terminated
 *               text.
 * \param max_width maximum width, in pixels, available for the string, or 0
 *                  for unbounded width.
 * \param measured_width a pointer filled in with the width, in pixels, of the
 *                       string that will fit, may be NULL.
 * \param measured_length a pointer filled in with the length, in bytes, of
 *                        the string that will fit, may be NULL.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
(* Const before type ignored *)
function TTF_MeasureString(font:PTTF_Font; text:Pchar; length:Tsize_t; max_width:longint; measured_width:Plongint; 
           measured_length:Psize_t):Tbool;cdecl;external;
{*
 * Render UTF-8 text at fast quality to a new 8-bit surface.
 *
 * This function will allocate a new 8-bit, palettized surface. The surface's
 * 0 pixel will be the colorkey, giving a transparent background. The 1 pixel
 * will be set to the text color.
 *
 * This will not word-wrap the string; you'll get a surface with a single line
 * of text, as long as the string requires. You can use
 * TTF_RenderText_Solid_Wrapped() instead if you need to wrap the output to
 * multiple lines.
 *
 * This will not wrap on newline characters.
 *
 * You can render at other quality levels with TTF_RenderText_Shaded,
 * TTF_RenderText_Blended, and TTF_RenderText_LCD.
 *
 * \param font the font to render with.
 * \param text text to render, in UTF-8 encoding.
 * \param length the length of the text, in bytes, or 0 for null terminated
 *               text.
 * \param fg the foreground color for the text.
 * \returns a new 8-bit, palettized surface, or NULL if there was an error.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_RenderText_Blended
 * \sa TTF_RenderText_LCD
 * \sa TTF_RenderText_Shaded
 * \sa TTF_RenderText_Solid
 * \sa TTF_RenderText_Solid_Wrapped
  }
(* Const before type ignored *)
function TTF_RenderText_Solid(font:PTTF_Font; text:Pchar; length:Tsize_t; fg:TSDL_Color):PSDL_Surface;cdecl;external;
{*
 * Render word-wrapped UTF-8 text at fast quality to a new 8-bit surface.
 *
 * This function will allocate a new 8-bit, palettized surface. The surface's
 * 0 pixel will be the colorkey, giving a transparent background. The 1 pixel
 * will be set to the text color.
 *
 * Text is wrapped to multiple lines on line endings and on word boundaries if
 * it extends beyond `wrapLength` in pixels.
 *
 * If wrapLength is 0, this function will only wrap on newline characters.
 *
 * You can render at other quality levels with TTF_RenderText_Shaded_Wrapped,
 * TTF_RenderText_Blended_Wrapped, and TTF_RenderText_LCD_Wrapped.
 *
 * \param font the font to render with.
 * \param text text to render, in UTF-8 encoding.
 * \param length the length of the text, in bytes, or 0 for null terminated
 *               text.
 * \param fg the foreground color for the text.
 * \param wrapLength the maximum width of the text surface or 0 to wrap on
 *                   newline characters.
 * \returns a new 8-bit, palettized surface, or NULL if there was an error.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_RenderText_Blended_Wrapped
 * \sa TTF_RenderText_LCD_Wrapped
 * \sa TTF_RenderText_Shaded_Wrapped
 * \sa TTF_RenderText_Solid
  }
(* Const before type ignored *)
function TTF_RenderText_Solid_Wrapped(font:PTTF_Font; text:Pchar; length:Tsize_t; fg:TSDL_Color; wrapLength:longint):PSDL_Surface;cdecl;external;
{*
 * Render a single 32-bit glyph at fast quality to a new 8-bit surface.
 *
 * This function will allocate a new 8-bit, palettized surface. The surface's
 * 0 pixel will be the colorkey, giving a transparent background. The 1 pixel
 * will be set to the text color.
 *
 * The glyph is rendered without any padding or centering in the X direction,
 * and aligned normally in the Y direction.
 *
 * You can render at other quality levels with TTF_RenderGlyph_Shaded,
 * TTF_RenderGlyph_Blended, and TTF_RenderGlyph_LCD.
 *
 * \param font the font to render with.
 * \param ch the character to render.
 * \param fg the foreground color for the text.
 * \returns a new 8-bit, palettized surface, or NULL if there was an error.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_RenderGlyph_Blended
 * \sa TTF_RenderGlyph_LCD
 * \sa TTF_RenderGlyph_Shaded
  }
function TTF_RenderGlyph_Solid(font:PTTF_Font; ch:TUint32; fg:TSDL_Color):PSDL_Surface;cdecl;external;
{*
 * Render UTF-8 text at high quality to a new 8-bit surface.
 *
 * This function will allocate a new 8-bit, palettized surface. The surface's
 * 0 pixel will be the specified background color, while other pixels have
 * varying degrees of the foreground color. This function returns the new
 * surface, or NULL if there was an error.
 *
 * This will not word-wrap the string; you'll get a surface with a single line
 * of text, as long as the string requires. You can use
 * TTF_RenderText_Shaded_Wrapped() instead if you need to wrap the output to
 * multiple lines.
 *
 * This will not wrap on newline characters.
 *
 * You can render at other quality levels with TTF_RenderText_Solid,
 * TTF_RenderText_Blended, and TTF_RenderText_LCD.
 *
 * \param font the font to render with.
 * \param text text to render, in UTF-8 encoding.
 * \param length the length of the text, in bytes, or 0 for null terminated
 *               text.
 * \param fg the foreground color for the text.
 * \param bg the background color for the text.
 * \returns a new 8-bit, palettized surface, or NULL if there was an error.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_RenderText_Blended
 * \sa TTF_RenderText_LCD
 * \sa TTF_RenderText_Shaded_Wrapped
 * \sa TTF_RenderText_Solid
  }
(* Const before type ignored *)
function TTF_RenderText_Shaded(font:PTTF_Font; text:Pchar; length:Tsize_t; fg:TSDL_Color; bg:TSDL_Color):PSDL_Surface;cdecl;external;
{*
 * Render word-wrapped UTF-8 text at high quality to a new 8-bit surface.
 *
 * This function will allocate a new 8-bit, palettized surface. The surface's
 * 0 pixel will be the specified background color, while other pixels have
 * varying degrees of the foreground color. This function returns the new
 * surface, or NULL if there was an error.
 *
 * Text is wrapped to multiple lines on line endings and on word boundaries if
 * it extends beyond `wrap_width` in pixels.
 *
 * If wrap_width is 0, this function will only wrap on newline characters.
 *
 * You can render at other quality levels with TTF_RenderText_Solid_Wrapped,
 * TTF_RenderText_Blended_Wrapped, and TTF_RenderText_LCD_Wrapped.
 *
 * \param font the font to render with.
 * \param text text to render, in UTF-8 encoding.
 * \param length the length of the text, in bytes, or 0 for null terminated
 *               text.
 * \param fg the foreground color for the text.
 * \param bg the background color for the text.
 * \param wrap_width the maximum width of the text surface or 0 to wrap on
 *                   newline characters.
 * \returns a new 8-bit, palettized surface, or NULL if there was an error.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_RenderText_Blended_Wrapped
 * \sa TTF_RenderText_LCD_Wrapped
 * \sa TTF_RenderText_Shaded
 * \sa TTF_RenderText_Solid_Wrapped
  }
(* Const before type ignored *)
function TTF_RenderText_Shaded_Wrapped(font:PTTF_Font; text:Pchar; length:Tsize_t; fg:TSDL_Color; bg:TSDL_Color; 
           wrap_width:longint):PSDL_Surface;cdecl;external;
{*
 * Render a single UNICODE codepoint at high quality to a new 8-bit surface.
 *
 * This function will allocate a new 8-bit, palettized surface. The surface's
 * 0 pixel will be the specified background color, while other pixels have
 * varying degrees of the foreground color. This function returns the new
 * surface, or NULL if there was an error.
 *
 * The glyph is rendered without any padding or centering in the X direction,
 * and aligned normally in the Y direction.
 *
 * You can render at other quality levels with TTF_RenderGlyph_Solid,
 * TTF_RenderGlyph_Blended, and TTF_RenderGlyph_LCD.
 *
 * \param font the font to render with.
 * \param ch the codepoint to render.
 * \param fg the foreground color for the text.
 * \param bg the background color for the text.
 * \returns a new 8-bit, palettized surface, or NULL if there was an error.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_RenderGlyph_Blended
 * \sa TTF_RenderGlyph_LCD
 * \sa TTF_RenderGlyph_Solid
  }
function TTF_RenderGlyph_Shaded(font:PTTF_Font; ch:TUint32; fg:TSDL_Color; bg:TSDL_Color):PSDL_Surface;cdecl;external;
{*
 * Render UTF-8 text at high quality to a new ARGB surface.
 *
 * This function will allocate a new 32-bit, ARGB surface, using alpha
 * blending to dither the font with the given color. This function returns the
 * new surface, or NULL if there was an error.
 *
 * This will not word-wrap the string; you'll get a surface with a single line
 * of text, as long as the string requires. You can use
 * TTF_RenderText_Blended_Wrapped() instead if you need to wrap the output to
 * multiple lines.
 *
 * This will not wrap on newline characters.
 *
 * You can render at other quality levels with TTF_RenderText_Solid,
 * TTF_RenderText_Shaded, and TTF_RenderText_LCD.
 *
 * \param font the font to render with.
 * \param text text to render, in UTF-8 encoding.
 * \param length the length of the text, in bytes, or 0 for null terminated
 *               text.
 * \param fg the foreground color for the text.
 * \returns a new 32-bit, ARGB surface, or NULL if there was an error.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_RenderText_Blended_Wrapped
 * \sa TTF_RenderText_LCD
 * \sa TTF_RenderText_Shaded
 * \sa TTF_RenderText_Solid
  }
(* Const before type ignored *)
function TTF_RenderText_Blended(font:PTTF_Font; text:Pchar; length:Tsize_t; fg:TSDL_Color):PSDL_Surface;cdecl;external;
{*
 * Render word-wrapped UTF-8 text at high quality to a new ARGB surface.
 *
 * This function will allocate a new 32-bit, ARGB surface, using alpha
 * blending to dither the font with the given color. This function returns the
 * new surface, or NULL if there was an error.
 *
 * Text is wrapped to multiple lines on line endings and on word boundaries if
 * it extends beyond `wrap_width` in pixels.
 *
 * If wrap_width is 0, this function will only wrap on newline characters.
 *
 * You can render at other quality levels with TTF_RenderText_Solid_Wrapped,
 * TTF_RenderText_Shaded_Wrapped, and TTF_RenderText_LCD_Wrapped.
 *
 * \param font the font to render with.
 * \param text text to render, in UTF-8 encoding.
 * \param length the length of the text, in bytes, or 0 for null terminated
 *               text.
 * \param fg the foreground color for the text.
 * \param wrap_width the maximum width of the text surface or 0 to wrap on
 *                   newline characters.
 * \returns a new 32-bit, ARGB surface, or NULL if there was an error.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_RenderText_Blended
 * \sa TTF_RenderText_LCD_Wrapped
 * \sa TTF_RenderText_Shaded_Wrapped
 * \sa TTF_RenderText_Solid_Wrapped
  }
(* Const before type ignored *)
function TTF_RenderText_Blended_Wrapped(font:PTTF_Font; text:Pchar; length:Tsize_t; fg:TSDL_Color; wrap_width:longint):PSDL_Surface;cdecl;external;
{*
 * Render a single UNICODE codepoint at high quality to a new ARGB surface.
 *
 * This function will allocate a new 32-bit, ARGB surface, using alpha
 * blending to dither the font with the given color. This function returns the
 * new surface, or NULL if there was an error.
 *
 * The glyph is rendered without any padding or centering in the X direction,
 * and aligned normally in the Y direction.
 *
 * You can render at other quality levels with TTF_RenderGlyph_Solid,
 * TTF_RenderGlyph_Shaded, and TTF_RenderGlyph_LCD.
 *
 * \param font the font to render with.
 * \param ch the codepoint to render.
 * \param fg the foreground color for the text.
 * \returns a new 32-bit, ARGB surface, or NULL if there was an error.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_RenderGlyph_LCD
 * \sa TTF_RenderGlyph_Shaded
 * \sa TTF_RenderGlyph_Solid
  }
function TTF_RenderGlyph_Blended(font:PTTF_Font; ch:TUint32; fg:TSDL_Color):PSDL_Surface;cdecl;external;
{*
 * Render UTF-8 text at LCD subpixel quality to a new ARGB surface.
 *
 * This function will allocate a new 32-bit, ARGB surface, and render
 * alpha-blended text using FreeType's LCD subpixel rendering. This function
 * returns the new surface, or NULL if there was an error.
 *
 * This will not word-wrap the string; you'll get a surface with a single line
 * of text, as long as the string requires. You can use
 * TTF_RenderText_LCD_Wrapped() instead if you need to wrap the output to
 * multiple lines.
 *
 * This will not wrap on newline characters.
 *
 * You can render at other quality levels with TTF_RenderText_Solid,
 * TTF_RenderText_Shaded, and TTF_RenderText_Blended.
 *
 * \param font the font to render with.
 * \param text text to render, in UTF-8 encoding.
 * \param length the length of the text, in bytes, or 0 for null terminated
 *               text.
 * \param fg the foreground color for the text.
 * \param bg the background color for the text.
 * \returns a new 32-bit, ARGB surface, or NULL if there was an error.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_RenderText_Blended
 * \sa TTF_RenderText_LCD_Wrapped
 * \sa TTF_RenderText_Shaded
 * \sa TTF_RenderText_Solid
  }
(* Const before type ignored *)
function TTF_RenderText_LCD(font:PTTF_Font; text:Pchar; length:Tsize_t; fg:TSDL_Color; bg:TSDL_Color):PSDL_Surface;cdecl;external;
{*
 * Render word-wrapped UTF-8 text at LCD subpixel quality to a new ARGB
 * surface.
 *
 * This function will allocate a new 32-bit, ARGB surface, and render
 * alpha-blended text using FreeType's LCD subpixel rendering. This function
 * returns the new surface, or NULL if there was an error.
 *
 * Text is wrapped to multiple lines on line endings and on word boundaries if
 * it extends beyond `wrap_width` in pixels.
 *
 * If wrap_width is 0, this function will only wrap on newline characters.
 *
 * You can render at other quality levels with TTF_RenderText_Solid_Wrapped,
 * TTF_RenderText_Shaded_Wrapped, and TTF_RenderText_Blended_Wrapped.
 *
 * \param font the font to render with.
 * \param text text to render, in UTF-8 encoding.
 * \param length the length of the text, in bytes, or 0 for null terminated
 *               text.
 * \param fg the foreground color for the text.
 * \param bg the background color for the text.
 * \param wrap_width the maximum width of the text surface or 0 to wrap on
 *                   newline characters.
 * \returns a new 32-bit, ARGB surface, or NULL if there was an error.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_RenderText_Blended_Wrapped
 * \sa TTF_RenderText_LCD
 * \sa TTF_RenderText_Shaded_Wrapped
 * \sa TTF_RenderText_Solid_Wrapped
  }
(* Const before type ignored *)
function TTF_RenderText_LCD_Wrapped(font:PTTF_Font; text:Pchar; length:Tsize_t; fg:TSDL_Color; bg:TSDL_Color; 
           wrap_width:longint):PSDL_Surface;cdecl;external;
{*
 * Render a single UNICODE codepoint at LCD subpixel quality to a new ARGB
 * surface.
 *
 * This function will allocate a new 32-bit, ARGB surface, and render
 * alpha-blended text using FreeType's LCD subpixel rendering. This function
 * returns the new surface, or NULL if there was an error.
 *
 * The glyph is rendered without any padding or centering in the X direction,
 * and aligned normally in the Y direction.
 *
 * You can render at other quality levels with TTF_RenderGlyph_Solid,
 * TTF_RenderGlyph_Shaded, and TTF_RenderGlyph_Blended.
 *
 * \param font the font to render with.
 * \param ch the codepoint to render.
 * \param fg the foreground color for the text.
 * \param bg the background color for the text.
 * \returns a new 32-bit, ARGB surface, or NULL if there was an error.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_RenderGlyph_Blended
 * \sa TTF_RenderGlyph_Shaded
 * \sa TTF_RenderGlyph_Solid
  }
function TTF_RenderGlyph_LCD(font:PTTF_Font; ch:TUint32; fg:TSDL_Color; bg:TSDL_Color):PSDL_Surface;cdecl;external;
{*
 * A text engine used to create text objects.
 *
 * This is a public interface that can be used by applications and libraries
 * to perform customize rendering with text objects. See
 * <SDL3_ttf/SDL_textengine.h> for details.
 *
 * There are three text engines provided with the library:
 *
 * - Drawing to an SDL_Surface, created with TTF_CreateSurfaceTextEngine()
 * - Drawing with an SDL 2D renderer, created with
 *   TTF_CreateRendererTextEngine()
 * - Drawing with the SDL GPU API, created with TTF_CreateGPUTextEngine()
 *
 * \since This struct is available since SDL_ttf 3.0.0.
  }
type
{*
 * Internal data for TTF_Text
 *
 * \since This struct is available since SDL_ttf 3.0.0.
  }
{*
 * Text created with TTF_CreateText()
 *
 * \since This struct is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_CreateText
 * \sa TTF_GetTextProperties
 * \sa TTF_DestroyText
  }
{*< A copy of the UTF-8 string that this text object represents, useful for layout, debugging and retrieving substring text. This is updated when the text object is modified and will be freed automatically when the object is destroyed.  }
{*< The number of lines in the text, 0 if it's empty  }
{*< Application reference count, used when freeing surface  }
{*< Private  }

  PTTF_Text = ^TTTF_Text;
  TTTF_Text = record
      text : Pchar;
      num_lines : longint;
      refcount : longint;
      internal : PTTF_TextData;
    end;
{*
 * Create a text engine for drawing text on SDL surfaces.
 *
 * \returns a TTF_TextEngine object or NULL on failure; call SDL_GetError()
 *          for more information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_DestroySurfaceTextEngine
 * \sa TTF_DrawSurfaceText
  }

function TTF_CreateSurfaceTextEngine:PTTF_TextEngine;cdecl;external;
{*
 * Draw text to an SDL surface.
 *
 * `text` must have been created using a TTF_TextEngine from
 * TTF_CreateSurfaceTextEngine().
 *
 * \param text the text to draw.
 * \param x the x coordinate in pixels, positive from the left edge towards
 *          the right.
 * \param y the y coordinate in pixels, positive from the top edge towards the
 *          bottom.
 * \param surface the surface to draw on.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_CreateSurfaceTextEngine
 * \sa TTF_CreateText
  }
function TTF_DrawSurfaceText(text:PTTF_Text; x:longint; y:longint; surface:PSDL_Surface):Tbool;cdecl;external;
{*
 * Destroy a text engine created for drawing text on SDL surfaces.
 *
 * All text created by this engine should be destroyed before calling this
 * function.
 *
 * \param engine a TTF_TextEngine object created with
 *               TTF_CreateSurfaceTextEngine().
 *
 * \threadsafety This function should be called on the thread that created the
 *               engine.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_CreateSurfaceTextEngine
  }
procedure TTF_DestroySurfaceTextEngine(engine:PTTF_TextEngine);cdecl;external;
{*
 * Create a text engine for drawing text on an SDL renderer.
 *
 * \param renderer the renderer to use for creating textures and drawing text.
 * \returns a TTF_TextEngine object or NULL on failure; call SDL_GetError()
 *          for more information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               renderer.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_DestroyRendererTextEngine
  }
function TTF_CreateRendererTextEngine(renderer:PSDL_Renderer):PTTF_TextEngine;cdecl;external;
{*
 * Create a text engine for drawing text on an SDL renderer, with the
 * specified properties.
 *
 * These are the supported properties:
 *
 * - `TTF_PROP_RENDERER_TEXT_ENGINE_RENDERER`: the renderer to use for
 *   creating textures and drawing text
 * - `TTF_PROP_RENDERER_TEXT_ENGINE_ATLAS_TEXTURE_SIZE`: the size of the
 *   texture atlas
 *
 * \param props the properties to use.
 * \returns a TTF_TextEngine object or NULL on failure; call SDL_GetError()
 *          for more information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               renderer.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_DestroyRendererTextEngine
  }
function TTF_CreateRendererTextEngineWithProperties(props:TSDL_PropertiesID):PTTF_TextEngine;cdecl;external;
const
  TTF_PROP_RENDERER_TEXT_ENGINE_RENDERER = 'SDL_ttf.renderer_text_engine.create.renderer';  
  TTF_PROP_RENDERER_TEXT_ENGINE_ATLAS_TEXTURE_SIZE = 'SDL_ttf.renderer_text_engine.create.atlas_texture_size';  
{*
 * Draw text to an SDL renderer.
 *
 * `text` must have been created using a TTF_TextEngine from
 * TTF_CreateRendererTextEngine(), and will draw using the renderer passed to
 * that function.
 *
 * \param text the text to draw.
 * \param x the x coordinate in pixels, positive from the left edge towards
 *          the right.
 * \param y the y coordinate in pixels, positive from the top edge towards the
 *          bottom.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_CreateRendererTextEngine
 * \sa TTF_CreateText
  }

function TTF_DrawRendererText(text:PTTF_Text; x:single; y:single):Tbool;cdecl;external;
{*
 * Destroy a text engine created for drawing text on an SDL renderer.
 *
 * All text created by this engine should be destroyed before calling this
 * function.
 *
 * \param engine a TTF_TextEngine object created with
 *               TTF_CreateRendererTextEngine().
 *
 * \threadsafety This function should be called on the thread that created the
 *               engine.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_CreateRendererTextEngine
  }
procedure TTF_DestroyRendererTextEngine(engine:PTTF_TextEngine);cdecl;external;
{*
 * Create a text engine for drawing text with the SDL GPU API.
 *
 * \param device the SDL_GPUDevice to use for creating textures and drawing
 *               text.
 * \returns a TTF_TextEngine object or NULL on failure; call SDL_GetError()
 *          for more information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               device.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_DestroyGPUTextEngine
  }
function TTF_CreateGPUTextEngine(device:PSDL_GPUDevice):PTTF_TextEngine;cdecl;external;
{*
 * Create a text engine for drawing text with the SDL GPU API, with the
 * specified properties.
 *
 * These are the supported properties:
 *
 * - `TTF_PROP_GPU_TEXT_ENGINE_DEVICE`: the SDL_GPUDevice to use for creating
 *   textures and drawing text.
 * - `TTF_PROP_GPU_TEXT_ENGINE_ATLAS_TEXTURE_SIZE`: the size of the texture
 *   atlas
 *
 * \param props the properties to use.
 * \returns a TTF_TextEngine object or NULL on failure; call SDL_GetError()
 *          for more information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               device.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_DestroyGPUTextEngine
  }
function TTF_CreateGPUTextEngineWithProperties(props:TSDL_PropertiesID):PTTF_TextEngine;cdecl;external;
const
  TTF_PROP_GPU_TEXT_ENGINE_DEVICE = 'SDL_ttf.gpu_text_engine.create.device';  
  TTF_PROP_GPU_TEXT_ENGINE_ATLAS_TEXTURE_SIZE = 'SDL_ttf.gpu_text_engine.create.atlas_texture_size';  
{*
 * Draw sequence returned by TTF_GetGPUTextDrawData
 *
 * \since This struct is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_GetGPUTextDrawData
  }
{*< Texture atlas that stores the glyphs  }
{*< An array of vertex positions  }
{*< An array of normalized texture coordinates for each vertex  }
{*< Number of vertices  }
{*< An array of indices into the 'vertices' arrays  }
{*< Number of indices  }
{*< The image type of this draw sequence  }
{*< The next sequence (will be NULL in case of the last sequence)  }
type
  PTTF_GPUAtlasDrawSequence = ^TTTF_GPUAtlasDrawSequence;
  TTTF_GPUAtlasDrawSequence = record
      atlas_texture : PSDL_GPUTexture;
      xy : PSDL_FPoint;
      uv : PSDL_FPoint;
      num_vertices : longint;
      indices : Plongint;
      num_indices : longint;
      image_type : TTTF_ImageType;
      next : PTTF_GPUAtlasDrawSequence;
    end;
{*
 * Get the geometry data needed for drawing the text.
 *
 * `text` must have been created using a TTF_TextEngine from
 * TTF_CreateGPUTextEngine().
 *
 * The positive X-axis is taken towards the right and the positive Y-axis is
 * taken upwards for both the vertex and the texture coordinates, i.e, it
 * follows the same convention used by the SDL_GPU API. If you want to use a
 * different coordinate system you will need to transform the vertices
 * yourself.
 *
 * If the text looks blocky use linear filtering.
 *
 * \param text the text to draw.
 * \returns a NULL terminated linked list of TTF_GPUAtlasDrawSequence objects
 *          or NULL if the passed text is empty or in case of failure; call
 *          SDL_GetError() for more information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_CreateGPUTextEngine
 * \sa TTF_CreateText
  }

function TTF_GetGPUTextDrawData(text:PTTF_Text):PTTF_GPUAtlasDrawSequence;cdecl;external;
{*
 * Destroy a text engine created for drawing text with the SDL GPU API.
 *
 * All text created by this engine should be destroyed before calling this
 * function.
 *
 * \param engine a TTF_TextEngine object created with
 *               TTF_CreateGPUTextEngine().
 *
 * \threadsafety This function should be called on the thread that created the
 *               engine.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_CreateGPUTextEngine
  }
procedure TTF_DestroyGPUTextEngine(engine:PTTF_TextEngine);cdecl;external;
{*
 * The winding order of the vertices returned by TTF_GetGPUTextDrawData
 *
 * \since This enum is available since SDL_ttf 3.0.0.
  }
type
  PTTF_GPUTextEngineWinding = ^TTTF_GPUTextEngineWinding;
  TTTF_GPUTextEngineWinding =  Longint;
  Const
    TTF_GPU_TEXTENGINE_WINDING_INVALID = -(1);
    TTF_GPU_TEXTENGINE_WINDING_CLOCKWISE = (-(1))+1;
    TTF_GPU_TEXTENGINE_WINDING_COUNTER_CLOCKWISE = (-(1))+2;
;
{*
 * Sets the winding order of the vertices returned by TTF_GetGPUTextDrawData
 * for a particular GPU text engine.
 *
 * \param engine a TTF_TextEngine object created with
 *               TTF_CreateGPUTextEngine().
 * \param winding the new winding order option.
 *
 * \threadsafety This function should be called on the thread that created the
 *               engine.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_GetGPUTextEngineWinding
  }

procedure TTF_SetGPUTextEngineWinding(engine:PTTF_TextEngine; winding:TTTF_GPUTextEngineWinding);cdecl;external;
{*
 * Get the winding order of the vertices returned by TTF_GetGPUTextDrawData
 * for a particular GPU text engine
 *
 * \param engine a TTF_TextEngine object created with
 *               TTF_CreateGPUTextEngine().
 * \returns the winding order used by the GPU text engine or
 *          TTF_GPU_TEXTENGINE_WINDING_INVALID in case of error.
 *
 * \threadsafety This function should be called on the thread that created the
 *               engine.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_SetGPUTextEngineWinding
  }
(* Const before type ignored *)
function TTF_GetGPUTextEngineWinding(engine:PTTF_TextEngine):TTTF_GPUTextEngineWinding;cdecl;external;
{*
 * Create a text object from UTF-8 text and a text engine.
 *
 * \param engine the text engine to use when creating the text object, may be
 *               NULL.
 * \param font the font to render with.
 * \param text the text to use, in UTF-8 encoding.
 * \param length the length of the text, in bytes, or 0 for null terminated
 *               text.
 * \returns a TTF_Text object or NULL on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               font and text engine.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_DestroyText
  }
(* Const before type ignored *)
function TTF_CreateText(engine:PTTF_TextEngine; font:PTTF_Font; text:Pchar; length:Tsize_t):PTTF_Text;cdecl;external;
{*
 * Get the properties associated with a text object.
 *
 * \param text the TTF_Text to query.
 * \returns a valid property ID on success or 0 on failure; call
 *          SDL_GetError() for more information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
function TTF_GetTextProperties(text:PTTF_Text):TSDL_PropertiesID;cdecl;external;
{*
 * Set the text engine used by a text object.
 *
 * This function may cause the internal text representation to be rebuilt.
 *
 * \param text the TTF_Text to modify.
 * \param engine the text engine to use for drawing.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_GetTextEngine
  }
function TTF_SetTextEngine(text:PTTF_Text; engine:PTTF_TextEngine):Tbool;cdecl;external;
{*
 * Get the text engine used by a text object.
 *
 * \param text the TTF_Text to query.
 * \returns the TTF_TextEngine used by the text on success or NULL on failure;
 *          call SDL_GetError() for more information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_SetTextEngine
  }
function TTF_GetTextEngine(text:PTTF_Text):PTTF_TextEngine;cdecl;external;
{*
 * Set the font used by a text object.
 *
 * When a text object has a font, any changes to the font will automatically
 * regenerate the text. If you set the font to NULL, the text will continue to
 * render but changes to the font will no longer affect the text.
 *
 * This function may cause the internal text representation to be rebuilt.
 *
 * \param text the TTF_Text to modify.
 * \param font the font to use, may be NULL.
 * \returns false if the text pointer is null; otherwise, true. call
 *          SDL_GetError() for more information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_GetTextFont
  }
function TTF_SetTextFont(text:PTTF_Text; font:PTTF_Font):Tbool;cdecl;external;
{*
 * Get the font used by a text object.
 *
 * \param text the TTF_Text to query.
 * \returns the TTF_Font used by the text on success or NULL on failure; call
 *          SDL_GetError() for more information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_SetTextFont
  }
function TTF_GetTextFont(text:PTTF_Text):PTTF_Font;cdecl;external;
{*
 * Set the direction to be used for text shaping a text object.
 *
 * This function only supports left-to-right text shaping if SDL_ttf was not
 * built with HarfBuzz support.
 *
 * \param text the text to modify.
 * \param direction the new direction for text to flow.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
function TTF_SetTextDirection(text:PTTF_Text; direction:TTTF_Direction):Tbool;cdecl;external;
{*
 * Get the direction to be used for text shaping a text object.
 *
 * This defaults to the direction of the font used by the text object.
 *
 * \param text the text to query.
 * \returns the direction to be used for text shaping.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
function TTF_GetTextDirection(text:PTTF_Text):TTTF_Direction;cdecl;external;
{*
 * Set the script to be used for text shaping a text object.
 *
 * This returns false if SDL_ttf isn't build with HarfBuzz support.
 *
 * \param text the text to modify.
 * \param script a script tag in the format used by HarfBuzz.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
function TTF_SetTextScript(text:PTTF_Text; script:TUint32):Tbool;cdecl;external;
{*
 * Get the script used for text shaping a text object.
 *
 * This defaults to the script of the font used by the text object.
 *
 * \param text the text to query.
 * \returns a script tag in the format used by HarfBuzz.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
function TTF_GetTextScript(text:PTTF_Text):TUint32;cdecl;external;
{*
 * Set the color of a text object.
 *
 * The default text color is white (255, 255, 255, 255).
 *
 * \param text the TTF_Text to modify.
 * \param r the red color value in the range of 0-255.
 * \param g the green color value in the range of 0-255.
 * \param b the blue color value in the range of 0-255.
 * \param a the alpha value in the range of 0-255.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_GetTextColor
 * \sa TTF_SetTextColorFloat
  }
function TTF_SetTextColor(text:PTTF_Text; r:TUint8; g:TUint8; b:TUint8; a:TUint8):Tbool;cdecl;external;
{*
 * Set the color of a text object.
 *
 * The default text color is white (1.0f, 1.0f, 1.0f, 1.0f).
 *
 * \param text the TTF_Text to modify.
 * \param r the red color value, normally in the range of 0-1.
 * \param g the green color value, normally in the range of 0-1.
 * \param b the blue color value, normally in the range of 0-1.
 * \param a the alpha value in the range of 0-1.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_GetTextColorFloat
 * \sa TTF_SetTextColor
  }
function TTF_SetTextColorFloat(text:PTTF_Text; r:single; g:single; b:single; a:single):Tbool;cdecl;external;
{*
 * Get the color of a text object.
 *
 * \param text the TTF_Text to query.
 * \param r a pointer filled in with the red color value in the range of
 *          0-255, may be NULL.
 * \param g a pointer filled in with the green color value in the range of
 *          0-255, may be NULL.
 * \param b a pointer filled in with the blue color value in the range of
 *          0-255, may be NULL.
 * \param a a pointer filled in with the alpha value in the range of 0-255,
 *          may be NULL.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_GetTextColorFloat
 * \sa TTF_SetTextColor
  }
function TTF_GetTextColor(text:PTTF_Text; r:PUint8; g:PUint8; b:PUint8; a:PUint8):Tbool;cdecl;external;
{*
 * Get the color of a text object.
 *
 * \param text the TTF_Text to query.
 * \param r a pointer filled in with the red color value, normally in the
 *          range of 0-1, may be NULL.
 * \param g a pointer filled in with the green color value, normally in the
 *          range of 0-1, may be NULL.
 * \param b a pointer filled in with the blue color value, normally in the
 *          range of 0-1, may be NULL.
 * \param a a pointer filled in with the alpha value in the range of 0-1, may
 *          be NULL.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_GetTextColor
 * \sa TTF_SetTextColorFloat
  }
function TTF_GetTextColorFloat(text:PTTF_Text; r:Psingle; g:Psingle; b:Psingle; a:Psingle):Tbool;cdecl;external;
{*
 * Set the position of a text object.
 *
 * This can be used to position multiple text objects within a single wrapping
 * text area.
 *
 * This function may cause the internal text representation to be rebuilt.
 *
 * \param text the TTF_Text to modify.
 * \param x the x offset of the upper left corner of this text in pixels.
 * \param y the y offset of the upper left corner of this text in pixels.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_GetTextPosition
  }
function TTF_SetTextPosition(text:PTTF_Text; x:longint; y:longint):Tbool;cdecl;external;
{*
 * Get the position of a text object.
 *
 * \param text the TTF_Text to query.
 * \param x a pointer filled in with the x offset of the upper left corner of
 *          this text in pixels, may be NULL.
 * \param y a pointer filled in with the y offset of the upper left corner of
 *          this text in pixels, may be NULL.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_SetTextPosition
  }
function TTF_GetTextPosition(text:PTTF_Text; x:Plongint; y:Plongint):Tbool;cdecl;external;
{*
 * Set whether wrapping is enabled on a text object.
 *
 * This function may cause the internal text representation to be rebuilt.
 *
 * \param text the TTF_Text to modify.
 * \param wrap_width the maximum width in pixels, 0 to wrap on newline
 *                   characters.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_GetTextWrapWidth
  }
function TTF_SetTextWrapWidth(text:PTTF_Text; wrap_width:longint):Tbool;cdecl;external;
{*
 * Get whether wrapping is enabled on a text object.
 *
 * \param text the TTF_Text to query.
 * \param wrap_width a pointer filled in with the maximum width in pixels or 0
 *                   if the text is being wrapped on newline characters.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_SetTextWrapWidth
  }
function TTF_GetTextWrapWidth(text:PTTF_Text; wrap_width:Plongint):Tbool;cdecl;external;
{*
 * Set whether whitespace should be visible when wrapping a text object.
 *
 * If the whitespace is visible, it will take up space for purposes of
 * alignment and wrapping. This is good for editing, but looks better when
 * centered or aligned if whitespace around line wrapping is hidden. This
 * defaults false.
 *
 * This function may cause the internal text representation to be rebuilt.
 *
 * \param text the TTF_Text to modify.
 * \param visible true to show whitespace when wrapping text, false to hide
 *                it.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_TextWrapWhitespaceVisible
  }
function TTF_SetTextWrapWhitespaceVisible(text:PTTF_Text; visible:Tbool):Tbool;cdecl;external;
{*
 * Return whether whitespace is shown when wrapping a text object.
 *
 * \param text the TTF_Text to query.
 * \returns true if whitespace is shown when wrapping text, or false
 *          otherwise.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_SetTextWrapWhitespaceVisible
  }
function TTF_TextWrapWhitespaceVisible(text:PTTF_Text):Tbool;cdecl;external;
{*
 * Set the UTF-8 text used by a text object.
 *
 * This function may cause the internal text representation to be rebuilt.
 *
 * \param text the TTF_Text to modify.
 * \param string the UTF-8 text to use, may be NULL.
 * \param length the length of the text, in bytes, or 0 for null terminated
 *               text.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_AppendTextString
 * \sa TTF_DeleteTextString
 * \sa TTF_InsertTextString
  }
(* Const before type ignored *)
function TTF_SetTextString(text:PTTF_Text; _string:Pchar; length:Tsize_t):Tbool;cdecl;external;
{*
 * Insert UTF-8 text into a text object.
 *
 * This function may cause the internal text representation to be rebuilt.
 *
 * \param text the TTF_Text to modify.
 * \param offset the offset, in bytes, from the beginning of the string if >=
 *               0, the offset from the end of the string if < 0. Note that
 *               this does not do UTF-8 validation, so you should only insert
 *               at UTF-8 sequence boundaries.
 * \param string the UTF-8 text to insert.
 * \param length the length of the text, in bytes, or 0 for null terminated
 *               text.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_AppendTextString
 * \sa TTF_DeleteTextString
 * \sa TTF_SetTextString
  }
(* Const before type ignored *)
function TTF_InsertTextString(text:PTTF_Text; offset:longint; _string:Pchar; length:Tsize_t):Tbool;cdecl;external;
{*
 * Append UTF-8 text to a text object.
 *
 * This function may cause the internal text representation to be rebuilt.
 *
 * \param text the TTF_Text to modify.
 * \param string the UTF-8 text to insert.
 * \param length the length of the text, in bytes, or 0 for null terminated
 *               text.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_DeleteTextString
 * \sa TTF_InsertTextString
 * \sa TTF_SetTextString
  }
(* Const before type ignored *)
function TTF_AppendTextString(text:PTTF_Text; _string:Pchar; length:Tsize_t):Tbool;cdecl;external;
{*
 * Delete UTF-8 text from a text object.
 *
 * This function may cause the internal text representation to be rebuilt.
 *
 * \param text the TTF_Text to modify.
 * \param offset the offset, in bytes, from the beginning of the string if >=
 *               0, the offset from the end of the string if < 0. Note that
 *               this does not do UTF-8 validation, so you should only delete
 *               at UTF-8 sequence boundaries.
 * \param length the length of text to delete, in bytes, or -1 for the
 *               remainder of the string.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_AppendTextString
 * \sa TTF_InsertTextString
 * \sa TTF_SetTextString
  }
function TTF_DeleteTextString(text:PTTF_Text; offset:longint; length:longint):Tbool;cdecl;external;
{*
 * Get the size of a text object.
 *
 * The size of the text may change when the font or font style and size
 * change.
 *
 * \param text the TTF_Text to query.
 * \param w a pointer filled in with the width of the text, in pixels, may be
 *          NULL.
 * \param h a pointer filled in with the height of the text, in pixels, may be
 *          NULL.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
function TTF_GetTextSize(text:PTTF_Text; w:Plongint; h:Plongint):Tbool;cdecl;external;
{*
 * Flags for TTF_SubString
 *
 * \since This datatype is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_SubString
  }
type
  PTTF_SubStringFlags = ^TTTF_SubStringFlags;
  TTTF_SubStringFlags = TUint32;
{*< The mask for the flow direction for this substring  }

const
  TTF_SUBSTRING_DIRECTION_MASK = $000000FF;  
{*< This substring contains the beginning of the text  }
  TTF_SUBSTRING_TEXT_START = $00000100;  
{*< This substring contains the beginning of line `line_index`  }
  TTF_SUBSTRING_LINE_START = $00000200;  
{*< This substring contains the end of line `line_index`  }
  TTF_SUBSTRING_LINE_END = $00000400;  
{*< This substring contains the end of the text  }
  TTF_SUBSTRING_TEXT_END = $00000800;  
{*
 * The representation of a substring within text.
 *
 * \since This struct is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_GetNextTextSubString
 * \sa TTF_GetPreviousTextSubString
 * \sa TTF_GetTextSubString
 * \sa TTF_GetTextSubStringForLine
 * \sa TTF_GetTextSubStringForPoint
 * \sa TTF_GetTextSubStringsForRange
  }
{*< The flags for this substring  }
{*< The byte offset from the beginning of the text  }
{*< The byte length starting at the offset  }
{*< The index of the line that contains this substring  }
{*< The internal cluster index, used for quickly iterating  }
{*< The rectangle, relative to the top left of the text, containing the substring  }
type
  PTTF_SubString = ^TTTF_SubString;
  TTTF_SubString = record
      flags : TTTF_SubStringFlags;
      offset : longint;
      length : longint;
      line_index : longint;
      cluster_index : longint;
      rect : TSDL_Rect;
    end;
{*
 * Get the substring of a text object that surrounds a text offset.
 *
 * If `offset` is less than 0, this will return a zero length substring at the
 * beginning of the text with the TTF_SUBSTRING_TEXT_START flag set. If
 * `offset` is greater than or equal to the length of the text string, this
 * will return a zero length substring at the end of the text with the
 * TTF_SUBSTRING_TEXT_END flag set.
 *
 * \param text the TTF_Text to query.
 * \param offset a byte offset into the text string.
 * \param substring a pointer filled in with the substring containing the
 *                  offset.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }

function TTF_GetTextSubString(text:PTTF_Text; offset:longint; substring:PTTF_SubString):Tbool;cdecl;external;
{*
 * Get the substring of a text object that contains the given line.
 *
 * If `line` is less than 0, this will return a zero length substring at the
 * beginning of the text with the TTF_SUBSTRING_TEXT_START flag set. If `line`
 * is greater than or equal to `text->num_lines` this will return a zero
 * length substring at the end of the text with the TTF_SUBSTRING_TEXT_END
 * flag set.
 *
 * \param text the TTF_Text to query.
 * \param line a zero-based line index, in the range [0 .. text->num_lines-1].
 * \param substring a pointer filled in with the substring containing the
 *                  offset.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
function TTF_GetTextSubStringForLine(text:PTTF_Text; line:longint; substring:PTTF_SubString):Tbool;cdecl;external;
{*
 * Get the substrings of a text object that contain a range of text.
 *
 * \param text the TTF_Text to query.
 * \param offset a byte offset into the text string.
 * \param length the length of the range being queried, in bytes, or -1 for
 *               the remainder of the string.
 * \param count a pointer filled in with the number of substrings returned,
 *              may be NULL.
 * \returns a NULL terminated array of substring pointers or NULL on failure;
 *          call SDL_GetError() for more information. This is a single
 *          allocation that should be freed with SDL_free() when it is no
 *          longer needed.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
function TTF_GetTextSubStringsForRange(text:PTTF_Text; offset:longint; length:longint; count:Plongint):^PTTF_SubString;cdecl;external;
{*
 * Get the portion of a text string that is closest to a point.
 *
 * This will return the closest substring of text to the given point.
 *
 * \param text the TTF_Text to query.
 * \param x the x coordinate relative to the left side of the text, may be
 *          outside the bounds of the text area.
 * \param y the y coordinate relative to the top side of the text, may be
 *          outside the bounds of the text area.
 * \param substring a pointer filled in with the closest substring of text to
 *                  the given point.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
function TTF_GetTextSubStringForPoint(text:PTTF_Text; x:longint; y:longint; substring:PTTF_SubString):Tbool;cdecl;external;
{*
 * Get the previous substring in a text object
 *
 * If called at the start of the text, this will return a zero length
 * substring with the TTF_SUBSTRING_TEXT_START flag set.
 *
 * \param text the TTF_Text to query.
 * \param substring the TTF_SubString to query.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
(* Const before type ignored *)
function TTF_GetPreviousTextSubString(text:PTTF_Text; substring:PTTF_SubString; previous:PTTF_SubString):Tbool;cdecl;external;
{*
 * Get the next substring in a text object
 *
 * If called at the end of the text, this will return a zero length substring
 * with the TTF_SUBSTRING_TEXT_END flag set.
 *
 * \param text the TTF_Text to query.
 * \param substring the TTF_SubString to query.
 * \param next a pointer filled in with the next substring.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
(* Const before type ignored *)
function TTF_GetNextTextSubString(text:PTTF_Text; substring:PTTF_SubString; next:PTTF_SubString):Tbool;cdecl;external;
{*
 * Update the layout of a text object.
 *
 * This is automatically done when the layout is requested or the text is
 * rendered, but you can call this if you need more control over the timing of
 * when the layout and text engine representation are updated.
 *
 * \param text the TTF_Text to update.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
function TTF_UpdateText(text:PTTF_Text):Tbool;cdecl;external;
{*
 * Destroy a text object created by a text engine.
 *
 * \param text the text to destroy.
 *
 * \threadsafety This function should be called on the thread that created the
 *               text.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_CreateText
  }
procedure TTF_DestroyText(text:PTTF_Text);cdecl;external;
{*
 * Dispose of a previously-created font.
 *
 * Call this when done with a font. This function will free any resources
 * associated with it. It is safe to call this function on NULL, for example
 * on the result of a failed call to TTF_OpenFont().
 *
 * The font is not valid after being passed to this function. String pointers
 * from functions that return information on this font, such as
 * TTF_GetFontFamilyName() and TTF_GetFontStyleName(), are no longer valid
 * after this call, as well.
 *
 * \param font the font to dispose of.
 *
 * \threadsafety This function should not be called while any other thread is
 *               using the font.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_OpenFont
 * \sa TTF_OpenFontIO
  }
procedure TTF_CloseFont(font:PTTF_Font);cdecl;external;
{*
 * Deinitialize SDL_ttf.
 *
 * You must call this when done with the library, to free internal resources.
 * It is safe to call this when the library isn't initialized, as it will just
 * return immediately.
 *
 * Once you have as many quit calls as you have had successful calls to
 * TTF_Init, the library will actually deinitialize.
 *
 * Please note that this does not automatically close any fonts that are still
 * open at the time of deinitialization, and it is possibly not safe to close
 * them afterwards, as parts of the library will no longer be initialized to
 * deal with it. A well-written program should call TTF_CloseFont() on any
 * open fonts before calling this function!
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
  }
procedure TTF_Quit;cdecl;external;
{*
 * Check if SDL_ttf is initialized.
 *
 * This reports the number of times the library has been initialized by a call
 * to TTF_Init(), without a paired deinitialization request from TTF_Quit().
 *
 * In short: if it's greater than zero, the library is currently initialized
 * and ready to work. If zero, it is not initialized.
 *
 * Despite the return value being a signed integer, this function should not
 * return a negative number.
 *
 * \returns the current number of initialization calls, that need to
 *          eventually be paired with this many calls to TTF_Quit().
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_Init
 * \sa TTF_Quit
  }
function TTF_WasInit:longint;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_TTF_H_  }

implementation

{ was #define dname def_expr }
function SDL_TTF_VERSION : longint; { return type might be wrong }
  begin
    SDL_TTF_VERSION:=SDL_VERSIONNUM(SDL_TTF_MAJOR_VERSION,SDL_TTF_MINOR_VERSION,SDL_TTF_MICRO_VERSION);
  end;


end.
