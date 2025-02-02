
unit SDL_textengine;
interface

{
  Automatically converted by H2Pas 1.0.0 from SDL_textengine.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_textengine.h
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
PTTF_CopyOperation  = ^TTF_CopyOperation;
PTTF_DrawCommand  = ^TTF_DrawCommand;
PTTF_DrawOperation  = ^TTF_DrawOperation;
PTTF_FillOperation  = ^TTF_FillOperation;
PTTF_Font  = ^TTF_Font;
PTTF_SubString  = ^TTF_SubString;
PTTF_Text  = ^TTF_Text;
PTTF_TextData  = ^TTF_TextData;
PTTF_TextEngine  = ^TTF_TextEngine;
PTTF_TextLayout  = ^TTF_TextLayout;
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
{*
 *  \file SDL_textengine.h
 *
 * Definitions for implementations of the TTF_TextEngine interface.
  }
{$ifndef SDL_TTF_TEXTENGINE_H_}
{$define SDL_TTF_TEXTENGINE_H_}
{$include <SDL3/SDL.h>}
{$include <SDL3_ttf/SDL_ttf.h>}
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{*
 * A font atlas draw command.
 *
 * \since This enum is available since SDL_ttf 3.0.0.
  }
type
  PTTF_DrawCommand = ^TTTF_DrawCommand;
  TTTF_DrawCommand =  Longint;
  Const
    TTF_DRAW_COMMAND_NOOP = 0;
    TTF_DRAW_COMMAND_FILL = 1;
    TTF_DRAW_COMMAND_COPY = 2;
;
{*
 * A filled rectangle draw operation.
 *
 * \since This struct is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_DrawOperation
  }
{*< TTF_DRAW_COMMAND_FILL  }
{*< The rectangle to fill, in pixels. The x coordinate is relative to the left side of the text area, going right, and the y coordinate is relative to the top side of the text area, going down.  }
type
  PTTF_FillOperation = ^TTTF_FillOperation;
  TTTF_FillOperation = record
      cmd : TTTF_DrawCommand;
      rect : TSDL_Rect;
    end;
{*
 * A texture copy draw operation.
 *
 * \since This struct is available since SDL_ttf 3.0.0.
 *
 * \sa TTF_DrawOperation
  }
{*< TTF_DRAW_COMMAND_COPY  }
{*< The offset in the text corresponding to this glyph.
                                      There may be multiple glyphs with the same text offset
                                      and the next text offset might be several Unicode codepoints
                                      later. In this case the glyphs and codepoints are grouped
                                      together and the group bounding box is the union of the dst
                                      rectangles for the corresponding glyphs.  }
{*< The font containing the glyph to be drawn, can be passed to TTF_GetGlyphImageForIndex()  }
{*< The glyph index of the glyph to be drawn, can be passed to TTF_GetGlyphImageForIndex()  }
{*< The area within the glyph to be drawn  }
{*< The drawing coordinates of the glyph, in pixels. The x coordinate is relative to the left side of the text area, going right, and the y coordinate is relative to the top side of the text area, going down.  }

  PTTF_CopyOperation = ^TTTF_CopyOperation;
  TTTF_CopyOperation = record
      cmd : TTTF_DrawCommand;
      text_offset : longint;
      glyph_font : PTTF_Font;
      glyph_index : TUint32;
      src : TSDL_Rect;
      dst : TSDL_Rect;
      reserved : pointer;
    end;
{*
 * A text engine draw operation.
 *
 * \since This struct is available since SDL_ttf 3.0.0.
  }

  PTTF_DrawOperation = ^TTTF_DrawOperation;
  TTTF_DrawOperation = record
      case longint of
        0 : ( cmd : TTTF_DrawCommand );
        1 : ( fill : TTTF_FillOperation );
        2 : ( copy : TTTF_CopyOperation );
      end;
{ Private data in TTF_Text, to assist in text measurement and layout  }
{ Private data in TTF_Text, available to implementations  }
{*< The font used by this text, read-only.  }
{*< The color of the text, read-only.  }
{*< True if the layout needs to be updated  }
{*< Cached layout information, read-only.  }
{*< The x offset of the upper left corner of this text, in pixels, read-only.  }
{*< The y offset of the upper left corner of this text, in pixels, read-only.  }
{*< The width of this text, in pixels, read-only.  }
{*< The height of this text, in pixels, read-only.  }
{*< The number of drawing operations to render this text, read-only.  }
{*< The drawing operations used to render this text, read-only.  }
{*< The number of substrings representing clusters of glyphs in the string, read-only  }
{*< Substrings representing clusters of glyphs in the string, read-only  }
{*< Custom properties associated with this text, read-only. This field is created as-needed using TTF_GetTextProperties() and the properties may be then set and read normally  }
{*< True if the engine text needs to be updated  }
{*< The engine used to render this text, read-only.  }
{*< The implementation-specific representation of this text  }
  PTTF_TextData = ^TTTF_TextData;
  TTTF_TextData = record
      font : PTTF_Font;
      color : TSDL_FColor;
      needs_layout_update : Tbool;
      layout : PTTF_TextLayout;
      x : longint;
      y : longint;
      w : longint;
      h : longint;
      num_ops : longint;
      ops : PTTF_DrawOperation;
      num_clusters : longint;
      clusters : PTTF_SubString;
      props : TSDL_PropertiesID;
      needs_engine_update : Tbool;
      engine : PTTF_TextEngine;
      engine_text : pointer;
    end;

{*
 * A text engine interface.
 *
 * This structure should be initialized using SDL_INIT_INTERFACE()
 *
 * \since This struct is available since SDL_ttf 3.0.0.
 *
 * \sa SDL_INIT_INTERFACE
  }
{*< The version of this interface  }
{*< User data pointer passed to callbacks  }
{ Create a text representation from draw instructions.
     *
     * All fields of `text` except `internal->engine_text` will already be filled out.
     *
     * This function should set the `internal->engine_text` field to a non-NULL value.
     *
     * \param userdata the userdata pointer in this interface.
     * \param text the text object being created.
      }
{*
     * Destroy a text representation.
      }
  PTTF_TextEngine = ^TTTF_TextEngine;
  TTTF_TextEngine = record
      version : TUint32;
      userdata : pointer;
      CreateText : function (userdata:pointer; text:PTTF_Text):Tbool;cdecl;
      DestroyText : procedure (userdata:pointer; text:PTTF_Text);cdecl;
    end;

{ Check the size of TTF_TextEngine
 *
 * If this assert fails, either the compiler is padding to an unexpected size,
 * or the interface has been updated and this should be updated to match and
 * the code using this interface should be updated to handle the old version.
  }
{SDL_COMPILE_TIME_ASSERT(TTF_TextEngine_SIZE, }
{    (sizeof(void *) == 4 && sizeof(TTF_TextEngine) == 16) || }
{    (sizeof(void *) == 8 && sizeof(TTF_TextEngine) == 32)); }
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_TTF_TEXTENGINE_H_  }

implementation


end.
