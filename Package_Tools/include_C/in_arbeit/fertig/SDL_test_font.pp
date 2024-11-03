
unit SDL_test_font;
interface

{
  Automatically converted by H2Pas 0.99.16 from SDL_test_font.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_test_font.h
}

Type
PSDL_Renderer = ^TSDL_Renderer;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{
  Simple DirectMedia Layer
  Copyright (C) 1997-2024 Sam Lantinga <slouken@libsdl.org>

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
{
 *  \file SDL_test_font.h
 *
 *  Font related functions of SDL test framework.
 *
 *  This code is a part of the SDL test library, not the main SDL library.
  }
{$ifndef SDL_test_font_h_}
{$define SDL_test_font_h_}
{$include <SDL3/SDL_stdinc.h>}
{$include <SDL3/SDL_rect.h>}
{$include <SDL3/SDL_render.h>}
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{ Function prototypes  }
  var
    FONT_CHARACTER_SIZE : longint;cvar;external;

const
  FONT_LINE_HEIGHT = FONT_CHARACTER_SIZE+2;  
{
 *  Draw a string in the currently set font.
 *
 *  \param renderer The renderer to draw on.
 *  \param x The X coordinate of the upper left corner of the character.
 *  \param y The Y coordinate of the upper left corner of the character.
 *  \param c The character to draw.
 *
 *  \returns true on success, false on failure.
  }

function SDLTest_DrawCharacter(renderer:PSDL_Renderer; x:single; y:single; c:TUint32):Tbool;cdecl;external;
{
 *  Draw a UTF-8 string in the currently set font.
 *
 *  The font currently only supports characters in the Basic Latin and Latin-1 Supplement sets.
 *
 *  \param renderer The renderer to draw on.
 *  \param x The X coordinate of the upper left corner of the string.
 *  \param y The Y coordinate of the upper left corner of the string.
 *  \param s The string to draw.
 *
 *  \returns true on success, false on failure.
  }
(* Const before declarator ignored *)
function SDLTest_DrawString(renderer:PSDL_Renderer; x:single; y:single; s:Pansichar):Tbool;cdecl;external;
{
 *  Data used for multi-line text output
  }
type
  PSDLTest_TextWindow = ^TSDLTest_TextWindow;
  TSDLTest_TextWindow = record
      rect : TSDL_FRect;
      current : longint;
      numlines : longint;
      lines : ^Pansichar;
    end;
{
 *  Create a multi-line text output window
 *
 *  \param x The X coordinate of the upper left corner of the window.
 *  \param y The Y coordinate of the upper left corner of the window.
 *  \param w The width of the window (currently ignored)
 *  \param h The height of the window (currently ignored)
 *
 *  \returns the new window, or NULL on failure.
 *
 *  \since This function is available since SDL 3.1.3.
  }

function SDLTest_TextWindowCreate(x:single; y:single; w:single; h:single):PSDLTest_TextWindow;cdecl;external;
{
 *  Display a multi-line text output window
 *
 *  This function should be called every frame to display the text
 *
 *  \param textwin The text output window
 *  \param renderer The renderer to use for display
 *
 *  \since This function is available since SDL 3.1.3.
  }
procedure SDLTest_TextWindowDisplay(textwin:PSDLTest_TextWindow; renderer:PSDL_Renderer);cdecl;external;
{
 *  Add text to a multi-line text output window
 *
 *  Adds UTF-8 text to the end of the current text. The newline character starts a
 *  new line of text. The backspace character deletes the last character or, if the
 *  line is empty, deletes the line and goes to the end of the previous line.
 *
 *  \param textwin The text output window
 *  \param fmt A printf() style format string
 *  \param ...  additional parameters matching % tokens in the `fmt` string, if any
 *
 *  \since This function is available since SDL 3.1.3.
  }
(* Const before declarator ignored *)
procedure SDLTest_TextWindowAddText(textwin:PSDLTest_TextWindow; fmt:Pansichar; args:array of const);cdecl;external;
procedure SDLTest_TextWindowAddText(textwin:PSDLTest_TextWindow; fmt:Pansichar);cdecl;external;
{
 *  Add text to a multi-line text output window
 *
 *  Adds UTF-8 text to the end of the current text. The newline character starts a
 *  new line of text. The backspace character deletes the last character or, if the
 *  line is empty, deletes the line and goes to the end of the previous line.
 *
 *  \param textwin The text output window
 *  \param text The text to add to the window
 *  \param len The length, in bytes, of the text to add to the window
 *
 *  \since This function is available since SDL 3.1.3.
  }
(* Const before declarator ignored *)
procedure SDLTest_TextWindowAddTextWithLength(textwin:PSDLTest_TextWindow; text:Pansichar; len:Tsize_t);cdecl;external;
{
 *  Clear the text in a multi-line text output window
 *
 *  \param textwin The text output window
 *
 *  \since This function is available since SDL 3.1.3.
  }
procedure SDLTest_TextWindowClear(textwin:PSDLTest_TextWindow);cdecl;external;
{
 *  Free the storage associated with a multi-line text output window
 *
 *  \param textwin The text output window
 *
 *  \since This function is available since SDL 3.1.3.
  }
procedure SDLTest_TextWindowDestroy(textwin:PSDLTest_TextWindow);cdecl;external;
{
 *  Cleanup textures used by font drawing functions.
  }
procedure SDLTest_CleanupTextDrawing;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_test_font_h_  }

implementation


end.
