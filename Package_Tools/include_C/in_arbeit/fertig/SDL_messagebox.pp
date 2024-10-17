
unit SDL_messagebox;
interface

{
  Automatically converted by H2Pas 0.99.16 from SDL_messagebox.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_messagebox.h
}

Type
PSDL_Window = ^TSDL_Window;

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
{*
 * # CategoryMessagebox
 *
 * Message box support routines.
  }
{$ifndef SDL_messagebox_h_}
{$define SDL_messagebox_h_}
{$include <SDL3/SDL_stdinc.h>}
{$include <SDL3/SDL_error.h>}
{$include <SDL3/SDL_video.h>      /* For SDL_Window */}
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{*
 * SDL_MessageBox flags.
 *
 * If supported will display warning icon, etc.
 *
 * \since This datatype is available since SDL 3.0.0.
  }
type
  PSDL_MessageBoxFlags = ^TSDL_MessageBoxFlags;
  TSDL_MessageBoxFlags = TUint32;
{*< error dialog  }

const
  SDL_MESSAGEBOX_ERROR = $00000010;  
{*< warning dialog  }
  SDL_MESSAGEBOX_WARNING = $00000020;  
{*< informational dialog  }
  SDL_MESSAGEBOX_INFORMATION = $00000040;  
{*< buttons placed left to right  }
  SDL_MESSAGEBOX_BUTTONS_LEFT_TO_RIGHT = $00000080;  
{*< buttons placed right to left  }
  SDL_MESSAGEBOX_BUTTONS_RIGHT_TO_LEFT = $00000100;  
{*
 * SDL_MessageBoxButtonData flags.
 *
 * \since This datatype is available since SDL 3.0.0.
  }
type
  PSDL_MessageBoxButtonFlags = ^TSDL_MessageBoxButtonFlags;
  TSDL_MessageBoxButtonFlags = TUint32;
{*< Marks the default button when return is hit  }

const
  SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT = $00000001;  
{*< Marks the default button when escape is hit  }
  SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT = $00000002;  
{*
 * Individual button data.
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< User defined button id (value returned via SDL_ShowMessageBox)  }
(* Const before declarator ignored *)
{*< The UTF-8 button text  }
type
  PSDL_MessageBoxButtonData = ^TSDL_MessageBoxButtonData;
  TSDL_MessageBoxButtonData = record
      flags : TSDL_MessageBoxButtonFlags;
      buttonID : longint;
      text : Pansichar;
    end;
{*
 * RGB value used in a message box color scheme
 *
 * \since This struct is available since SDL 3.0.0.
  }

  PSDL_MessageBoxColor = ^TSDL_MessageBoxColor;
  TSDL_MessageBoxColor = record
      r : TUint8;
      g : TUint8;
      b : TUint8;
    end;
{*
 * An enumeration of indices inside the colors array of
 * SDL_MessageBoxColorScheme.
  }
{*< Size of the colors array of SDL_MessageBoxColorScheme.  }

  PSDL_MessageBoxColorType = ^TSDL_MessageBoxColorType;
  TSDL_MessageBoxColorType =  Longint;
  Const
    SDL_MESSAGEBOX_COLOR_BACKGROUND = 0;
    SDL_MESSAGEBOX_COLOR_TEXT = 1;
    SDL_MESSAGEBOX_COLOR_BUTTON_BORDER = 2;
    SDL_MESSAGEBOX_COLOR_BUTTON_BACKGROUND = 3;
    SDL_MESSAGEBOX_COLOR_BUTTON_SELECTED = 4;
    SDL_MESSAGEBOX_COLOR_COUNT = 5;
;
{*
 * A set of colors to use for message box dialogs
 *
 * \since This struct is available since SDL 3.0.0.
  }
type
  PSDL_MessageBoxColorScheme = ^TSDL_MessageBoxColorScheme;
  TSDL_MessageBoxColorScheme = record
      colors : array[0..(SDL_MESSAGEBOX_COLOR_COUNT)-1] of TSDL_MessageBoxColor;
    end;
{*
 * MessageBox structure containing title, text, window, etc.
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< Parent window, can be NULL  }
(* Const before declarator ignored *)
{*< UTF-8 title  }
(* Const before declarator ignored *)
{*< UTF-8 message text  }
(* Const before declarator ignored *)
(* Const before declarator ignored *)
{*< SDL_MessageBoxColorScheme, can be NULL to use system settings  }

  PSDL_MessageBoxData = ^TSDL_MessageBoxData;
  TSDL_MessageBoxData = record
      flags : TSDL_MessageBoxFlags;
      window : PSDL_Window;
      title : Pansichar;
      message : Pansichar;
      numbuttons : longint;
      buttons : PSDL_MessageBoxButtonData;
      colorScheme : PSDL_MessageBoxColorScheme;
    end;
{*
 * Create a modal message box.
 *
 * If your needs aren't complex, it might be easier to use
 * SDL_ShowSimpleMessageBox.
 *
 * This function should be called on the thread that created the parent
 * window, or on the main thread if the messagebox has no parent. It will
 * block execution of that thread until the user clicks a button or closes the
 * messagebox.
 *
 * This function may be called at any time, even before SDL_Init(). This makes
 * it useful for reporting errors like a failure to create a renderer or
 * OpenGL context.
 *
 * On X11, SDL rolls its own dialog box with X11 primitives instead of a
 * formal toolkit like GTK+ or Qt.
 *
 * Note that if SDL_Init() would fail because there isn't any available video
 * target, this function is likely to fail for the same reasons. If this is a
 * concern, check the return value from this function and fall back to writing
 * to stderr if you can.
 *
 * \param messageboxdata the SDL_MessageBoxData structure with title, text and
 *                       other options.
 * \param buttonid the pointer to which user id of hit button should be
 *                 copied.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_ShowSimpleMessageBox
  }
(* Const before declarator ignored *)

function SDL_ShowMessageBox(messageboxdata:PSDL_MessageBoxData; buttonid:Plongint):Tbool;cdecl;external;
{*
 * Display a simple modal message box.
 *
 * If your needs aren't complex, this function is preferred over
 * SDL_ShowMessageBox.
 *
 * `flags` may be any of the following:
 *
 * - `SDL_MESSAGEBOX_ERROR`: error dialog
 * - `SDL_MESSAGEBOX_WARNING`: warning dialog
 * - `SDL_MESSAGEBOX_INFORMATION`: informational dialog
 *
 * This function should be called on the thread that created the parent
 * window, or on the main thread if the messagebox has no parent. It will
 * block execution of that thread until the user clicks a button or closes the
 * messagebox.
 *
 * This function may be called at any time, even before SDL_Init(). This makes
 * it useful for reporting errors like a failure to create a renderer or
 * OpenGL context.
 *
 * On X11, SDL rolls its own dialog box with X11 primitives instead of a
 * formal toolkit like GTK+ or Qt.
 *
 * Note that if SDL_Init() would fail because there isn't any available video
 * target, this function is likely to fail for the same reasons. If this is a
 * concern, check the return value from this function and fall back to writing
 * to stderr if you can.
 *
 * \param flags an SDL_MessageBoxFlags value.
 * \param title uTF-8 title text.
 * \param message uTF-8 message text.
 * \param window the parent window, or NULL for no parent.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_ShowMessageBox
  }
(* Const before declarator ignored *)
(* Const before declarator ignored *)
function SDL_ShowSimpleMessageBox(flags:TSDL_MessageBoxFlags; title:Pansichar; message:Pansichar; window:PSDL_Window):Tbool;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_messagebox_h_  }

implementation


end.
