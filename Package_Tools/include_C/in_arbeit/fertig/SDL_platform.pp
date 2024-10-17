
unit SDL_platform;
interface

{
  Automatically converted by H2Pas 0.99.16 from SDL_platform.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_platform.h
}


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
 * # CategoryPlatform
 *
 * SDL provides a means to identify the app's platform, both at compile time
 * and runtime.
  }
{$ifndef SDL_platform_h_}
{$define SDL_platform_h_}
{$include <SDL3/SDL_platform_defines.h>}
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{*
 * Get the name of the platform.
 *
 * Here are the names returned for some (but not all) supported platforms:
 *
 * - "Windows"
 * - "macOS"
 * - "Linux"
 * - "iOS"
 * - "Android"
 *
 * \returns the name of the platform. If the correct platform name is not
 *          available, returns a string beginning with the text "Unknown".
 *
 * \since This function is available since SDL 3.0.0.
  }
(* Const before declarator ignored *)

function SDL_GetPlatform:Pansichar;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_platform_h_  }

implementation


end.
