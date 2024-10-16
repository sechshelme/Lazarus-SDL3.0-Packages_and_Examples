
unit SDL_power;
interface

{
  Automatically converted by H2Pas 0.99.16 from SDL_power.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_power.h
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
{$ifndef SDL_power_h_}
{$define SDL_power_h_}
{*
 * # CategoryPower
 *
 * SDL power management routines.
  }
{$include <SDL3/SDL_stdinc.h>}
{$include <SDL3/SDL_error.h>}
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{*
 * The basic state for the system's power supply.
 *
 * These are results returned by SDL_GetPowerInfo().
 *
 * \since This enum is available since SDL 3.0.0
  }
{*< error determining power status  }
{*< cannot determine power status  }
{*< Not plugged in, running on the battery  }
{*< Plugged in, no battery available  }
{*< Plugged in, charging battery  }
{*< Plugged in, battery charged  }
type
  PSDL_PowerState = ^TSDL_PowerState;
  TSDL_PowerState =  Longint;
  Const
    SDL_POWERSTATE_ERROR = -(1);
    SDL_POWERSTATE_UNKNOWN = (-(1))+1;
    SDL_POWERSTATE_ON_BATTERY = (-(1))+2;
    SDL_POWERSTATE_NO_BATTERY = (-(1))+3;
    SDL_POWERSTATE_CHARGING = (-(1))+4;
    SDL_POWERSTATE_CHARGED = (-(1))+5;
;
{*
 * Get the current power supply details.
 *
 * You should never take a battery status as absolute truth. Batteries
 * (especially failing batteries) are delicate hardware, and the values
 * reported here are best estimates based on what that hardware reports. It's
 * not uncommon for older batteries to lose stored power much faster than it
 * reports, or completely drain when reporting it has 20 percent left, etc.
 *
 * Battery status can change at any time; if you are concerned with power
 * state, you should call this function frequently, and perhaps ignore changes
 * until they seem to be stable for a few seconds.
 *
 * It's possible a platform can only report battery percentage or time left
 * but not both.
 *
 * \param seconds a pointer filled in with the seconds of battery life left,
 *                or NULL to ignore. This will be filled in with -1 if we
 *                can't determine a value or there is no battery.
 * \param percent a pointer filled in with the percentage of battery life
 *                left, between 0 and 100, or NULL to ignore. This will be
 *                filled in with -1 we can't determine a value or there is no
 *                battery.
 * \returns the current battery state or `SDL_POWERSTATE_ERROR` on failure;
 *          call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }

function SDL_GetPowerInfo(seconds:Plongint; percent:Plongint):TSDL_PowerState;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_power_h_  }

implementation


end.
