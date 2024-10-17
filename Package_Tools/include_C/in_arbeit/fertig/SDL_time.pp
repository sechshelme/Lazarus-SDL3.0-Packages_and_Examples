
unit SDL_time;
interface

{
  Automatically converted by H2Pas 0.99.16 from SDL_time.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_time.h
}

Type
PSDL_Time = ^TSDL_Time;
PUint32 = ^TUint32;

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
{$ifndef SDL_time_h_}
{$define SDL_time_h_}
{*
 * # CategoryTime
 *
 * SDL realtime clock and date/time routines.
  }
{$include <SDL3/SDL_error.h>}
{$include <SDL3/SDL_stdinc.h>}
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{*
 * A structure holding a calendar date and time broken down into its
 * components.
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< Year  }
{*< Month [01-12]  }
{*< Day of the month [01-31]  }
{*< Hour [0-23]  }
{*< Minute [0-59]  }
{*< Seconds [0-60]  }
{*< Nanoseconds [0-999999999]  }
{*< Day of the week [0-6] (0 being Sunday)  }
{*< Seconds east of UTC  }
type
  PSDL_DateTime = ^TSDL_DateTime;
  TSDL_DateTime = record
      year : longint;
      month : longint;
      day : longint;
      hour : longint;
      minute : longint;
      second : longint;
      nanosecond : longint;
      day_of_week : longint;
      utc_offset : longint;
    end;
{*
 * The preferred date format of the current system locale.
 *
 * \since This enum is available since SDL 3.0.0.
 *
 * \sa SDL_GetDateTimeLocalePreferences
  }
{*< Year/Month/Day  }
{*< Day/Month/Year  }
{*< Month/Day/Year  }

  PSDL_DateFormat = ^TSDL_DateFormat;
  TSDL_DateFormat =  Longint;
  Const
    SDL_DATE_FORMAT_YYYYMMDD = &;
    SDL_DATE_FORMAT_DDMMYYYY = 1;
    SDL_DATE_FORMAT_MMDDYYYY = 2;
;
{*
 * The preferred time format of the current system locale.
 *
 * \since This enum is available since SDL 3.0.0.
 *
 * \sa SDL_GetDateTimeLocalePreferences
  }
{*< 24 hour time  }
{*< 12 hour time  }
type
  PSDL_TimeFormat = ^TSDL_TimeFormat;
  TSDL_TimeFormat =  Longint;
  Const
    SDL_TIME_FORMAT_24HR = &;
    SDL_TIME_FORMAT_12HR = 1;
;
{*
 * Gets the current preferred date and time format for the system locale.
 *
 * This might be a "slow" call that has to query the operating system. It's
 * best to ask for this once and save the results. However, the preferred
 * formats can change, usually because the user has changed a system
 * preference outside of your program.
 *
 * \param dateFormat a pointer to the SDL_DateFormat to hold the returned date
 *                   format, may be NULL.
 * \param timeFormat a pointer to the SDL_TimeFormat to hold the returned time
 *                   format, may be NULL.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
  }

function SDL_GetDateTimeLocalePreferences(dateFormat:PSDL_DateFormat; timeFormat:PSDL_TimeFormat):Tbool;cdecl;external;
{*
 * Gets the current value of the system realtime clock in nanoseconds since
 * Jan 1, 1970 in Universal Coordinated Time (UTC).
 *
 * \param ticks the SDL_Time to hold the returned tick count.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetCurrentTime(ticks:PSDL_Time):Tbool;cdecl;external;
{*
 * Converts an SDL_Time in nanoseconds since the epoch to a calendar time in
 * the SDL_DateTime format.
 *
 * \param ticks the SDL_Time to be converted.
 * \param dt the resulting SDL_DateTime.
 * \param localTime the resulting SDL_DateTime will be expressed in local time
 *                  if true, otherwise it will be in Universal Coordinated
 *                  Time (UTC).
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_TimeToDateTime(ticks:TSDL_Time; dt:PSDL_DateTime; localTime:Tbool):Tbool;cdecl;external;
{*
 * Converts a calendar time to an SDL_Time in nanoseconds since the epoch.
 *
 * This function ignores the day_of_week member of the SDL_DateTime struct, so
 * it may remain unset.
 *
 * \param dt the source SDL_DateTime.
 * \param ticks the resulting SDL_Time.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
  }
(* Const before declarator ignored *)
function SDL_DateTimeToTime(dt:PSDL_DateTime; ticks:PSDL_Time):Tbool;cdecl;external;
{*
 * Converts an SDL time into a Windows FILETIME (100-nanosecond intervals
 * since January 1, 1601).
 *
 * This function fills in the two 32-bit values of the FILETIME structure.
 *
 * \param ticks the time to convert.
 * \param dwLowDateTime a pointer filled in with the low portion of the
 *                      Windows FILETIME value.
 * \param dwHighDateTime a pointer filled in with the high portion of the
 *                       Windows FILETIME value.
 *
 * \since This function is available since SDL 3.0.0.
  }
procedure SDL_TimeToWindows(ticks:TSDL_Time; dwLowDateTime:PUint32; dwHighDateTime:PUint32);cdecl;external;
{*
 * Converts a Windows FILETIME (100-nanosecond intervals since January 1,
 * 1601) to an SDL time.
 *
 * This function takes the two 32-bit values of the FILETIME structure as
 * parameters.
 *
 * \param dwLowDateTime the low portion of the Windows FILETIME value.
 * \param dwHighDateTime the high portion of the Windows FILETIME value.
 * \returns the converted SDL time.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_TimeFromWindows(dwLowDateTime:TUint32; dwHighDateTime:TUint32):TSDL_Time;cdecl;external;
{*
 * Get the number of days in a month for a given year.
 *
 * \param year the year.
 * \param month the month [1-12].
 * \returns the number of days in the requested month or -1 on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetDaysInMonth(year:longint; month:longint):longint;cdecl;external;
{*
 * Get the day of year for a calendar date.
 *
 * \param year the year component of the date.
 * \param month the month component of the date.
 * \param day the day component of the date.
 * \returns the day of year [0-365] if the date is valid or -1 on failure;
 *          call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetDayOfYear(year:longint; month:longint; day:longint):longint;cdecl;external;
{*
 * Get the day of week for a calendar date.
 *
 * \param year the year component of the date.
 * \param month the month component of the date.
 * \param day the day component of the date.
 * \returns a value between 0 and 6 (0 being Sunday) if the date is valid or
 *          -1 on failure; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetDayOfWeek(year:longint; month:longint; day:longint):longint;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_time_h_  }

implementation


end.
