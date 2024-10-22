unit SDL_time;

interface

uses
  SDL_stdinc;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  TSDL_DateTime = record
    year: longint;
    month: longint;
    day: longint;
    hour: longint;
    minute: longint;
    second: longint;
    nanosecond: longint;
    day_of_week: longint;
    utc_offset: longint;
  end;
  PSDL_DateTime = ^TSDL_DateTime;

  PSDL_DateFormat = ^TSDL_DateFormat;
  TSDL_DateFormat = longint;

const
  SDL_DATE_FORMAT_YYYYMMDD = 0;
  SDL_DATE_FORMAT_DDMMYYYY = 1;
  SDL_DATE_FORMAT_MMDDYYYY = 2;

type
  PSDL_TimeFormat = ^TSDL_TimeFormat;
  TSDL_TimeFormat = longint;

const
  SDL_TIME_FORMAT_24HR = 0;
  SDL_TIME_FORMAT_12HR = 1;

function SDL_GetDateTimeLocalePreferences(dateFormat: PSDL_DateFormat; timeFormat: PSDL_TimeFormat): Tbool; cdecl; external libSDL3;
function SDL_GetCurrentTime(ticks: PSDL_Time): Tbool; cdecl; external libSDL3;
function SDL_TimeToDateTime(ticks: TSDL_Time; dt: PSDL_DateTime; localTime: Tbool): Tbool; cdecl; external libSDL3;
function SDL_DateTimeToTime(dt: PSDL_DateTime; ticks: PSDL_Time): Tbool; cdecl; external libSDL3;
procedure SDL_TimeToWindows(ticks: TSDL_Time; dwLowDateTime: PUint32; dwHighDateTime: PUint32); cdecl; external libSDL3;
function SDL_TimeFromWindows(dwLowDateTime: TUint32; dwHighDateTime: TUint32): TSDL_Time; cdecl; external libSDL3;
function SDL_GetDaysInMonth(year: longint; month: longint): longint; cdecl; external libSDL3;
function SDL_GetDayOfYear(year: longint; month: longint; day: longint): longint; cdecl; external libSDL3;
function SDL_GetDayOfWeek(year: longint; month: longint; day: longint): longint; cdecl; external libSDL3;

implementation


end.
