/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 03-10-2024   17:45                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_DateTime = ^TSDL_DateTime;

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

function SDL_GetDateTimeLocalePreferences(dateFormat: PSDL_DateFormat; timeFormat: PSDL_TimeFormat): longint; cdecl; external sdl3_lib;
function SDL_GetCurrentTime(ticks: PSDL_Time): longint; cdecl; external sdl3_lib;
function SDL_TimeToDateTime(ticks: TSDL_Time; dt: PSDL_DateTime; localTime: TSDL_bool): longint; cdecl; external sdl3_lib;
function SDL_DateTimeToTime(dt: PSDL_DateTime; ticks: PSDL_Time): longint; cdecl; external sdl3_lib;
procedure SDL_TimeToWindows(ticks: TSDL_Time; dwLowDateTime: PUint32; dwHighDateTime: PUint32); cdecl; external sdl3_lib;
function SDL_TimeFromWindows(dwLowDateTime: TUint32; dwHighDateTime: TUint32): TSDL_Time; cdecl; external sdl3_lib;
function SDL_GetDaysInMonth(year: longint; month: longint): longint; cdecl; external sdl3_lib;
function SDL_GetDayOfYear(year: longint; month: longint; day: longint): longint; cdecl; external sdl3_lib;
function SDL_GetDayOfWeek(year: longint; month: longint; day: longint): longint; cdecl; external sdl3_lib;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}
