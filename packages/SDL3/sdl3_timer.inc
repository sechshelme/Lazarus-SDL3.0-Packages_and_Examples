
{$IFDEF read_interface}

const
  SDL_MS_PER_SECOND = 1000;
  SDL_US_PER_SECOND = 1000000;
  SDL_NS_PER_SECOND = 1000000000;
  SDL_NS_PER_MS = 1000000;
  SDL_NS_PER_US = 1000;

function SDL_SECONDS_TO_NS(S : longint) : int64;
function SDL_NS_TO_SECONDS(NS : int64) : longint;
function SDL_MS_TO_NS(MS: longint): int64;
function SDL_NS_TO_MS(NS: int64): longint;
function SDL_US_TO_NS(US: longint): int64;
function SDL_NS_TO_US(NS: int64): longint;

function SDL_GetTicks: uint64; cdecl; external sdl3_lib;
function SDL_GetTicksNS: uint64; cdecl; external sdl3_lib;
function SDL_GetPerformanceCounter: uint64; cdecl; external sdl3_lib;
function SDL_GetPerformanceFrequency: uint64; cdecl; external sdl3_lib;
procedure SDL_Delay(ms: uint32); cdecl; external sdl3_lib;
procedure SDL_DelayNS(ns: uint64); cdecl; external sdl3_lib;

type
  PSDL_TimerID = ^TSDL_TimerID;
  TSDL_TimerID = uint32;
//  TSDL_TimerCallback = function(interval: uint32; param: pointer): uint32; cdecl;
  TSDL_TimerCallback = function (userdata:pointer; timerID:TSDL_TimerID; interval:TUint32):TUint32;cdecl;

//function SDL_AddTimer(interval: uint32; callback: TSDL_TimerCallback; param: pointer): TSDL_TimerID; cdecl; external sdl3_lib;
function SDL_AddTimer(interval:TUint32; callback:TSDL_TimerCallback; userdata:pointer):TSDL_TimerID;cdecl;external sdl3_lib;

type
  TSDL_NSTimerCallback = function (userdata:pointer; timerID:TSDL_TimerID; interval:TUint64):TUint64;cdecl;

function SDL_AddTimerNS(interval:TUint64; callback:TSDL_NSTimerCallback; userdata:pointer):TSDL_TimerID;cdecl;external sdl3_lib;

function SDL_RemoveTimer(id: TSDL_TimerID): TSDL_bool; cdecl; external sdl3_lib;

{$ENDIF read_interface}


{$IFDEF read_implementation}


function SDL_SECONDS_TO_NS(S : longint) : int64;
begin
  SDL_SECONDS_TO_NS:=(TUint64(S)) * SDL_NS_PER_SECOND;
end;

function SDL_NS_TO_SECONDS(NS : int64) : longint;
begin
  SDL_NS_TO_SECONDS:=NS div SDL_NS_PER_SECOND;
end;

function SDL_MS_TO_NS(MS: longint): int64;
begin
  SDL_MS_TO_NS := (Uint64(MS)) * SDL_NS_PER_MS;
end;

function SDL_NS_TO_MS(NS: int64): longint;
begin
  SDL_NS_TO_MS := NS div SDL_NS_PER_MS;
end;

function SDL_US_TO_NS(US: longint): int64;
begin
  SDL_US_TO_NS := (Uint64(US)) * SDL_NS_PER_US;
end;

function SDL_NS_TO_US(NS: int64): longint;
begin
  SDL_NS_TO_US := NS div SDL_NS_PER_US;
end;

{$ENDIF read_implementation}
