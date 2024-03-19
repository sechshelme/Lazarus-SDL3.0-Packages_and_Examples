/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

const
  SDL_MS_PER_SECOND = 1000;
  SDL_US_PER_SECOND = 1000000;
  SDL_NS_PER_SECOND = 1000000000;
  SDL_NS_PER_MS = 1000000;
  SDL_NS_PER_US = 1000;

function SDL_MS_TO_NS(MS: longint): int64;
function SDL_NS_TO_MS(NS: int64): longint;
function SDL_US_TO_NS(US: longint): int64;
function SDL_NS_TO_US(NS: int64): longint;

function SDL_GetTicks: uint64; cdecl; external;
function SDL_GetTicksNS: uint64; cdecl; external;
function SDL_GetPerformanceCounter: uint64; cdecl; external;
function SDL_GetPerformanceFrequency: uint64; cdecl; external;
procedure SDL_Delay(ms: uint32); cdecl; external;
procedure SDL_DelayNS(ns: uint64); cdecl; external;

type
  TSDL_TimerCallback = function(interval: uint32; param: pointer): uint32; cdecl;

  PSDL_TimerID = ^TSDL_TimerID;
  TSDL_TimerID = uint32;

function SDL_AddTimer(interval: uint32; callback: TSDL_TimerCallback; param: pointer): TSDL_TimerID; cdecl; external;
function SDL_RemoveTimer(id: TSDL_TimerID): TSDL_bool; cdecl; external;

{$ENDIF read_interface}


{$IFDEF read_implementation}

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
