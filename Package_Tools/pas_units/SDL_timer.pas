unit SDL_timer;

interface

uses
  SDL_stdinc;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

const
  SDL_MS_PER_SECOND = 1000;  
  SDL_US_PER_SECOND = 1000000;  
  SDL_NS_PER_SECOND = 1000000000;  
  SDL_NS_PER_MS = 1000000;  
  SDL_NS_PER_US = 1000;  

function SDL_SECONDS_TO_NS(S : TUint64) : TUint64;
function SDL_NS_TO_SECONDS(NS : TUint64) : TUint64;
function SDL_MS_TO_NS(MS : TUint64) : TUint64;
function SDL_NS_TO_MS(NS : TUint64) : TUint64;
function SDL_US_TO_NS(US : TUint64) : TUint64;
function SDL_NS_TO_US(NS : TUint64) : TUint64;

function SDL_GetTicks:TUint64;cdecl;external libSDL3;
function SDL_GetTicksNS:TUint64;cdecl;external libSDL3;
function SDL_GetPerformanceCounter:TUint64;cdecl;external libSDL3;
function SDL_GetPerformanceFrequency:TUint64;cdecl;external libSDL3;
procedure SDL_Delay(ms:TUint32);cdecl;external libSDL3;
procedure SDL_DelayNS(ns:TUint64);cdecl;external libSDL3;
type
  TSDL_TimerID = TUint32;
  PSDL_TimerID = ^TSDL_TimerID;

  TSDL_TimerCallback = function (userdata:pointer; timerID:TSDL_TimerID; interval:TUint32):TUint32;cdecl;

function SDL_AddTimer(interval:TUint32; callback:TSDL_TimerCallback; userdata:pointer):TSDL_TimerID;cdecl;external libSDL3;
type
  TSDL_NSTimerCallback = function (userdata:pointer; timerID:TSDL_TimerID; interval:TUint64):TUint64;cdecl;

function SDL_AddTimerNS(interval:TUint64; callback:TSDL_NSTimerCallback; userdata:pointer):TSDL_TimerID;cdecl;external libSDL3;
function SDL_RemoveTimer(id:TSDL_TimerID):Tbool;cdecl;external libSDL3;

implementation

function SDL_SECONDS_TO_NS(S: TUint64): TUint64;
begin
  SDL_SECONDS_TO_NS:=(TUint64(S))*SDL_NS_PER_SECOND;
end;

function SDL_NS_TO_SECONDS(NS: TUint64): TUint64;
begin
  SDL_NS_TO_SECONDS:=NS div SDL_NS_PER_SECOND;
end;

function SDL_MS_TO_NS(MS: TUint64): TUint64;
begin
  SDL_MS_TO_NS:=(TUint64(MS))*SDL_NS_PER_MS;
end;

function SDL_NS_TO_MS(NS: TUint64): TUint64;
begin
  SDL_NS_TO_MS:=NS div SDL_NS_PER_MS;
end;

function SDL_US_TO_NS(US: TUint64): TUint64;
begin
  SDL_US_TO_NS:=(TUint64(US))*SDL_NS_PER_US;
end;

function SDL_NS_TO_US(NS: TUint64): TUint64;
begin
  SDL_NS_TO_US:=NS div SDL_NS_PER_US;
end;


end.
