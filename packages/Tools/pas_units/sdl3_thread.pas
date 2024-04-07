unit SDL3_thread;

interface

uses
  sdl3;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

//type
  //TUint8 = uint8;
  //TUint16 = uint16;
  //TUint32 = uint32;
  //TUint64 = uint64;
  //
  //Tint8 = uint8;
  //Tint16 = uint16;
  //Tint32 = uint32;
  //Tint64 = uint64;
  //
  //Tsize_t = SizeUInt;
  //Tuintptr_t = PtrUInt;

type
  PSDL_Thread = ^TSDL_Thread;
  TSDL_Thread = record
    {undefined structure}
  end;

  PSDL_ThreadID = ^TSDL_ThreadID;
  TSDL_ThreadID = TUint64;

  PSDL_TLSID = ^TSDL_TLSID;
  TSDL_TLSID = TUint32;

  PSDL_ThreadPriority = ^TSDL_ThreadPriority;
  TSDL_ThreadPriority = longint;

const
  SDL_THREAD_PRIORITY_LOW = 0;
  SDL_THREAD_PRIORITY_NORMAL = 1;
  SDL_THREAD_PRIORITY_HIGH = 2;
  SDL_THREAD_PRIORITY_TIME_CRITICAL = 3;

type
  TSDL_ThreadFunction = function(Data: pointer): longint; cdecl;

{$IFDEF Windows}
  TBeginThreadFunc = function(para1: pointer): dword;

  TpfnSDL_CurrentBeginThread = function(para1: pointer; para2: dword; func: TBeginThreadFunc; para4: pointer; para5: dword; para6: Pdword): Tuintptr_t; cdecl;
  TpfnSDL_CurrentEndThread = procedure(code: dword); cdecl;

function SDL_CreateThread(fn: TSDL_ThreadFunction; Name: PChar; Data: pointer; pfnBeginThread: TpfnSDL_CurrentBeginThread; pfnEndThread: TpfnSDL_CurrentEndThread): PSDL_Thread; cdecl; external sdl3_lib;
function SDL_CreateThreadWithStackSize(fn: TSDL_ThreadFunction; Name: PChar; stacksize: Tsize_t; Data: pointer; pfnBeginThread: TpfnSDL_CurrentBeginThread; pfnEndThread: TpfnSDL_CurrentEndThread): PSDL_Thread; cdecl; external sdl3_lib;

function SDL_CreateThread(fn: TSDL_ThreadFunction; Name: PChar; Data: pointer): PSDL_Thread;
function SDL_CreateThreadWithStackSize(fn: TSDL_ThreadFunction; Name: PChar; stacksize: Tsize_t; Data: pointer): PSDL_Thread;
{$endif}

{$IFDEF Linux}
function SDL_CreateThread(fn: TSDL_ThreadFunction; Name: PChar; Data: pointer): PSDL_Thread; cdecl; external;
function SDL_CreateThreadWithStackSize(fn: TSDL_ThreadFunction; Name: PChar; stacksize: Tsize_t; Data: pointer): PSDL_Thread; cdecl; external;
{$endif}
function SDL_GetThreadName(thread: PSDL_Thread): PChar; cdecl; external sdl3_lib;
function SDL_GetCurrentThreadID: TSDL_ThreadID; cdecl; external sdl3_lib;
function SDL_GetThreadID(thread: PSDL_Thread): TSDL_ThreadID; cdecl; external sdl3_lib;
function SDL_SetThreadPriority(priority: TSDL_ThreadPriority): longint; cdecl; external sdl3_lib;
procedure SDL_WaitThread(thread: PSDL_Thread; status: Plongint); cdecl; external sdl3_lib;
procedure SDL_DetachThread(thread: PSDL_Thread); cdecl; external sdl3_lib;
function SDL_CreateTLS: TSDL_TLSID; cdecl; external sdl3_lib;
function SDL_GetTLS(id: TSDL_TLSID): pointer; cdecl; external sdl3_lib;

type
  TSDL_SetTLSFunction = procedure(para1: pointer); cdecl;

function SDL_SetTLS(id: TSDL_TLSID; Value: pointer; destructor_: TSDL_SetTLSFunction): longint; cdecl; external sdl3_lib;
procedure SDL_CleanupTLS; cdecl; external sdl3_lib;

implementation

{$IFDEF Windows}
function SDL_CreateThread(fn: TSDL_ThreadFunction; Name: PChar; Data: pointer): PSDL_Thread;
begin
  SDL_CreateThread := SDL_CreateThread(fn, Name, Data, nil, nil);
end;

function SDL_CreateThreadWithStackSize(fn: TSDL_ThreadFunction; Name: PChar; stacksize: Tsize_t; Data: pointer): PSDL_Thread;
begin
  SDL_CreateThreadWithStackSize := SDL_CreateThreadWithStackSize(fn, Name, stacksize, Data, nil, nil);
end;
{$endif}

end.
