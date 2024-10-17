/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   17:58                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_ThreadID = ^TSDL_ThreadID;
  TSDL_ThreadID = TUint64;

  PSDL_TLSID = ^TSDL_TLSID;
  TSDL_TLSID = TSDL_AtomicInt;

  PSDL_ThreadPriority = ^TSDL_ThreadPriority;
  TSDL_ThreadPriority = longint;

const
  SDL_THREAD_PRIORITY_LOW = 0;
  SDL_THREAD_PRIORITY_NORMAL = 1;
  SDL_THREAD_PRIORITY_HIGH = 2;
  SDL_THREAD_PRIORITY_TIME_CRITICAL = 3;

type
  TSDL_ThreadFunction = function(Data: pointer): longint; cdecl;

  TSDL_Thread = record
  end;
  PSDL_Thread = ^TSDL_Thread;

function SDL_CreateThread(fn: TSDL_ThreadFunction; Name: pansichar; Data: pointer): PSDL_Thread; cdecl; external libSDL3;
function SDL_CreateThreadWithProperties(props: TSDL_PropertiesID): PSDL_Thread; cdecl; external libSDL3;

const
  SDL_PROP_THREAD_CREATE_ENTRY_FUNCTION_POINTER = 'SDL.thread.create.entry_function';
  SDL_PROP_THREAD_CREATE_NAME_STRING = 'SDL.thread.create.name';
  SDL_PROP_THREAD_CREATE_USERDATA_POINTER = 'SDL.thread.create.userdata';
  SDL_PROP_THREAD_CREATE_STACKSIZE_NUMBER = 'SDL.thread.create.stacksize';

  SDL_BeginThreadFunction = nil;
  SDL_EndThreadFunction = nil;

function SDL_CreateThreadRuntime(fn: TSDL_ThreadFunction; Name: pansichar; Data: pointer; pfnBeginThread: TSDL_FunctionPointer; pfnEndThread: TSDL_FunctionPointer): PSDL_Thread; cdecl; external libSDL3;
function SDL_CreateThreadWithPropertiesRuntime(props: TSDL_PropertiesID; pfnBeginThread: TSDL_FunctionPointer; pfnEndThread: TSDL_FunctionPointer): PSDL_Thread; cdecl; external libSDL3;

function SDL_GetThreadName(thread: PSDL_Thread): pansichar; cdecl; external libSDL3;
function SDL_GetCurrentThreadID: TSDL_ThreadID; cdecl; external libSDL3;
function SDL_GetThreadID(thread: PSDL_Thread): TSDL_ThreadID; cdecl; external libSDL3;
function SDL_SetCurrentThreadPriority(priority: TSDL_ThreadPriority): Tbool; cdecl; external libSDL3;
procedure SDL_WaitThread(thread: PSDL_Thread; status: Plongint); cdecl; external libSDL3;
procedure SDL_DetachThread(thread: PSDL_Thread); cdecl; external libSDL3;
function SDL_GetTLS(id: PSDL_TLSID): pointer; cdecl; external libSDL3;

type
  TSDL_TLSDestructorCallback = procedure(Value: pointer); cdecl;

function SDL_SetTLS(id: PSDL_TLSID; Value: pointer; destruc: TSDL_TLSDestructorCallback): Tbool; cdecl; external libSDL3;

procedure SDL_CleanupTLS; cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}
