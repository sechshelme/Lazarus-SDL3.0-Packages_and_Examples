  {$IFDEF read_interface}

type
  TSDL_AsyncIO = record
  end;
  PSDL_AsyncIO = ^TSDL_AsyncIO;

type
  PSDL_AsyncIOTaskType = ^TSDL_AsyncIOTaskType;
  TSDL_AsyncIOTaskType = longint;

const
  SDL_ASYNCIO_TASK_READ = 0;
  SDL_ASYNCIO_TASK_WRITE = 1;
  SDL_ASYNCIO_TASK_CLOSE = 2;

type
  PSDL_AsyncIOResult = ^TSDL_AsyncIOResult;
  TSDL_AsyncIOResult = longint;

const
  SDL_ASYNCIO_COMPLETE = 0;
  SDL_ASYNCIO_FAILURE = 1;
  SDL_ASYNCIO_CANCELED = 2;

type
  TSDL_AsyncIOOutcome = record
    asyncio: PSDL_AsyncIO;
    _type: TSDL_AsyncIOTaskType;
    Result: TSDL_AsyncIOResult;
    buffer: pointer;
    offset: TUint64;
    bytes_requested: TUint64;
    bytes_transferred: TUint64;
    userdata: pointer;
  end;
  PSDL_AsyncIOOutcome = ^TSDL_AsyncIOOutcome;

type
  TSDL_AsyncIOQueue = record
  end;
  PSDL_AsyncIOQueue = ^TSDL_AsyncIOQueue;

function SDL_AsyncIOFromFile(file_: pchar; mode: pchar): PSDL_AsyncIO; cdecl; external libSDL3;
function SDL_GetAsyncIOSize(asyncio: PSDL_AsyncIO): TSint64; cdecl; external libSDL3;
function SDL_ReadAsyncIO(asyncio: PSDL_AsyncIO; ptr: pointer; offset: TUint64; size: TUint64; queue: PSDL_AsyncIOQueue;
  userdata: pointer): Tbool; cdecl; external libSDL3;
function SDL_WriteAsyncIO(asyncio: PSDL_AsyncIO; ptr: pointer; offset: TUint64; size: TUint64; queue: PSDL_AsyncIOQueue;
  userdata: pointer): Tbool; cdecl; external libSDL3;
function SDL_CloseAsyncIO(asyncio: PSDL_AsyncIO; flush: Tbool; queue: PSDL_AsyncIOQueue; userdata: pointer): Tbool; cdecl; external libSDL3;
function SDL_CreateAsyncIOQueue: PSDL_AsyncIOQueue; cdecl; external libSDL3;
procedure SDL_DestroyAsyncIOQueue(queue: PSDL_AsyncIOQueue); cdecl; external libSDL3;
function SDL_GetAsyncIOResult(queue: PSDL_AsyncIOQueue; outcome: PSDL_AsyncIOOutcome): Tbool; cdecl; external libSDL3;
function SDL_WaitAsyncIOResult(queue: PSDL_AsyncIOQueue; outcome: PSDL_AsyncIOOutcome; timeoutMS: TSint32): Tbool; cdecl; external libSDL3;
procedure SDL_SignalAsyncIOQueue(queue: PSDL_AsyncIOQueue); cdecl; external libSDL3;
function SDL_LoadFileAsync(file_: pchar; queue: PSDL_AsyncIOQueue; userdata: pointer): Tbool; cdecl; external libSDL3;


{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}
