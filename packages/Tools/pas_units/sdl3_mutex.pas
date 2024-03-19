unit SDL3_mutex;

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

const
  SDL_MUTEX_TIMEDOUT = 1;

type
  PSDL_Mutex = ^TSDL_Mutex;
  TSDL_Mutex = Pointer;  {undefined structure}

function SDL_CreateMutex: PSDL_Mutex; cdecl; external;
procedure SDL_LockMutex(mutex: PSDL_Mutex); cdecl; external;
function SDL_TryLockMutex(mutex: PSDL_Mutex): longint; cdecl; external;
procedure SDL_UnlockMutex(mutex: PSDL_Mutex); cdecl; external;
procedure SDL_DestroyMutex(mutex: PSDL_Mutex); cdecl; external;

type
  PSDL_RWLock = ^TSDL_RWLock;
  TSDL_RWLock = Pointer;  {undefined structure}

const
  SDL_RWLOCK_TIMEDOUT = SDL_MUTEX_TIMEDOUT;

function SDL_CreateRWLock: PSDL_RWLock; cdecl; external;
procedure SDL_LockRWLockForReading(rwlock: PSDL_RWLock); cdecl; external;
procedure SDL_LockRWLockForWriting(rwlock: PSDL_RWLock); cdecl; external;
function SDL_TryLockRWLockForReading(rwlock: PSDL_RWLock): longint; cdecl; external;
function SDL_TryLockRWLockForWriting(rwlock: PSDL_RWLock): longint; cdecl; external;
procedure SDL_UnlockRWLock(rwlock: PSDL_RWLock); cdecl; external;
procedure SDL_DestroyRWLock(rwlock: PSDL_RWLock); cdecl; external;

type
  PSDL_Semaphore = ^TSDL_Semaphore;
  TSDL_Semaphore = Pointer;  {undefined structure}

function SDL_CreateSemaphore(initial_value: uint32): PSDL_Semaphore; cdecl; external;
procedure SDL_DestroySemaphore(sem: PSDL_Semaphore); cdecl; external;
function SDL_WaitSemaphore(sem: PSDL_Semaphore): longint; cdecl; external;
function SDL_TryWaitSemaphore(sem: PSDL_Semaphore): longint; cdecl; external;
function SDL_WaitSemaphoreTimeout(sem: PSDL_Semaphore; timeoutMS: int32): longint; cdecl; external;
function SDL_PostSemaphore(sem: PSDL_Semaphore): longint; cdecl; external;
function SDL_GetSemaphoreValue(sem: PSDL_Semaphore): uint32; cdecl; external;

type
  PSDL_Condition = ^TSDL_Condition;
  TSDL_Condition = Pointer;  {undefined structure}

function SDL_CreateCondition: PSDL_Condition; cdecl; external;
procedure SDL_DestroyCondition(cond: PSDL_Condition); cdecl; external;
function SDL_SignalCondition(cond: PSDL_Condition): longint; cdecl; external;
function SDL_BroadcastCondition(cond: PSDL_Condition): longint; cdecl; external;
function SDL_WaitCondition(cond: PSDL_Condition; mutex: PSDL_Mutex): longint; cdecl; external;
function SDL_WaitConditionTimeout(cond: PSDL_Condition; mutex: PSDL_Mutex; timeoutMS: int32): longint; cdecl; external;

implementation

end.
