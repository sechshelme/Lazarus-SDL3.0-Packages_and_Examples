/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   17:58                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

const
  SDL_MUTEX_TIMEDOUT = 1;

type
  PSDL_Mutex = ^TSDL_Mutex;
  TSDL_Mutex = Pointer;  {undefined structure}

function SDL_CreateMutex: PSDL_Mutex; cdecl; external sdl3_lib;
procedure SDL_LockMutex(mutex: PSDL_Mutex); cdecl; external sdl3_lib;
function SDL_TryLockMutex(mutex: PSDL_Mutex): longint; cdecl; external sdl3_lib;
procedure SDL_UnlockMutex(mutex: PSDL_Mutex); cdecl; external sdl3_lib;
procedure SDL_DestroyMutex(mutex: PSDL_Mutex); cdecl; external sdl3_lib;

type
  PSDL_RWLock = ^TSDL_RWLock;
  TSDL_RWLock = Pointer;  {undefined structure}

const
  SDL_RWLOCK_TIMEDOUT = SDL_MUTEX_TIMEDOUT;

function SDL_CreateRWLock: PSDL_RWLock; cdecl; external sdl3_lib;
procedure SDL_LockRWLockForReading(rwlock: PSDL_RWLock); cdecl; external sdl3_lib;
procedure SDL_LockRWLockForWriting(rwlock: PSDL_RWLock); cdecl; external sdl3_lib;
function SDL_TryLockRWLockForReading(rwlock: PSDL_RWLock): longint; cdecl; external sdl3_lib;
function SDL_TryLockRWLockForWriting(rwlock: PSDL_RWLock): longint; cdecl; external sdl3_lib;
procedure SDL_UnlockRWLock(rwlock: PSDL_RWLock); cdecl; external sdl3_lib;
procedure SDL_DestroyRWLock(rwlock: PSDL_RWLock); cdecl; external sdl3_lib;

type
  PSDL_Semaphore = ^TSDL_Semaphore;
  TSDL_Semaphore = Pointer;  {undefined structure}

function SDL_CreateSemaphore(initial_value: uint32): PSDL_Semaphore; cdecl; external sdl3_lib;
procedure SDL_DestroySemaphore(sem: PSDL_Semaphore); cdecl; external sdl3_lib;
function SDL_WaitSemaphore(sem: PSDL_Semaphore): longint; cdecl; external sdl3_lib;
function SDL_TryWaitSemaphore(sem: PSDL_Semaphore): longint; cdecl; external sdl3_lib;
function SDL_WaitSemaphoreTimeout(sem: PSDL_Semaphore; timeoutMS: int32): longint; cdecl; external sdl3_lib;
function SDL_PostSemaphore(sem: PSDL_Semaphore): longint; cdecl; external sdl3_lib;
function SDL_GetSemaphoreValue(sem: PSDL_Semaphore): uint32; cdecl; external sdl3_lib;

type
  PSDL_Condition = ^TSDL_Condition;
  TSDL_Condition = Pointer;  {undefined structure}

function SDL_CreateCondition: PSDL_Condition; cdecl; external sdl3_lib;
procedure SDL_DestroyCondition(cond: PSDL_Condition); cdecl; external sdl3_lib;
function SDL_SignalCondition(cond: PSDL_Condition): longint; cdecl; external sdl3_lib;
function SDL_BroadcastCondition(cond: PSDL_Condition): longint; cdecl; external sdl3_lib;
function SDL_WaitCondition(cond: PSDL_Condition; mutex: PSDL_Mutex): longint; cdecl; external sdl3_lib;
function SDL_WaitConditionTimeout(cond: PSDL_Condition; mutex: PSDL_Mutex; timeoutMS: int32): longint; cdecl; external sdl3_lib;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}
