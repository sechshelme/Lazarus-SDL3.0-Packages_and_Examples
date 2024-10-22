unit SDL_mutex;

interface

uses
  SDL_stdinc, SDL_atomic, SDL_thread;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

  //function SDL_CAPABILITY(x : longint) : longint;
  //
  //const
  //  SDL_SCOPED_CAPABILITY = scoped_lockable;
  //
  //function SDL_GUARDED_BY(x : longint) : longint;
  //function SDL_PT_GUARDED_BY(x : longint) : longint;
  //function SDL_ACQUIRED_BEFORE(x : longint) : longint;
  //function SDL_ACQUIRED_AFTER(x : longint) : longint;
  //function SDL_REQUIRES(x : longint) : longint;
  //function SDL_REQUIRES_SHARED(x : longint) : longint;
  //function SDL_ACQUIRE(x : longint) : longint;
  //function SDL_ACQUIRE_SHARED(x : longint) : longint;
  //function SDL_RELEASE(x : longint) : longint;
  //function SDL_RELEASE_SHARED(x : longint) : longint;
  //function SDL_RELEASE_GENERIC(x : longint) : longint;
  //function SDL_TRY_ACQUIRE(x,y : longint) : longint;
  //function SDL_TRY_ACQUIRE_SHARED(x,y : longint) : longint;
  //function SDL_EXCLUDES(x : longint) : longint;
  //function SDL_ASSERT_CAPABILITY(x : longint) : longint;
  //function SDL_ASSERT_SHARED_CAPABILITY(x : longint) : longint;
  //function SDL_RETURN_CAPABILITY(x : longint) : longint;
  //
  //const
  //  SDL_NO_THREAD_SAFETY_ANALYSIS = no_thread_safety_analysis;
type
  TSDL_Mutex = record
  end;
  PSDL_Mutex = ^TSDL_Mutex;

  TSDL_RWLock = record
  end;
  PSDL_RWLock = ^TSDL_RWLock;

  TSDL_Semaphore = record
  end;
  PSDL_Semaphore = ^TSDL_Semaphore;

  TSDL_Condition = record
  end;
  PSDL_Condition = ^TSDL_Condition;

function SDL_CreateMutex: PSDL_Mutex; cdecl; external libSDL3;
procedure SDL_LockMutex(mutex: PSDL_Mutex); cdecl; external libSDL3;
function SDL_TryLockMutex(mutex: PSDL_Mutex): Tbool; cdecl; external libSDL3;
procedure SDL_UnlockMutex(mutex: PSDL_Mutex); cdecl; external libSDL3;
procedure SDL_DestroyMutex(mutex: PSDL_Mutex); cdecl; external libSDL3;
function SDL_CreateRWLock: PSDL_RWLock; cdecl; external libSDL3;
procedure SDL_LockRWLockForReading(rwlock: PSDL_RWLock); cdecl; external libSDL3;
procedure SDL_LockRWLockForWriting(rwlock: PSDL_RWLock); cdecl; external libSDL3;
function SDL_TryLockRWLockForReading(rwlock: PSDL_RWLock): Tbool; cdecl; external libSDL3;
function SDL_TryLockRWLockForWriting(rwlock: PSDL_RWLock): Tbool; cdecl; external libSDL3;
procedure SDL_UnlockRWLock(rwlock: PSDL_RWLock); cdecl; external libSDL3;
procedure SDL_DestroyRWLock(rwlock: PSDL_RWLock); cdecl; external libSDL3;
function SDL_CreateSemaphore(initial_value: TUint32): PSDL_Semaphore; cdecl; external libSDL3;
procedure SDL_DestroySemaphore(sem: PSDL_Semaphore); cdecl; external libSDL3;
procedure SDL_WaitSemaphore(sem: PSDL_Semaphore); cdecl; external libSDL3;
function SDL_TryWaitSemaphore(sem: PSDL_Semaphore): Tbool; cdecl; external libSDL3;
function SDL_WaitSemaphoreTimeout(sem: PSDL_Semaphore; timeoutMS: TSint32): Tbool; cdecl; external libSDL3;
procedure SDL_SignalSemaphore(sem: PSDL_Semaphore); cdecl; external libSDL3;
function SDL_GetSemaphoreValue(sem: PSDL_Semaphore): TUint32; cdecl; external libSDL3;
function SDL_CreateCondition: PSDL_Condition; cdecl; external libSDL3;
procedure SDL_DestroyCondition(cond: PSDL_Condition); cdecl; external libSDL3;
procedure SDL_SignalCondition(cond: PSDL_Condition); cdecl; external libSDL3;
procedure SDL_BroadcastCondition(cond: PSDL_Condition); cdecl; external libSDL3;
procedure SDL_WaitCondition(cond: PSDL_Condition; mutex: PSDL_Mutex); cdecl; external libSDL3;
function SDL_WaitConditionTimeout(cond: PSDL_Condition; mutex: PSDL_Mutex; timeoutMS: TSint32): Tbool; cdecl; external libSDL3;

type
  PSDL_InitStatus = ^TSDL_InitStatus;
  TSDL_InitStatus = longint;

const
  SDL_INIT_STATUS_UNINITIALIZED = 0;
  SDL_INIT_STATUS_INITIALIZING = 1;
  SDL_INIT_STATUS_INITIALIZED = 2;
  SDL_INIT_STATUS_UNINITIALIZING = 3;

type
  TSDL_InitState = record
    status: TSDL_AtomicInt;
    thread: TSDL_ThreadID;
    reserved: pointer;
  end;
  PSDL_InitState = ^TSDL_InitState;

function SDL_ShouldInit(state: PSDL_InitState): Tbool; cdecl; external libSDL3;
function SDL_ShouldQuit(state: PSDL_InitState): Tbool; cdecl; external libSDL3;
procedure SDL_SetInitialized(state: PSDL_InitState; initialized: Tbool); cdecl; external libSDL3;

implementation

//function SDL_CAPABILITY(x : longint) : longint;
//begin
//  SDL_CAPABILITY:=capability(x);
//end;
//
//function SDL_GUARDED_BY(x : longint) : longint;
//begin
//  SDL_GUARDED_BY:=guarded_by(x);
//end;
//
//function SDL_PT_GUARDED_BY(x : longint) : longint;
//begin
//  SDL_PT_GUARDED_BY:=pt_guarded_by(x);
//end;
//
//function SDL_ACQUIRED_BEFORE(x : longint) : longint;
//begin
//  SDL_ACQUIRED_BEFORE:=acquired_before(x);
//end;
//
//function SDL_ACQUIRED_AFTER(x : longint) : longint;
//begin
//  SDL_ACQUIRED_AFTER:=acquired_after(x);
//end;
//
//function SDL_REQUIRES(x : longint) : longint;
//begin
//  SDL_REQUIRES:=requires_capability(x);
//end;
//
//function SDL_REQUIRES_SHARED(x : longint) : longint;
//begin
//  SDL_REQUIRES_SHARED:=requires_shared_capability(x);
//end;
//
//function SDL_ACQUIRE(x : longint) : longint;
//begin
//  SDL_ACQUIRE:=acquire_capability(x);
//end;
//
//function SDL_ACQUIRE_SHARED(x : longint) : longint;
//begin
//  SDL_ACQUIRE_SHARED:=acquire_shared_capability(x);
//end;
//
//function SDL_RELEASE(x : longint) : longint;
//begin
//  SDL_RELEASE:=release_capability(x);
//end;
//
//function SDL_RELEASE_SHARED(x : longint) : longint;
//begin
//  SDL_RELEASE_SHARED:=release_shared_capability(x);
//end;
//
//function SDL_RELEASE_GENERIC(x : longint) : longint;
//begin
//  SDL_RELEASE_GENERIC:=release_generic_capability(x);
//end;
//
//function SDL_TRY_ACQUIRE(x,y : longint) : longint;
//begin
//  SDL_TRY_ACQUIRE:=try_acquire_capability(x,y);
//end;
//
//function SDL_TRY_ACQUIRE_SHARED(x,y : longint) : longint;
//begin
//  SDL_TRY_ACQUIRE_SHARED:=try_acquire_shared_capability(x,y);
//end;
//
//function SDL_EXCLUDES(x : longint) : longint;
//begin
//  SDL_EXCLUDES:=locks_excluded(x);
//end;
//
//function SDL_ASSERT_CAPABILITY(x : longint) : longint;
//begin
//  SDL_ASSERT_CAPABILITY:=assert_capability(x);
//end;
//
//function SDL_ASSERT_SHARED_CAPABILITY(x : longint) : longint;
//begin
//  SDL_ASSERT_SHARED_CAPABILITY:=assert_shared_capability(x);
//end;
//
//function SDL_RETURN_CAPABILITY(x : longint) : longint;
//begin
//  SDL_RETURN_CAPABILITY:=lock_returned(x);
//end;


end.
