unit SDL3_atomic;

interface

uses
  SDL3_stdinc;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  PSDL_SpinLock = ^TSDL_SpinLock;
  TSDL_SpinLock = longint;

function SDL_TryLockSpinlock(lock: PSDL_SpinLock): TSDL_bool; cdecl; external;
procedure SDL_LockSpinlock(lock: PSDL_SpinLock); cdecl; external;
procedure SDL_UnlockSpinlock(lock: PSDL_SpinLock); cdecl; external;
procedure _ReadWriteBarrier; cdecl; external;
procedure SDL_CompilerBarrier; cdecl; external;
procedure SDL_MemoryBarrierReleaseFunction; cdecl; external;
procedure SDL_MemoryBarrierAcquireFunction; cdecl; external;

type
  TSDL_KernelMemoryBarrierFunc = procedure; cdecl;

function SDL_MemoryBarrierRelease: TSDL_KernelMemoryBarrierFunc;
function SDL_MemoryBarrierAcquire: TSDL_KernelMemoryBarrierFunc;

procedure SDL_CPUPauseInstruction; cdecl; external;

type
  PSDL_AtomicInt = ^TSDL_AtomicInt;

  TSDL_AtomicInt = record
    Value: longint;
  end;

function SDL_AtomicCompareAndSwap(a: PSDL_AtomicInt; oldval: longint; newval: longint): TSDL_bool; cdecl; external;
function SDL_AtomicSet(a: PSDL_AtomicInt; v: longint): longint; cdecl; external;
function SDL_AtomicGet(a: PSDL_AtomicInt): longint; cdecl; external;
function SDL_AtomicAdd(a: PSDL_AtomicInt; v: longint): longint; cdecl; external;

function SDL_AtomicIncRef(a: PSDL_AtomicInt): longint;
function SDL_AtomicDecRef(a: PSDL_AtomicInt): TSDL_bool;

function SDL_AtomicCompareAndSwapPointer(a: Ppointer; oldval: pointer; newval: pointer): TSDL_bool; cdecl; external;
function SDL_AtomicSetPtr(a: Ppointer; v: pointer): pointer; cdecl; external;

implementation

function SDL_MemoryBarrierRelease: TSDL_KernelMemoryBarrierFunc;
begin
  SDL_MemoryBarrierRelease := @SDL_MemoryBarrierReleaseFunction;
end;

function SDL_MemoryBarrierAcquire: TSDL_KernelMemoryBarrierFunc;
begin
  SDL_MemoryBarrierAcquire := @SDL_MemoryBarrierAcquireFunction;
end;

function SDL_AtomicIncRef(a: PSDL_AtomicInt): longint;
begin
  SDL_AtomicIncRef := SDL_AtomicAdd(a, 1);
end;

function SDL_AtomicDecRef(a: PSDL_AtomicInt): TSDL_bool;
begin
  SDL_AtomicDecRef := TSDL_bool((SDL_AtomicAdd(a, -(1))) = 1);
end;


end.
