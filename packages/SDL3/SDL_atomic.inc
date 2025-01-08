
{$IFDEF read_interface}

type
  PSDL_SpinLock = ^TSDL_SpinLock;
  TSDL_SpinLock = longint;

function SDL_TryLockSpinlock(lock: PSDL_SpinLock): Tbool; cdecl; external libSDL3;
procedure SDL_LockSpinlock(lock: PSDL_SpinLock); cdecl; external libSDL3;
procedure SDL_UnlockSpinlock(lock: PSDL_SpinLock); cdecl; external libSDL3;
procedure _ReadWriteBarrier; cdecl; external libSDL3;
//procedure SDL_CompilerBarrier; cdecl; external libSDL3;
procedure SDL_MemoryBarrierReleaseFunction; cdecl; external libSDL3;
procedure SDL_MemoryBarrierAcquireFunction; cdecl; external libSDL3;

procedure SDL_CPUPauseInstruction; cdecl; external libSDL3;

type
  TSDL_AtomicInt = record
    Value: longint;
  end;
  PSDL_AtomicInt = ^TSDL_AtomicInt;

function SDL_CompareAndSwapAtomicInt(a: PSDL_AtomicInt; oldval: longint; newval: longint): Tbool; cdecl; external libSDL3;
function SDL_SetAtomicInt(a: PSDL_AtomicInt; v: longint): longint; cdecl; external libSDL3;
function SDL_GetAtomicInt(a: PSDL_AtomicInt): longint; cdecl; external libSDL3;
function SDL_AddAtomicInt(a: PSDL_AtomicInt; v: longint): longint; cdecl; external libSDL3;

type
  TSDL_AtomicU32 = record
    Value: TUint32;
  end;
  PSDL_AtomicU32 = ^TSDL_AtomicU32;

function SDL_CompareAndSwapAtomicU32(a: PSDL_AtomicU32; oldval: TUint32; newval: TUint32): Tbool; cdecl; external libSDL3;
function SDL_SetAtomicU32(a: PSDL_AtomicU32; v: TUint32): TUint32; cdecl; external libSDL3;
function SDL_GetAtomicU32(a: PSDL_AtomicU32): TUint32; cdecl; external libSDL3;
function SDL_CompareAndSwapAtomicPointer(a: Ppointer; oldval: pointer; newval: pointer): Tbool; cdecl; external libSDL3;
function SDL_SetAtomicPointer(a: Ppointer; v: pointer): pointer; cdecl; external libSDL3;
function SDL_GetAtomicPointer(a: Ppointer): pointer; cdecl; external libSDL3;

function SDL_AtomicIncRef(a: PSDL_AtomicInt): longint;
function SDL_AtomicDecRef(a: PSDL_AtomicInt): Boolean;

procedure SDL_MemoryBarrierRelease;
procedure SDL_MemoryBarrierAcquire;

{$ENDIF read_interface}


{$IFDEF read_implementation}

function SDL_AtomicIncRef(a: PSDL_AtomicInt): longint;
begin
  SDL_AtomicIncRef := SDL_AddAtomicInt(a, 1);
end;

function SDL_AtomicDecRef(a: PSDL_AtomicInt): Boolean;
begin
  SDL_AtomicDecRef := (SDL_AddAtomicInt(a, -(1))) = 1;
end;


procedure SDL_MemoryBarrierRelease;
begin
//  SDL_CompilerBarrier;
end;

procedure SDL_MemoryBarrierAcquire;
begin
//  SDL_CompilerBarrier;
end;



{$ENDIF read_implementation}
