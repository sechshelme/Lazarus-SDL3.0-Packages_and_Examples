/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 30-06-2024   17:38                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_SpinLock = ^TSDL_SpinLock;
  TSDL_SpinLock = longint;

function SDL_TryLockSpinlock(lock: PSDL_SpinLock): TSDL_bool; cdecl; external sdl3_lib;
procedure SDL_LockSpinlock(lock: PSDL_SpinLock); cdecl; external sdl3_lib;
procedure SDL_UnlockSpinlock(lock: PSDL_SpinLock); cdecl; external sdl3_lib;
procedure _ReadWriteBarrier; cdecl; external sdl3_lib;
procedure SDL_CompilerBarrier; cdecl; external sdl3_lib;
procedure SDL_MemoryBarrierReleaseFunction; cdecl; external sdl3_lib;
procedure SDL_MemoryBarrierAcquireFunction; cdecl; external sdl3_lib;

type
  TSDL_KernelMemoryBarrierFunc = procedure; cdecl;

function SDL_MemoryBarrierRelease: TSDL_KernelMemoryBarrierFunc;
function SDL_MemoryBarrierAcquire: TSDL_KernelMemoryBarrierFunc;

procedure SDL_CPUPauseInstruction; cdecl; external sdl3_lib;

type
  PSDL_AtomicInt = ^TSDL_AtomicInt;

  TSDL_AtomicInt = record
    Value: longint;
  end;

function SDL_AtomicCompareAndSwap(a: PSDL_AtomicInt; oldval: longint; newval: longint): TSDL_bool; cdecl; external sdl3_lib;
function SDL_AtomicSet(a: PSDL_AtomicInt; v: longint): longint; cdecl; external sdl3_lib;
function SDL_AtomicGet(a: PSDL_AtomicInt): longint; cdecl; external sdl3_lib;
function SDL_AtomicAdd(a: PSDL_AtomicInt; v: longint): longint; cdecl; external sdl3_lib;

function SDL_AtomicIncRef(a: PSDL_AtomicInt): longint;
function SDL_AtomicDecRef(a: PSDL_AtomicInt): TSDL_bool;

function SDL_AtomicCompareAndSwapPointer(a: Ppointer; oldval: pointer; newval: pointer): TSDL_bool; cdecl; external sdl3_lib;
function SDL_AtomicSetPtr(a: Ppointer; v: pointer): pointer; cdecl; external sdl3_lib;

{$ENDIF read_interface}


{$IFDEF read_implementation}

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


{$ENDIF read_implementation}
