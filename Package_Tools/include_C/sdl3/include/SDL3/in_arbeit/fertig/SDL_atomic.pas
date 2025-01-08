unit SDL_atomic;

interface

uses
  ctypes, SDL_stdinc;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{$ifndef SDL_atomic_h_}
{$define SDL_atomic_h_}
{$include <SDL3/SDL_stdinc.h>}
{$include <SDL3/SDL_platform_defines.h>}
{$include <SDL3/SDL_begin_code.h>}
type
  PSDL_SpinLock = ^TSDL_SpinLock;
  TSDL_SpinLock = longint;

function SDL_TryLockSpinlock(lock:PSDL_SpinLock):Tbool;cdecl;external libSDL3;
procedure SDL_LockSpinlock(lock:PSDL_SpinLock);cdecl;external libSDL3;
procedure SDL_UnlockSpinlock(lock:PSDL_SpinLock);cdecl;external libSDL3;
procedure _ReadWriteBarrier;cdecl;external libSDL3;
procedure SDL_CompilerBarrier;cdecl;external libSDL3;
procedure SDL_MemoryBarrierReleaseFunction;cdecl;external libSDL3;
procedure SDL_MemoryBarrierAcquireFunction;cdecl;external libSDL3;
{$ifdef SDL_WIKI_DOCUMENTATION_SECTION}
{ was #define dname(params) para_def_expr }
{ return type might be wrong }   
function SDL_MemoryBarrierRelease : longint;

{ was #define dname(params) para_def_expr }
{ return type might be wrong }   
function SDL_MemoryBarrierAcquire : longint;

{ was #define dname(params) para_def_expr }
{ return type might be wrong }   
function SDL_MemoryBarrierRelease : longint;

{ was #define dname(params) para_def_expr }
{ return type might be wrong }   
function SDL_MemoryBarrierAcquire : longint;

{ was #define dname(params) para_def_expr }
{ return type might be wrong }   
function SDL_MemoryBarrierRelease : longint;

{ was #define dname(params) para_def_expr }
{ return type might be wrong }   
function SDL_MemoryBarrierAcquire : longint;

{$endif}
{$endif}

procedure SDL_CPUPauseInstruction;cdecl;external libSDL3;
type
  PSDL_AtomicInt = ^TSDL_AtomicInt;
  TSDL_AtomicInt = record
      value : longint;
    end;

function SDL_CompareAndSwapAtomicInt(a:PSDL_AtomicInt; oldval:longint; newval:longint):Tbool;cdecl;external libSDL3;
function SDL_SetAtomicInt(a:PSDL_AtomicInt; v:longint):longint;cdecl;external libSDL3;
function SDL_GetAtomicInt(a:PSDL_AtomicInt):longint;cdecl;external libSDL3;
function SDL_AddAtomicInt(a:PSDL_AtomicInt; v:longint):longint;cdecl;external libSDL3;
type
  PSDL_AtomicU32 = ^TSDL_AtomicU32;
  TSDL_AtomicU32 = record
      value : TUint32;
    end;

function SDL_CompareAndSwapAtomicU32(a:PSDL_AtomicU32; oldval:TUint32; newval:TUint32):Tbool;cdecl;external libSDL3;
function SDL_SetAtomicU32(a:PSDL_AtomicU32; v:TUint32):TUint32;cdecl;external libSDL3;
function SDL_GetAtomicU32(a:PSDL_AtomicU32):TUint32;cdecl;external libSDL3;
function SDL_CompareAndSwapAtomicPointer(a:Ppointer; oldval:pointer; newval:pointer):Tbool;cdecl;external libSDL3;
function SDL_SetAtomicPointer(a:Ppointer; v:pointer):pointer;cdecl;external libSDL3;
function SDL_GetAtomicPointer(a:Ppointer):pointer;cdecl;external libSDL3;

implementation

{ was #define dname(params) para_def_expr }
{ return type might be wrong }   
function SDL_MemoryBarrierRelease : longint;
begin
  SDL_MemoryBarrierRelease:=SDL_MemoryBarrierReleaseFunction;
end;

{ was #define dname(params) para_def_expr }
{ return type might be wrong }   
function SDL_MemoryBarrierAcquire : longint;
begin
  SDL_MemoryBarrierAcquire:=SDL_MemoryBarrierAcquireFunction;
end;

{ was #define dname(params) para_def_expr }
{ return type might be wrong }   
function SDL_MemoryBarrierRelease : longint;
begin
  SDL_MemoryBarrierRelease:=SDL_MemoryBarrierReleaseFunction;
end;

{ was #define dname(params) para_def_expr }
{ return type might be wrong }   
function SDL_MemoryBarrierAcquire : longint;
begin
  SDL_MemoryBarrierAcquire:=SDL_MemoryBarrierAcquireFunction;
end;

{ was #define dname(params) para_def_expr }
{ return type might be wrong }   
function SDL_MemoryBarrierRelease : longint;
begin
  SDL_MemoryBarrierRelease:=SDL_CompilerBarrier;
end;

{ was #define dname(params) para_def_expr }
{ return type might be wrong }   
function SDL_MemoryBarrierAcquire : longint;
begin
  SDL_MemoryBarrierAcquire:=SDL_CompilerBarrier;
end;


end.
