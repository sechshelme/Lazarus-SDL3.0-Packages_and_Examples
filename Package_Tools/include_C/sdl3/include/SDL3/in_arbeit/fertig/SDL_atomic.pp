
unit SDL_atomic;
interface

{
  Automatically converted by H2Pas 1.0.0 from SDL_atomic.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_atomic.h
}

{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

Type
PSDL_AtomicInt  = ^SDL_AtomicInt;
PSDL_AtomicU32  = ^SDL_AtomicU32;
PSDL_SpinLock  = ^SDL_SpinLock;
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

function SDL_TryLockSpinlock(lock:PSDL_SpinLock):Tbool;cdecl;external;
procedure SDL_LockSpinlock(lock:PSDL_SpinLock);cdecl;external;
procedure SDL_UnlockSpinlock(lock:PSDL_SpinLock);cdecl;external;
procedure _ReadWriteBarrier;cdecl;external;
procedure SDL_CompilerBarrier;cdecl;external;
procedure SDL_MemoryBarrierReleaseFunction;cdecl;external;
procedure SDL_MemoryBarrierAcquireFunction;cdecl;external;
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

procedure SDL_CPUPauseInstruction;cdecl;external;
type
  PSDL_AtomicInt = ^TSDL_AtomicInt;
  TSDL_AtomicInt = record
      value : longint;
    end;

function SDL_CompareAndSwapAtomicInt(a:PSDL_AtomicInt; oldval:longint; newval:longint):Tbool;cdecl;external;
function SDL_SetAtomicInt(a:PSDL_AtomicInt; v:longint):longint;cdecl;external;
function SDL_GetAtomicInt(a:PSDL_AtomicInt):longint;cdecl;external;
function SDL_AddAtomicInt(a:PSDL_AtomicInt; v:longint):longint;cdecl;external;
type
  PSDL_AtomicU32 = ^TSDL_AtomicU32;
  TSDL_AtomicU32 = record
      value : TUint32;
    end;

function SDL_CompareAndSwapAtomicU32(a:PSDL_AtomicU32; oldval:TUint32; newval:TUint32):Tbool;cdecl;external;
function SDL_SetAtomicU32(a:PSDL_AtomicU32; v:TUint32):TUint32;cdecl;external;
function SDL_GetAtomicU32(a:PSDL_AtomicU32):TUint32;cdecl;external;
function SDL_CompareAndSwapAtomicPointer(a:Ppointer; oldval:pointer; newval:pointer):Tbool;cdecl;external;
function SDL_SetAtomicPointer(a:Ppointer; v:pointer):pointer;cdecl;external;
function SDL_GetAtomicPointer(a:Ppointer):pointer;cdecl;external;

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
