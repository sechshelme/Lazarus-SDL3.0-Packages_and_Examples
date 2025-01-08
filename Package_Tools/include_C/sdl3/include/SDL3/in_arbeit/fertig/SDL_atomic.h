
#ifndef SDL_atomic_h_
#define SDL_atomic_h_

#include <SDL3/SDL_stdinc.h>
#include <SDL3/SDL_platform_defines.h>

#include <SDL3/SDL_begin_code.h>


typedef int SDL_SpinLock;
extern  bool  SDL_TryLockSpinlock(SDL_SpinLock *lock);
extern  void  SDL_LockSpinlock(SDL_SpinLock *lock);
extern  void  SDL_UnlockSpinlock(SDL_SpinLock *lock);


void _ReadWriteBarrier(void);

extern  void SDL_CompilerBarrier(void);

extern  void  SDL_MemoryBarrierReleaseFunction(void);
extern  void  SDL_MemoryBarrierAcquireFunction(void);


#ifdef SDL_WIKI_DOCUMENTATION_SECTION

#define SDL_MemoryBarrierRelease() SDL_MemoryBarrierReleaseFunction()

#define SDL_MemoryBarrierAcquire() SDL_MemoryBarrierAcquireFunction()



#define SDL_MemoryBarrierRelease()   SDL_MemoryBarrierReleaseFunction()
#define SDL_MemoryBarrierAcquire()   SDL_MemoryBarrierAcquireFunction()

#define SDL_MemoryBarrierRelease()  SDL_CompilerBarrier()
#define SDL_MemoryBarrierAcquire()  SDL_CompilerBarrier()
#endif
#endif


    extern  void SDL_CPUPauseInstruction(void);

typedef struct SDL_AtomicInt { int value; } SDL_AtomicInt;

extern  bool  SDL_CompareAndSwapAtomicInt(SDL_AtomicInt *a, int oldval, int newval);
extern  int  SDL_SetAtomicInt(SDL_AtomicInt *a, int v);
extern  int  SDL_GetAtomicInt(SDL_AtomicInt *a);
extern  int  SDL_AddAtomicInt(SDL_AtomicInt *a, int v);


typedef struct SDL_AtomicU32 { Uint32 value; } SDL_AtomicU32;

extern  bool  SDL_CompareAndSwapAtomicU32(SDL_AtomicU32 *a, Uint32 oldval, Uint32 newval);
extern  Uint32  SDL_SetAtomicU32(SDL_AtomicU32 *a, Uint32 v);
extern  Uint32  SDL_GetAtomicU32(SDL_AtomicU32 *a);
extern  bool  SDL_CompareAndSwapAtomicPointer(void **a, void *oldval, void *newval);
extern  void *  SDL_SetAtomicPointer(void **a, void *v);
extern  void *  SDL_GetAtomicPointer(void **a);


