
unit SDL_cpuinfo;
interface

{
  Automatically converted by H2Pas 0.99.16 from SDL_cpuinfo.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_cpuinfo.h
}


{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{
  Simple DirectMedia Layer
  Copyright (C) 1997-2024 Sam Lantinga <slouken@libsdl.org>

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
 }
{ WIKI CATEGORY: CPUInfo  }
{*
 * # CategoryCPUInfo
 *
 * CPU feature detection for SDL.
 *
 * These functions are largely concerned with reporting if the system has
 * access to various SIMD instruction sets, but also has other important info
 * to share, such as system RAM size and number of logical CPU cores.
  }
{$ifndef SDL_cpuinfo_h_}
{$define SDL_cpuinfo_h_}
{$include <SDL3/SDL_stdinc.h>}
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{*
 * A guess for the cacheline size used for padding.
 *
 * Most x86 processors have a 64 byte cache line. The 64-bit PowerPC
 * processors have a 128 byte cache line. We use the larger value to be
 * generally safe.
 *
 * \since This macro is available since SDL 3.0.0.
  }

const
  SDL_CACHELINE_SIZE = 128;  
{*
 * Get the number of logical CPU cores available.
 *
 * \returns the total number of logical CPU cores. On CPUs that include
 *          technologies such as hyperthreading, the number of logical cores
 *          may be more than the number of physical cores.
 *
 * \since This function is available since SDL 3.0.0.
  }

function SDL_GetNumLogicalCPUCores:longint;cdecl;external;
{*
 * Determine the L1 cache line size of the CPU.
 *
 * This is useful for determining multi-threaded structure padding or SIMD
 * prefetch sizes.
 *
 * \returns the L1 cache line size of the CPU, in bytes.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetCPUCacheLineSize:longint;cdecl;external;
{*
 * Determine whether the CPU has AltiVec features.
 *
 * This always returns false on CPUs that aren't using PowerPC instruction
 * sets.
 *
 * \returns true if the CPU has AltiVec features or false if not.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_HasAltiVec:Tbool;cdecl;external;
{*
 * Determine whether the CPU has MMX features.
 *
 * This always returns false on CPUs that aren't using Intel instruction sets.
 *
 * \returns true if the CPU has MMX features or false if not.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_HasMMX:Tbool;cdecl;external;
{*
 * Determine whether the CPU has SSE features.
 *
 * This always returns false on CPUs that aren't using Intel instruction sets.
 *
 * \returns true if the CPU has SSE features or false if not.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_HasSSE2
 * \sa SDL_HasSSE3
 * \sa SDL_HasSSE41
 * \sa SDL_HasSSE42
  }
function SDL_HasSSE:Tbool;cdecl;external;
{*
 * Determine whether the CPU has SSE2 features.
 *
 * This always returns false on CPUs that aren't using Intel instruction sets.
 *
 * \returns true if the CPU has SSE2 features or false if not.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_HasSSE
 * \sa SDL_HasSSE3
 * \sa SDL_HasSSE41
 * \sa SDL_HasSSE42
  }
function SDL_HasSSE2:Tbool;cdecl;external;
{*
 * Determine whether the CPU has SSE3 features.
 *
 * This always returns false on CPUs that aren't using Intel instruction sets.
 *
 * \returns true if the CPU has SSE3 features or false if not.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_HasSSE
 * \sa SDL_HasSSE2
 * \sa SDL_HasSSE41
 * \sa SDL_HasSSE42
  }
function SDL_HasSSE3:Tbool;cdecl;external;
{*
 * Determine whether the CPU has SSE4.1 features.
 *
 * This always returns false on CPUs that aren't using Intel instruction sets.
 *
 * \returns true if the CPU has SSE4.1 features or false if not.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_HasSSE
 * \sa SDL_HasSSE2
 * \sa SDL_HasSSE3
 * \sa SDL_HasSSE42
  }
function SDL_HasSSE41:Tbool;cdecl;external;
{*
 * Determine whether the CPU has SSE4.2 features.
 *
 * This always returns false on CPUs that aren't using Intel instruction sets.
 *
 * \returns true if the CPU has SSE4.2 features or false if not.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_HasSSE
 * \sa SDL_HasSSE2
 * \sa SDL_HasSSE3
 * \sa SDL_HasSSE41
  }
function SDL_HasSSE42:Tbool;cdecl;external;
{*
 * Determine whether the CPU has AVX features.
 *
 * This always returns false on CPUs that aren't using Intel instruction sets.
 *
 * \returns true if the CPU has AVX features or false if not.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_HasAVX2
 * \sa SDL_HasAVX512F
  }
function SDL_HasAVX:Tbool;cdecl;external;
{*
 * Determine whether the CPU has AVX2 features.
 *
 * This always returns false on CPUs that aren't using Intel instruction sets.
 *
 * \returns true if the CPU has AVX2 features or false if not.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_HasAVX
 * \sa SDL_HasAVX512F
  }
function SDL_HasAVX2:Tbool;cdecl;external;
{*
 * Determine whether the CPU has AVX-512F (foundation) features.
 *
 * This always returns false on CPUs that aren't using Intel instruction sets.
 *
 * \returns true if the CPU has AVX-512F features or false if not.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_HasAVX
 * \sa SDL_HasAVX2
  }
function SDL_HasAVX512F:Tbool;cdecl;external;
{*
 * Determine whether the CPU has ARM SIMD (ARMv6) features.
 *
 * This is different from ARM NEON, which is a different instruction set.
 *
 * This always returns false on CPUs that aren't using ARM instruction sets.
 *
 * \returns true if the CPU has ARM SIMD features or false if not.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_HasNEON
  }
function SDL_HasARMSIMD:Tbool;cdecl;external;
{*
 * Determine whether the CPU has NEON (ARM SIMD) features.
 *
 * This always returns false on CPUs that aren't using ARM instruction sets.
 *
 * \returns true if the CPU has ARM NEON features or false if not.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_HasNEON:Tbool;cdecl;external;
{*
 * Determine whether the CPU has LSX (LOONGARCH SIMD) features.
 *
 * This always returns false on CPUs that aren't using LOONGARCH instruction
 * sets.
 *
 * \returns true if the CPU has LOONGARCH LSX features or false if not.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_HasLSX:Tbool;cdecl;external;
{*
 * Determine whether the CPU has LASX (LOONGARCH SIMD) features.
 *
 * This always returns false on CPUs that aren't using LOONGARCH instruction
 * sets.
 *
 * \returns true if the CPU has LOONGARCH LASX features or false if not.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_HasLASX:Tbool;cdecl;external;
{*
 * Get the amount of RAM configured in the system.
 *
 * \returns the amount of RAM configured in the system in MiB.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetSystemRAM:longint;cdecl;external;
{*
 * Report the alignment this system needs for SIMD allocations.
 *
 * This will return the minimum number of bytes to which a pointer must be
 * aligned to be compatible with SIMD instructions on the current machine. For
 * example, if the machine supports SSE only, it will return 16, but if it
 * supports AVX-512F, it'll return 64 (etc). This only reports values for
 * instruction sets SDL knows about, so if your SDL build doesn't have
 * SDL_HasAVX512F(), then it might return 16 for the SSE support it sees and
 * not 64 for the AVX-512 instructions that exist but SDL doesn't know about.
 * Plan accordingly.
 *
 * \returns the alignment in bytes needed for available, known SIMD
 *          instructions.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_aligned_alloc
 * \sa SDL_aligned_free
  }
function SDL_GetSIMDAlignment:Tsize_t;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_cpuinfo_h_  }

implementation


end.
