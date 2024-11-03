
unit SDL_endian;
interface

{
  Automatically converted by H2Pas 0.99.16 from SDL_endian.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_endian.h
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
{*
 * # CategoryEndian
 *
 * Functions for reading and writing endian-specific values.
  }
{$ifndef SDL_endian_h_}
{$define SDL_endian_h_}
{$include <SDL3/SDL_stdinc.h>}
{$if defined(_MSC_VER) && (_MSC_VER >= 1400)}
{ As of Clang 11, '_m_prefetchw' is conflicting with the winnt.h's version,
   so we define the needed '_m_prefetch' here as a pseudo-header, until the issue is fixed.  }
{$ifdef __clang__}
{$ifndef __PRFCHWINTRIN_H}
{$define __PRFCHWINTRIN_H}
(* error 
static __inline__ void __attribute__((__always_inline__, __nodebug__))
(* error 
static __inline__ void __attribute__((__always_inline__, __nodebug__))
 in declarator_list *)
 in declarator_list *)
{ _MM_HINT_T0  }{$endif}
{ __PRFCHWINTRIN_H  }
{$endif}
{ __clang__  }
{$include <intrin.h>}
{$endif}
{*
 *  \name The two types of endianness
  }
{ @  }

const
  SDL_LIL_ENDIAN = 1234;  
  SDL_BIG_ENDIAN = 4321;  
{ @  }
{$ifndef SDL_BYTEORDER}
{$ifdef SDL_PLATFORM_LINUX}
{$include <endian.h>}
  SDL_BYTEORDER = __BYTE_ORDER;  
(*** was #elif ****){$else defined(SDL_PLATFORM_SOLARIS)}
{$include <sys/byteorder.h>}
{$if defined(_LITTLE_ENDIAN)}
  SDL_BYTEORDER = SDL_LIL_ENDIAN;  
(*** was #elif ****){$else defined(_BIG_ENDIAN)}
  SDL_BYTEORDER = SDL_BIG_ENDIAN;  
{$else}
{$error Unsupported endianness}
{$endif}
(*** was #elif ****){$else defined(SDL_PLATFORM_OPENBSD) || defined(__DragonFly__)}
{$include <endian.h>}
  SDL_BYTEORDER = BYTE_ORDER;  
(*** was #elif ****){$else defined(SDL_PLATFORM_FREEBSD) || defined(SDL_PLATFORM_NETBSD)}
{$include <sys/endian.h>}
  SDL_BYTEORDER = BYTE_ORDER;  
{ predefs from newer gcc and clang versions:  }
(*** was #elif ****){$else defined(__ORDER_LITTLE_ENDIAN__) && defined(__ORDER_BIG_ENDIAN__) && defined(__BYTE_ORDER__)}
{$if (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)}
  SDL_BYTEORDER = SDL_LIL_ENDIAN;  
(*** was #elif ****){$else (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)}
  SDL_BYTEORDER = SDL_BIG_ENDIAN;  
{$else}
{$error Unsupported endianness}
{$endif}
{ }
{$else}
{$if defined(__hppa__) || \}
(* error 
    defined(__m68k__) || defined(mc68000) || defined(_M_M68K) || \
{$else}
{$endif}
{$endif}
{ SDL_PLATFORM_LINUX  }
{$endif}
{ !SDL_BYTEORDER  }
{$ifndef SDL_FLOATWORDORDER}
{ predefs from newer gcc versions:  }
{$if defined(__ORDER_LITTLE_ENDIAN__) && defined(__ORDER_BIG_ENDIAN__) && defined(__FLOAT_WORD_ORDER__)}
{$if (__FLOAT_WORD_ORDER__ == __ORDER_LITTLE_ENDIAN__)}
(*** was #elif ****){$else (__FLOAT_WORD_ORDER__ == __ORDER_BIG_ENDIAN__)}
{$else}
{$error Unsupported endianness}
{$endif}
{ }
(*** was #elif ****){$else defined(__MAVERICK__)}
{ For Maverick, float words are always little-endian.  }
(*** was #elif ****){$else (defined(__arm__) || defined(__thumb__)) && !defined(__VFP_FP__) && !defined(__ARM_EABI__)}
{ For FPA, float words are always big-endian.  }
{$else}
{ By default, assume that floats words follow the memory system mode.  }
{$endif}
{ __FLOAT_WORD_ORDER__  }
{$endif}
{ !SDL_FLOATWORDORDER  }
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{*
 *  \file SDL_endian.h
  }
{ various modern compilers may have builtin swap  }
{$if defined(__GNUC__) || defined(__clang__)}
{ this one is broken  }
{$else}
{$endif}
{ Byte swap 16-bit integer.  }
{$if HAS_BUILTIN_BSWAP16}
(*** was #elif ****){$else (defined(_MSC_VER) && (_MSC_VER >= 1400)) && !defined(__ICL)}
(** unsupported pragma#pragma intrinsic(_byteswap_ushort)*)
(*** was #elif ****){$else defined(__i386__) && !HAS_BROKEN_BSWAP}
 in declarator_list *)
(* error 
  __asm__("xchgb %b0,%h0": "=q"(x):"0"(x));
 in declarator_list *)
  var
    x : Treturn;cvar;public;
(* error 
}
(*** was #elif ****){$else defined(__x86_64__)}
in declaration at line 165 *)
        x : Treturn;cvar;public;
(* error 
}
(*** was #elif ****){$else (defined(__powerpc__) || defined(__ppc__))}
in declaration at line 171 *)
(* error 
  __asm__("rlwimi %0,%2,8,16,23": "=&r"(result):"0"(x >> 8), "r"(x));
(* error 
  __asm__("rlwimi %0,%2,8,16,23": "=&r"(result):"0"(x >> 8), "r"(x));
 in declarator_list *)
 in declarator_list *)
(* error 
    return (Uint16)result;
 in declarator_list *)
(* error 
}
(*** was #elif ****){$else (defined(__m68k__) && !defined(__mcoldfire__))}
in declaration at line 179 *)
        x : Treturn;cvar;public;
(* error 
}
(*** was #elif ****){$else defined(__WATCOMC__) && defined(__386__)}
in declaration at line 183 *)
(** unsupported pragma#pragma aux SDL_Swap16 = \*)
(* error 
  "xchg al, ah" \
in declaration at line 187 *)
{$else}
(* error 
SDL_FORCE_INLINE Uint16 SDL_Swap16(Uint16 x)
 in declarator_list *)
(* error 
    return SDL_static_cast(Uint16, ((x << 8) | (x >> 8)));
(* error 
    return SDL_static_cast(Uint16, ((x << 8) | (x >> 8)));
 in declarator_list *)
 in declarator_list *)
(* error 
}
{$endif}
{ Byte swap 32-bit integer.  }
{$if HAS_BUILTIN_BSWAP32}
in declaration at line 197 *)
(*** was #elif ****){$else (defined(_MSC_VER) && (_MSC_VER >= 1400)) && !defined(__ICL)}
(** unsupported pragma#pragma intrinsic(_byteswap_ulong)*)
    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   

    function SDL_Swap32(x : longint) : longint;    

(*** was #elif ****){$else defined(__i386__) && !HAS_BROKEN_BSWAP}
(* error 
SDL_FORCE_INLINE Uint32 SDL_Swap32(Uint32 x)
 in declarator_list *)
(* error 
  __asm__("bswap %0": "=r"(x):"0"(x));
 in declarator_list *)
      var
        x : Treturn;cvar;public;
(* error 
}
(*** was #elif ****){$else defined(__x86_64__)}
in declaration at line 210 *)
        x : Treturn;cvar;public;
(* error 
}
(*** was #elif ****){$else (defined(__powerpc__) || defined(__ppc__))}
in declaration at line 216 *)
(* error 
  __asm__("rlwimi %0,%2,24,16,23": "=&r"(result): "0" (x>>24),  "r"(x));
(* error 
  __asm__("rlwimi %0,%2,24,16,23": "=&r"(result): "0" (x>>24),  "r"(x));
 in declarator_list *)
 in declarator_list *)
(* error 
  __asm__("rlwimi %0,%2,8,8,15"  : "=&r"(result): "0" (result), "r"(x));
(* error 
  __asm__("rlwimi %0,%2,8,8,15"  : "=&r"(result): "0" (result), "r"(x));
 in declarator_list *)
 in declarator_list *)
(* error 
  __asm__("rlwimi %0,%2,24,0,7"  : "=&r"(result): "0" (result), "r"(x));
(* error 
  __asm__("rlwimi %0,%2,24,0,7"  : "=&r"(result): "0" (result), "r"(x));
 in declarator_list *)
 in declarator_list *)
        result : Treturn;cvar;public;
(* error 
}
(*** was #elif ****){$else (defined(__m68k__) && !defined(__mcoldfire__))}
in declaration at line 226 *)
        x : Treturn;cvar;public;
(* error 
}
(*** was #elif ****){$else defined(__WATCOMC__) && defined(__386__)}
in declaration at line 230 *)
(** unsupported pragma#pragma aux SDL_Swap32 = \*)
(* error 
  "bswap eax"  \
in declaration at line 234 *)
{$else}
(* error 
SDL_FORCE_INLINE Uint32 SDL_Swap32(Uint32 x)
 in declarator_list *)
(* error 
    return SDL_static_cast(Uint32, ((x << 24) | ((x << 8) & 0x00FF0000) |
(* error 
    return SDL_static_cast(Uint32, ((x << 24) | ((x << 8) & 0x00FF0000) |
 in declarator_list *)
 in declarator_list *)
(* error 
}
{$endif}
{ Byte swap 64-bit integer.  }
{$if HAS_BUILTIN_BSWAP64}
in declaration at line 245 *)
(*** was #elif ****){$else (defined(_MSC_VER) && (_MSC_VER >= 1400)) && !defined(__ICL)}
(** unsupported pragma#pragma intrinsic(_byteswap_uint64)*)
    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   

    function SDL_Swap64(x : longint) : longint;    

(*** was #elif ****){$else defined(__i386__) && !HAS_BROKEN_BSWAP}
(* error 
SDL_FORCE_INLINE Uint64 SDL_Swap64(Uint64 x)
 in declarator_list *)
(* error 
    union {
 in declarator_list *)
(* error 
        struct {
 in declarator_list *)
(* error 
            Uint32 a, b;
 in declarator_list *)
      var
        b : TSDL_FORCE_INLINE;cvar;public;
(* error 
        } s;
in declaration at line 255 *)
        u : TUint64;cvar;public;
(* error 
    } v;
in declaration at line 257 *)
(* error 
    v.u = x;
(* error 
    v.u = x;
 in declarator_list *)
(* error 
  __asm__("bswapl %0 ; bswapl %1 ; xchgl %0,%1"
(* error 
          : "=r"(v.s.a), "=r"(v.s.b)
(* error 
          : "0" (v.s.a),  "1"(v.s.b));
 in declarator_list *)
 in declarator_list *)
 in declarator_list *)
(* error 
    return v.u;
 in declarator_list *)
(* error 
}
(*** was #elif ****){$else defined(__x86_64__)}
in declaration at line 267 *)
        x : Treturn;cvar;public;
(* error 
}
(*** was #elif ****){$else defined(__WATCOMC__) && defined(__386__)}
in declaration at line 271 *)
(** unsupported pragma#pragma aux SDL_Swap64 = \*)
(* error 
  "bswap eax"     \
in declaration at line 277 *)
{$else}
(* error 
SDL_FORCE_INLINE Uint64 SDL_Swap64(Uint64 x)
 in declarator_list *)
(* error 
    Uint32 hi, lo;
 in declarator_list *)
        lo : TSDL_FORCE_INLINE;cvar;public;
{ Separate into high and low 32-bit values and swap them  }
(* error 
    lo = SDL_static_cast(Uint32, x & 0xFFFFFFFF);
in declaration at line 284 *)
(* error 
    x >>= 32;
in declaration at line 285 *)
(* error 
    hi = SDL_static_cast(Uint32, x & 0xFFFFFFFF);
in declaration at line 286 *)
(* error 
    x = SDL_Swap32(lo);
in declaration at line 287 *)
(* error 
    x <<= 32;
in declaration at line 288 *)
(* error 
    x |= SDL_Swap32(hi);
in declaration at line 289 *)
        x : Treturn;cvar;public;
(* error 
}
{$endif}
{*
 * Byte-swap a floating point number.
 *
 * This will always byte-swap the value, whether it's currently in the native
 * byteorder of the system or not. You should use SDL_SwapFloatLE or
 * SDL_SwapFloatBE instead, in most cases.
 *
 * Note that this is a forced-inline function in a header, and not a public
 * API function available in the SDL library (which is to say, the code is
 * embedded in the calling program and the linker and dynamic loader will not
 * be able to find this function inside SDL itself).
 *
 * \param x the value to byte-swap.
 * \returns x, with its bytes in the opposite endian order.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.1.3.
  }
in declaration at line 317 *)
        ui32 : TUint32;cvar;public;
(* error 
    } swapper;
in declaration at line 319 *)
(* error 
    swapper.f = x;
(* error 
    swapper.f = x;
 in declarator_list *)
(* error 
    swapper.ui32 = SDL_Swap32(swapper.ui32);
(* error 
    swapper.ui32 = SDL_Swap32(swapper.ui32);
 in declarator_list *)
(* error 
    return swapper.f;
 in declarator_list *)
(* error 
}
{ remove extra macros  }
{$undef HAS_BROKEN_BSWAP}
{$undef HAS_BUILTIN_BSWAP16}
{$undef HAS_BUILTIN_BSWAP32}
{$undef HAS_BUILTIN_BSWAP64}
{$ifdef SDL_WIKI_DOCUMENTATION_SECTION}
{*
 * Byte-swap an unsigned 16-bit number.
 *
 * This will always byte-swap the value, whether it's currently in the native
 * byteorder of the system or not. You should use SDL_Swap16LE or SDL_Swap16BE
 * instead, in most cases.
 *
 * Note that this is a forced-inline function in a header, and not a public
 * API function available in the SDL library (which is to say, the code is
 * embedded in the calling program and the linker and dynamic loader will not
 * be able to find this function inside SDL itself).
 *
 * \param x the value to byte-swap.
 * \returns `x`, with its bytes in the opposite endian order.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.1.3.
  }
in declaration at line 353 *)
(* error 
SDL_FORCE_INLINE Uint16 SDL_Swap16(Uint16 x) { return x_but_byteswapped; }
{*
 * Byte-swap an unsigned 32-bit number.
 *
 * This will always byte-swap the value, whether it's currently in the native
 * byteorder of the system or not. You should use SDL_Swap32LE or SDL_Swap32BE
 * instead, in most cases.
 *
 * Note that this is a forced-inline function in a header, and not a public
 * API function available in the SDL library (which is to say, the code is
 * embedded in the calling program and the linker and dynamic loader will not
 * be able to find this function inside SDL itself).
 *
 * \param x the value to byte-swap.
 * \returns `x`, with its bytes in the opposite endian order.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.1.3.
  }
in declaration at line 374 *)
(* error 
SDL_FORCE_INLINE Uint32 SDL_Swap32(Uint32 x) { return x_but_byteswapped; }
{*
 * Byte-swap an unsigned 64-bit number.
 *
 * This will always byte-swap the value, whether it's currently in the native
 * byteorder of the system or not. You should use SDL_Swap64LE or SDL_Swap64BE
 * instead, in most cases.
 *
 * Note that this is a forced-inline function in a header, and not a public
 * API function available in the SDL library (which is to say, the code is
 * embedded in the calling program and the linker and dynamic loader will not
 * be able to find this function inside SDL itself).
 *
 * \param x the value to byte-swap.
 * \returns `x`, with its bytes in the opposite endian order.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.1.3.
  }
in declaration at line 395 *)
(* error 
SDL_FORCE_INLINE Uint32 SDL_Swap64(Uint64 x) { return x_but_byteswapped; }
{*
 * Swap a 16-bit value from littleendian to native byte order.
 *
 * If this is running on a littleendian system, `x` is returned unchanged.
 *
 * This macro never references `x` more than once, avoiding side effects.
 *
 * \param x the value to swap, in littleendian byte order.
 * \returns `x` in native byte order.
 *
 * \threadsafety It is safe to call this macro from any thread.
 *
 * \since This macro is available since SDL 3.1.3.
  }
in declaration at line 411 *)
{*
 * Swap a 32-bit value from littleendian to native byte order.
 *
 * If this is running on a littleendian system, `x` is returned unchanged.
 *
 * This macro never references `x` more than once, avoiding side effects.
 *
 * \param x the value to swap, in littleendian byte order.
 * \returns `x` in native byte order.
 *
 * \threadsafety It is safe to call this macro from any thread.
 *
 * \since This macro is available since SDL 3.1.3.
  }
    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   

    function SDL_Swap32LE(x : longint) : longint;    

{*
 * Swap a 64-bit value from littleendian to native byte order.
 *
 * If this is running on a littleendian system, `x` is returned unchanged.
 *
 * This macro never references `x` more than once, avoiding side effects.
 *
 * \param x the value to swap, in littleendian byte order.
 * \returns `x` in native byte order.
 *
 * \threadsafety It is safe to call this macro from any thread.
 *
 * \since This macro is available since SDL 3.1.3.
  }
    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap64LE(x : longint) : longint;    

{*
 * Swap a floating point value from littleendian to native byte order.
 *
 * If this is running on a littleendian system, `x` is returned unchanged.
 *
 * This macro never references `x` more than once, avoiding side effects.
 *
 * \param x the value to swap, in littleendian byte order.
 * \returns `x` in native byte order.
 *
 * \threadsafety It is safe to call this macro from any thread.
 *
 * \since This macro is available since SDL 3.1.3.
  }
    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_SwapFloatLE(x : longint) : longint;    

{*
 * Swap a 16-bit value from bigendian to native byte order.
 *
 * If this is running on a bigendian system, `x` is returned unchanged.
 *
 * This macro never references `x` more than once, avoiding side effects.
 *
 * \param x the value to swap, in bigendian byte order.
 * \returns `x` in native byte order.
 *
 * \threadsafety It is safe to call this macro from any thread.
 *
 * \since This macro is available since SDL 3.1.3.
  }
    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap16BE(x : longint) : longint;    

{*
 * Swap a 32-bit value from bigendian to native byte order.
 *
 * If this is running on a bigendian system, `x` is returned unchanged.
 *
 * This macro never references `x` more than once, avoiding side effects.
 *
 * \param x the value to swap, in bigendian byte order.
 * \returns `x` in native byte order.
 *
 * \threadsafety It is safe to call this macro from any thread.
 *
 * \since This macro is available since SDL 3.1.3.
  }
    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap32BE(x : longint) : longint;    

{*
 * Swap a 64-bit value from bigendian to native byte order.
 *
 * If this is running on a bigendian system, `x` is returned unchanged.
 *
 * This macro never references `x` more than once, avoiding side effects.
 *
 * \param x the value to swap, in bigendian byte order.
 * \returns `x` in native byte order.
 *
 * \threadsafety It is safe to call this macro from any thread.
 *
 * \since This macro is available since SDL 3.1.3.
  }
    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap64BE(x : longint) : longint;    

{*
 * Swap a floating point value from bigendian to native byte order.
 *
 * If this is running on a bigendian system, `x` is returned unchanged.
 *
 * This macro never references `x` more than once, avoiding side effects.
 *
 * \param x the value to swap, in bigendian byte order.
 * \returns `x` in native byte order.
 *
 * \threadsafety It is safe to call this macro from any thread.
 *
 * \since This macro is available since SDL 3.1.3.
  }
    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_SwapFloatBE(x : longint) : longint;    

(*** was #elif ****){$else SDL_BYTEORDER == SDL_LIL_ENDIAN}
    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap16LE(x : longint) : longint;    

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap32LE(x : longint) : longint;    

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap64LE(x : longint) : longint;    

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_SwapFloatLE(x : longint) : longint;    

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap16BE(x : longint) : longint;    

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap32BE(x : longint) : longint;    

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap64BE(x : longint) : longint;    

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_SwapFloatBE(x : longint) : longint;    

{$else}
    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap16LE(x : longint) : longint;    

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap32LE(x : longint) : longint;    

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap64LE(x : longint) : longint;    

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_SwapFloatLE(x : longint) : longint;    

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap16BE(x : longint) : longint;    

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap32BE(x : longint) : longint;    

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap64BE(x : longint) : longint;    

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_SwapFloatBE(x : longint) : longint;    

{$endif}
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_endian_h_  }

implementation

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap32(x : longint) : longint;
    begin
      SDL_Swap32:=_byteswap_ulong(x);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap64(x : longint) : longint;
    begin
      SDL_Swap64:=_byteswap_uint64(x);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap32LE(x : longint) : longint;
    begin
      SDL_Swap32LE:=SwapOnlyIfNecessary(x);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap64LE(x : longint) : longint;
    begin
      SDL_Swap64LE:=SwapOnlyIfNecessary(x);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_SwapFloatLE(x : longint) : longint;
    begin
      SDL_SwapFloatLE:=SwapOnlyIfNecessary(x);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap16BE(x : longint) : longint;
    begin
      SDL_Swap16BE:=SwapOnlyIfNecessary(x);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap32BE(x : longint) : longint;
    begin
      SDL_Swap32BE:=SwapOnlyIfNecessary(x);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap64BE(x : longint) : longint;
    begin
      SDL_Swap64BE:=SwapOnlyIfNecessary(x);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_SwapFloatBE(x : longint) : longint;
    begin
      SDL_SwapFloatBE:=SwapOnlyIfNecessary(x);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap16LE(x : longint) : longint;
    begin
      SDL_Swap16LE:=x;
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap32LE(x : longint) : longint;
    begin
      SDL_Swap32LE:=x;
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap64LE(x : longint) : longint;
    begin
      SDL_Swap64LE:=x;
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_SwapFloatLE(x : longint) : longint;
    begin
      SDL_SwapFloatLE:=x;
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap16BE(x : longint) : longint;
    begin
      SDL_Swap16BE:=SDL_Swap16(x);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap32BE(x : longint) : longint;
    begin
      SDL_Swap32BE:=SDL_Swap32(x);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap64BE(x : longint) : longint;
    begin
      SDL_Swap64BE:=SDL_Swap64(x);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_SwapFloatBE(x : longint) : longint;
    begin
      SDL_SwapFloatBE:=SDL_SwapFloat(x);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap16LE(x : longint) : longint;
    begin
      SDL_Swap16LE:=SDL_Swap16(x);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap32LE(x : longint) : longint;
    begin
      SDL_Swap32LE:=SDL_Swap32(x);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap64LE(x : longint) : longint;
    begin
      SDL_Swap64LE:=SDL_Swap64(x);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_SwapFloatLE(x : longint) : longint;
    begin
      SDL_SwapFloatLE:=SDL_SwapFloat(x);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap16BE(x : longint) : longint;
    begin
      SDL_Swap16BE:=x;
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap32BE(x : longint) : longint;
    begin
      SDL_Swap32BE:=x;
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_Swap64BE(x : longint) : longint;
    begin
      SDL_Swap64BE:=x;
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
    function SDL_SwapFloatBE(x : longint) : longint;
    begin
      SDL_SwapFloatBE:=x;
    end;


end.
