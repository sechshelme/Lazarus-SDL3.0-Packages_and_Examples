
unit SDL_test_fuzzer;
interface

{
  Automatically converted by H2Pas 0.99.16 from SDL_test_fuzzer.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_test_fuzzer.h
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
 *  Fuzzer functions of SDL test framework.
 *
 *  This code is a part of the SDL test library, not the main SDL library.
  }
{

  Data generators for fuzzing test data in a reproducible way.

 }
{$ifndef SDL_test_fuzzer_h_}
{$define SDL_test_fuzzer_h_}
{$include <SDL3/SDL_stdinc.h>}
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{
  Based on GSOC code by Markus Kauppila <markus.kauppila@gmail.com>
 }
{*
 * Note: The fuzzer implementation uses a static instance of random context
 * internally which makes it thread-UNsafe.
  }
{*
 * Initializes the fuzzer for a test
 *
 * \param execKey Execution "Key" that initializes the random number generator uniquely for the test.
 *
  }

procedure SDLTest_FuzzerInit(execKey:TUint64);cdecl;external;
{*
 * Returns a random Uint8
 *
 * \returns a generated integer
  }
function SDLTest_RandomUint8:TUint8;cdecl;external;
{*
 * Returns a random Sint8
 *
 * \returns a generated signed integer
  }
function SDLTest_RandomSint8:TSint8;cdecl;external;
{*
 * Returns a random Uint16
 *
 * \returns a generated integer
  }
function SDLTest_RandomUint16:TUint16;cdecl;external;
{*
 * Returns a random Sint16
 *
 * \returns a generated signed integer
  }
function SDLTest_RandomSint16:TSint16;cdecl;external;
{*
 * Returns a random integer
 *
 * \returns a generated integer
  }
function SDLTest_RandomSint32:TSint32;cdecl;external;
{*
 * Returns a random positive integer
 *
 * \returns a generated integer
  }
function SDLTest_RandomUint32:TUint32;cdecl;external;
{*
 * Returns random Uint64.
 *
 * \returns a generated integer
  }
function SDLTest_RandomUint64:TUint64;cdecl;external;
{*
 * Returns random Sint64.
 *
 * \returns a generated signed integer
  }
function SDLTest_RandomSint64:TSint64;cdecl;external;
{*
 * \returns a random float in range [0.0 - 1.0]
  }
function SDLTest_RandomUnitFloat:single;cdecl;external;
{*
 * \returns a random double in range [0.0 - 1.0]
  }
function SDLTest_RandomUnitDouble:Tdouble;cdecl;external;
{*
 * \returns a random float.
 *
  }
function SDLTest_RandomFloat:single;cdecl;external;
{*
 * \returns a random double.
 *
  }
function SDLTest_RandomDouble:Tdouble;cdecl;external;
{*
 * Returns a random boundary value for Uint8 within the given boundaries.
 * Boundaries are inclusive, see the usage examples below. If validDomain
 * is true, the function will only return valid boundaries, otherwise non-valid
 * boundaries are also possible.
 * If boundary1 > boundary2, the values are swapped
 *
 * Usage examples:
 * RandomUint8BoundaryValue(10, 20, true) returns 10, 11, 19 or 20
 * RandomUint8BoundaryValue(1, 20, false) returns 0 or 21
 * RandomUint8BoundaryValue(0, 99, false) returns 100
 * RandomUint8BoundaryValue(0, 255, false) returns 0 (error set)
 *
 * \param boundary1 Lower boundary limit
 * \param boundary2 Upper boundary limit
 * \param validDomain Should the generated boundary be valid (=within the bounds) or not?
 *
 * \returns a random boundary value for the given range and domain or 0 with error set
  }
function SDLTest_RandomUint8BoundaryValue(boundary1:TUint8; boundary2:TUint8; validDomain:Tbool):TUint8;cdecl;external;
{*
 * Returns a random boundary value for Uint16 within the given boundaries.
 * Boundaries are inclusive, see the usage examples below. If validDomain
 * is true, the function will only return valid boundaries, otherwise non-valid
 * boundaries are also possible.
 * If boundary1 > boundary2, the values are swapped
 *
 * Usage examples:
 * RandomUint16BoundaryValue(10, 20, true) returns 10, 11, 19 or 20
 * RandomUint16BoundaryValue(1, 20, false) returns 0 or 21
 * RandomUint16BoundaryValue(0, 99, false) returns 100
 * RandomUint16BoundaryValue(0, 0xFFFF, false) returns 0 (error set)
 *
 * \param boundary1 Lower boundary limit
 * \param boundary2 Upper boundary limit
 * \param validDomain Should the generated boundary be valid (=within the bounds) or not?
 *
 * \returns a random boundary value for the given range and domain or 0 with error set
  }
function SDLTest_RandomUint16BoundaryValue(boundary1:TUint16; boundary2:TUint16; validDomain:Tbool):TUint16;cdecl;external;
{*
 * Returns a random boundary value for Uint32 within the given boundaries.
 * Boundaries are inclusive, see the usage examples below. If validDomain
 * is true, the function will only return valid boundaries, otherwise non-valid
 * boundaries are also possible.
 * If boundary1 > boundary2, the values are swapped
 *
 * Usage examples:
 * RandomUint32BoundaryValue(10, 20, true) returns 10, 11, 19 or 20
 * RandomUint32BoundaryValue(1, 20, false) returns 0 or 21
 * RandomUint32BoundaryValue(0, 99, false) returns 100
 * RandomUint32BoundaryValue(0, 0xFFFFFFFF, false) returns 0 (with error set)
 *
 * \param boundary1 Lower boundary limit
 * \param boundary2 Upper boundary limit
 * \param validDomain Should the generated boundary be valid (=within the bounds) or not?
 *
 * \returns a random boundary value for the given range and domain or 0 with error set
  }
function SDLTest_RandomUint32BoundaryValue(boundary1:TUint32; boundary2:TUint32; validDomain:Tbool):TUint32;cdecl;external;
{*
 * Returns a random boundary value for Uint64 within the given boundaries.
 * Boundaries are inclusive, see the usage examples below. If validDomain
 * is true, the function will only return valid boundaries, otherwise non-valid
 * boundaries are also possible.
 * If boundary1 > boundary2, the values are swapped
 *
 * Usage examples:
 * RandomUint64BoundaryValue(10, 20, true) returns 10, 11, 19 or 20
 * RandomUint64BoundaryValue(1, 20, false) returns 0 or 21
 * RandomUint64BoundaryValue(0, 99, false) returns 100
 * RandomUint64BoundaryValue(0, 0xFFFFFFFFFFFFFFFF, false) returns 0 (with error set)
 *
 * \param boundary1 Lower boundary limit
 * \param boundary2 Upper boundary limit
 * \param validDomain Should the generated boundary be valid (=within the bounds) or not?
 *
 * \returns a random boundary value for the given range and domain or 0 with error set
  }
function SDLTest_RandomUint64BoundaryValue(boundary1:TUint64; boundary2:TUint64; validDomain:Tbool):TUint64;cdecl;external;
{*
 * Returns a random boundary value for Sint8 within the given boundaries.
 * Boundaries are inclusive, see the usage examples below. If validDomain
 * is true, the function will only return valid boundaries, otherwise non-valid
 * boundaries are also possible.
 * If boundary1 > boundary2, the values are swapped
 *
 * Usage examples:
 * RandomSint8BoundaryValue(-10, 20, true) returns -11, -10, 19 or 20
 * RandomSint8BoundaryValue(-100, -10, false) returns -101 or -9
 * RandomSint8BoundaryValue(SINT8_MIN, 99, false) returns 100
 * RandomSint8BoundaryValue(SINT8_MIN, SINT8_MAX, false) returns SINT8_MIN (== error value) with error set
 *
 * \param boundary1 Lower boundary limit
 * \param boundary2 Upper boundary limit
 * \param validDomain Should the generated boundary be valid (=within the bounds) or not?
 *
 * \returns a random boundary value for the given range and domain or SINT8_MIN with error set
  }
function SDLTest_RandomSint8BoundaryValue(boundary1:TSint8; boundary2:TSint8; validDomain:Tbool):TSint8;cdecl;external;
{*
 * Returns a random boundary value for Sint16 within the given boundaries.
 * Boundaries are inclusive, see the usage examples below. If validDomain
 * is true, the function will only return valid boundaries, otherwise non-valid
 * boundaries are also possible.
 * If boundary1 > boundary2, the values are swapped
 *
 * Usage examples:
 * RandomSint16BoundaryValue(-10, 20, true) returns -11, -10, 19 or 20
 * RandomSint16BoundaryValue(-100, -10, false) returns -101 or -9
 * RandomSint16BoundaryValue(SINT16_MIN, 99, false) returns 100
 * RandomSint16BoundaryValue(SINT16_MIN, SINT16_MAX, false) returns SINT16_MIN (== error value) with error set
 *
 * \param boundary1 Lower boundary limit
 * \param boundary2 Upper boundary limit
 * \param validDomain Should the generated boundary be valid (=within the bounds) or not?
 *
 * \returns a random boundary value for the given range and domain or SINT16_MIN with error set
  }
function SDLTest_RandomSint16BoundaryValue(boundary1:TSint16; boundary2:TSint16; validDomain:Tbool):TSint16;cdecl;external;
{*
 * Returns a random boundary value for Sint32 within the given boundaries.
 * Boundaries are inclusive, see the usage examples below. If validDomain
 * is true, the function will only return valid boundaries, otherwise non-valid
 * boundaries are also possible.
 * If boundary1 > boundary2, the values are swapped
 *
 * Usage examples:
 * RandomSint32BoundaryValue(-10, 20, true) returns -11, -10, 19 or 20
 * RandomSint32BoundaryValue(-100, -10, false) returns -101 or -9
 * RandomSint32BoundaryValue(SINT32_MIN, 99, false) returns 100
 * RandomSint32BoundaryValue(SINT32_MIN, SINT32_MAX, false) returns SINT32_MIN (== error value)
 *
 * \param boundary1 Lower boundary limit
 * \param boundary2 Upper boundary limit
 * \param validDomain Should the generated boundary be valid (=within the bounds) or not?
 *
 * \returns a random boundary value for the given range and domain or SINT32_MIN with error set
  }
function SDLTest_RandomSint32BoundaryValue(boundary1:TSint32; boundary2:TSint32; validDomain:Tbool):TSint32;cdecl;external;
{*
 * Returns a random boundary value for Sint64 within the given boundaries.
 * Boundaries are inclusive, see the usage examples below. If validDomain
 * is true, the function will only return valid boundaries, otherwise non-valid
 * boundaries are also possible.
 * If boundary1 > boundary2, the values are swapped
 *
 * Usage examples:
 * RandomSint64BoundaryValue(-10, 20, true) returns -11, -10, 19 or 20
 * RandomSint64BoundaryValue(-100, -10, false) returns -101 or -9
 * RandomSint64BoundaryValue(SINT64_MIN, 99, false) returns 100
 * RandomSint64BoundaryValue(SINT64_MIN, SINT64_MAX, false) returns SINT64_MIN (== error value) and error set
 *
 * \param boundary1 Lower boundary limit
 * \param boundary2 Upper boundary limit
 * \param validDomain Should the generated boundary be valid (=within the bounds) or not?
 *
 * \returns a random boundary value for the given range and domain or SINT64_MIN with error set
  }
function SDLTest_RandomSint64BoundaryValue(boundary1:TSint64; boundary2:TSint64; validDomain:Tbool):TSint64;cdecl;external;
{*
 * Returns integer in range [min, max] (inclusive).
 * Min and max values can be negative values.
 * If Max in smaller than min, then the values are swapped.
 * Min and max are the same value, that value will be returned.
 *
 * \param min Minimum inclusive value of returned random number
 * \param max Maximum inclusive value of returned random number
 *
 * \returns a generated random integer in range
  }
function SDLTest_RandomIntegerInRange(min:TSint32; max:TSint32):TSint32;cdecl;external;
{*
 * Generates random null-terminated string. The minimum length for
 * the string is 1 character, maximum length for the string is 255
 * characters and it can contain ASCII characters from 32 to 126.
 *
 * Note: Returned string needs to be deallocated.
 *
 * \returns a newly allocated random string; or NULL if length was invalid or string could not be allocated.
  }
function SDLTest_RandomAsciiString:Pansichar;cdecl;external;
{*
 * Generates random null-terminated string. The maximum length for
 * the string is defined by the maxLength parameter.
 * String can contain ASCII characters from 32 to 126.
 *
 * Note: Returned string needs to be deallocated.
 *
 * \param maxLength The maximum length of the generated string.
 *
 * \returns a newly allocated random string; or NULL if maxLength was invalid or string could not be allocated.
  }
function SDLTest_RandomAsciiStringWithMaximumLength(maxLength:longint):Pansichar;cdecl;external;
{*
 * Generates random null-terminated string. The length for
 * the string is defined by the size parameter.
 * String can contain ASCII characters from 32 to 126.
 *
 * Note: Returned string needs to be deallocated.
 *
 * \param size The length of the generated string
 *
 * \returns a newly allocated random string; or NULL if size was invalid or string could not be allocated.
  }
function SDLTest_RandomAsciiStringOfSize(size:longint):Pansichar;cdecl;external;
{*
 * Get the invocation count for the fuzzer since last ...FuzzerInit.
 *
 * \returns the invocation count.
  }
function SDLTest_GetFuzzerInvocationCount:longint;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_test_fuzzer_h_  }

implementation


end.
