
unit SDL_test_crc32;
interface

{
  Automatically converted by H2Pas 0.99.16 from SDL_test_crc32.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_test_crc32.h
}

Type
PCrcUint32 = ^TCrcUint32;
PCrcUint8 = ^TCrcUint8;

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
 *  CRC32 functions of SDL test framework.
 *
 *  This code is a part of the SDL test library, not the main SDL library.
  }
{

 Implements CRC32 calculations (default output is Perl String::CRC32 compatible).

 }
{$ifndef SDL_test_crc32_h_}
{$define SDL_test_crc32_h_}
{$include <SDL3/SDL_stdinc.h>}
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{ ------------ Definitions ---------  }
{ Definition shared by all CRC routines  }
{$ifndef CrcUint32}

const
  CrcUint32 = dword;  
{$endif}
{$ifndef CrcUint8}
  CrcUint8 = ansichar;  
{$endif}
{$ifdef ORIGINAL_METHOD}
{ AUTODIN II, Ethernet, & FDDI  }
  CRC32_POLY = $04c11db7;  
{$else}
{ Perl String::CRC32 compatible  }
  CRC32_POLY = $EDB88320;  
{$endif}
{
 * Data structure for CRC32 (checksum) computation
  }
{ CRC table  }
type
  PSDLTest_Crc32Context = ^TSDLTest_Crc32Context;
  TSDLTest_Crc32Context = record
      crc32_table : array[0..255] of TCrcUint32;
    end;
{ ---------- Function Prototypes -------------  }
{
 * Initialize the CRC context
 *
 * Note: The function initializes the crc table required for all crc calculations.
 *
 * \param crcContext        pointer to context variable
 *
 * \returns true on success or false on failure; call SDL_GetError()
 *          for more information.
 *
  }

function SDLTest_Crc32Init(crcContext:PSDLTest_Crc32Context):Tbool;cdecl;external;
{
 * calculate a crc32 from a data block
 *
 * \param crcContext         pointer to context variable
 * \param inBuf              input buffer to checksum
 * \param inLen              length of input buffer
 * \param crc32              pointer to Uint32 to store the final CRC into
 *
 * \returns true on success or false on failure; call SDL_GetError()
 *          for more information.
 *
  }
function SDLTest_Crc32Calc(crcContext:PSDLTest_Crc32Context; inBuf:PCrcUint8; inLen:TCrcUint32; crc32:PCrcUint32):Tbool;cdecl;external;
{ Same routine broken down into three steps  }
function SDLTest_Crc32CalcStart(crcContext:PSDLTest_Crc32Context; crc32:PCrcUint32):Tbool;cdecl;external;
function SDLTest_Crc32CalcEnd(crcContext:PSDLTest_Crc32Context; crc32:PCrcUint32):Tbool;cdecl;external;
function SDLTest_Crc32CalcBuffer(crcContext:PSDLTest_Crc32Context; inBuf:PCrcUint8; inLen:TCrcUint32; crc32:PCrcUint32):Tbool;cdecl;external;
{
 * clean up CRC context
 *
 * \param crcContext        pointer to context variable
 *
 * \returns true on success or false on failure; call SDL_GetError()
 *          for more information.
 *
 }
function SDLTest_Crc32Done(crcContext:PSDLTest_Crc32Context):Tbool;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_test_crc32_h_  }

implementation


end.
