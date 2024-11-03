
unit SDL_test_md5;
interface

{
  Automatically converted by H2Pas 0.99.16 from SDL_test_md5.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_test_md5.h
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
 *  \file SDL_test_md5.h
 *
 *  MD5 related functions of SDL test framework.
 *
 *  This code is a part of the SDL test library, not the main SDL library.
  }
{
 ***********************************************************************
 ** Header file for implementation of MD5                             **
 ** RSA Data Security, Inc. MD5 Message-Digest Algorithm              **
 ** Created: 2/17/90 RLR                                              **
 ** Revised: 12/27/90 SRD,AJ,BSK,JT Reference C version               **
 ** Revised (for MD5): RLR 4/27/91                                    **
 **   -- G modified to have y&~z instead of y&z                       **
 **   -- FF, GG, HH modified to add in last register done             **
 **   -- Access pattern: round 2 works mod 5, round 3 works mod 3     **
 **   -- distinct additive constant for each step                     **
 **   -- round 4 added, working mod 7                                 **
 ***********************************************************************
 }
{
 ***********************************************************************
 **  Message-digest routines:                                         **
 **  To form the message digest for a message M                       **
 **    (1) Initialize a context buffer mdContext using MD5Init        **
 **    (2) Call MD5Update on mdContext and M                          **
 **    (3) Call MD5Final on mdContext                                 **
 **  The message digest is now in mdContext->digest[0...15]           **
 ***********************************************************************
 }
{$ifndef SDL_test_md5_h_}
{$define SDL_test_md5_h_}
{$include <SDL3/SDL_stdinc.h>}
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{ ------------ Definitions ---------  }
{ typedef a 32-bit type  }
type
  PMD5UINT4 = ^TMD5UINT4;
  TMD5UINT4 = TUint32;
{ Data structure for MD5 (Message-Digest) computation  }
{ number of _bits_ handled mod 2^64  }
{ scratch buffer  }
{ input buffer  }
{ actual digest after Md5Final call  }

  PSDLTest_Md5Context = ^TSDLTest_Md5Context;
  TSDLTest_Md5Context = record
      i : array[0..1] of TMD5UINT4;
      buf : array[0..3] of TMD5UINT4;
      in : array[0..63] of ansichar;
      digest : array[0..15] of ansichar;
    end;
{ ---------- Function Prototypes -------------  }
{*
 * initialize the context
 *
 * \param  mdContext        pointer to context variable
 *
 * Note: The function initializes the message-digest context
 *       mdContext. Call before each new use of the context -
 *       all fields are set to zero.
  }

procedure SDLTest_Md5Init(mdContext:PSDLTest_Md5Context);cdecl;external;
{*
 * update digest from variable length data
 *
 * \param  mdContext       pointer to context variable
 * \param  inBuf           pointer to data array/string
 * \param  inLen           length of data array/string
 *
 * Note: The function updates the message-digest context to account
 *       for the presence of each of the characters inBuf[0..inLen-1]
 *       in the message whose digest is being computed.
  }
procedure SDLTest_Md5Update(mdContext:PSDLTest_Md5Context; inBuf:Pansichar; inLen:dword);cdecl;external;
{*
 * complete digest computation
 *
 * \param mdContext     pointer to context variable
 *
 * Note: The function terminates the message-digest computation and
 *       ends with the desired message digest in mdContext.digest[0..15].
 *       Always call before using the digest[] variable.
  }
procedure SDLTest_Md5Final(mdContext:PSDLTest_Md5Context);cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_test_md5_h_  }

implementation


end.
