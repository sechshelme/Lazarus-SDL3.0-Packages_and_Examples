
unit SDL_test_assert;
interface

{
  Automatically converted by H2Pas 0.99.16 from SDL_test_assert.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_test_assert.h
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
 *  Assertion functions of SDL test framework.
 *
 *  This code is a part of the SDL test library, not the main SDL library.
  }
{
 *
 * Assert API for test code and test cases
 *
  }
{$ifndef SDL_test_assert_h_}
{$define SDL_test_assert_h_}
{$include <SDL3/SDL_stdinc.h>}
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{ Fails the assert.  }

const
  ASSERT_FAIL = &;  
{ Passes the assert.  }
  ASSERT_PASS = 1;  
{
 * Assert that logs and break execution flow on failures.
 *
 * \param assertCondition Evaluated condition or variable to assert; fail (==0) or pass (!=0).
 * \param assertDescription Message to log with the assert describing it.
  }
(* Const before declarator ignored *)

procedure SDLTest_Assert(assertCondition:longint; assertDescription:Pansichar; args:array of const);cdecl;external;
procedure SDLTest_Assert(assertCondition:longint; assertDescription:Pansichar);cdecl;external;
{
 * Assert for test cases that logs but does not break execution flow on failures. Updates assertion counters.
 *
 * \param assertCondition Evaluated condition or variable to assert; fail (==0) or pass (!=0).
 * \param assertDescription Message to log with the assert describing it.
 *
 * \returns the assertCondition so it can be used to externally to break execution flow if desired.
  }
(* Const before declarator ignored *)
function SDLTest_AssertCheck(assertCondition:longint; assertDescription:Pansichar; args:array of const):longint;cdecl;external;
function SDLTest_AssertCheck(assertCondition:longint; assertDescription:Pansichar):longint;cdecl;external;
{
 * Explicitly pass without checking an assertion condition. Updates assertion counter.
 *
 * \param assertDescription Message to log with the assert describing it.
  }
(* Const before declarator ignored *)
procedure SDLTest_AssertPass(assertDescription:Pansichar; args:array of const);cdecl;external;
procedure SDLTest_AssertPass(assertDescription:Pansichar);cdecl;external;
{
 * Resets the assert summary counters to zero.
  }
procedure SDLTest_ResetAssertSummary;cdecl;external;
{
 * Logs summary of all assertions (total, pass, fail) since last reset as INFO or ERROR.
  }
procedure SDLTest_LogAssertSummary;cdecl;external;
{
 * Converts the current assert summary state to a test result.
 *
 * \returns TEST_RESULT_PASSED, TEST_RESULT_FAILED, or TEST_RESULT_NO_ASSERT
  }
function SDLTest_AssertSummaryToTestResult:longint;cdecl;external;
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_test_assert_h_  }

implementation


end.
