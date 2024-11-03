
unit SDL_test_harness;
interface

{
  Automatically converted by H2Pas 0.99.16 from SDL_test_harness.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_test_harness.h
}

Type
PSDLTest_CommonState = ^TSDLTest_CommonState;
PSDLTest_TestSuiteRunner = ^TSDLTest_TestSuiteRunner;

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
 *  \file SDL_test_harness.h
 *
 *  Test suite related functions of SDL test framework.
 *
 *  This code is a part of the SDL test library, not the main SDL library.
  }
{
  Defines types for test case definitions and the test execution harness API.

  Based on original GSOC code by Markus Kauppila <markus.kauppila@gmail.com>
 }
{$ifndef SDL_test_h_arness_h}
{$define SDL_test_h_arness_h}
{$include <SDL3/SDL_stdinc.h>}
{$include <SDL3/SDL_test_common.h> /* SDLTest_CommonState */}
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{ ! Definitions for test case structures  }

const
  TEST_ENABLED = 1;  
  TEST_DISABLED = &;  
{ ! Definition of all the possible test return values of the test case method  }
  TEST_ABORTED = -(1);  
  TEST_STARTED = &;  
  TEST_COMPLETED = 1;  
  TEST_SKIPPED = 2;  
{ ! Definition of all the possible test results for the harness  }
  TEST_RESULT_PASSED = &;  
  TEST_RESULT_FAILED = 1;  
  TEST_RESULT_NO_ASSERT = 2;  
  TEST_RESULT_SKIPPED = 3;  
  TEST_RESULT_SETUP_FAILURE = 4;  
{ !< Function pointer to a test case setup function (run before every test)  }
type

  TSDLTest_TestCaseSetUpFp = procedure (arg:Ppointer);cdecl;
{ !< Function pointer to a test case function  }

  TSDLTest_TestCaseFp = function (arg:pointer):longint;cdecl;
{ !< Function pointer to a test case teardown function (run after every test)  }

  TSDLTest_TestCaseTearDownFp = procedure (arg:pointer);cdecl;
{
 * Holds information about a single test case.
  }
{ !< Func2Stress  }
{ !< Short name (or function name) "Func2Stress"  }
(* Const before declarator ignored *)
{ !< Long name or full description "This test pushes func2() to the limit."  }
(* Const before declarator ignored *)
{ !< Set to TEST_ENABLED or TEST_DISABLED (test won't be run)  }

  PSDLTest_TestCaseReference = ^TSDLTest_TestCaseReference;
  TSDLTest_TestCaseReference = record
      testCase : TSDLTest_TestCaseFp;
      name : Pansichar;
      description : Pansichar;
      enabled : longint;
    end;
{
 * Holds information about a test suite (multiple test cases).
  }
{ !< "PlatformSuite"  }
(* Const before declarator ignored *)
{ !< The function that is run before each test. NULL skips.  }
{ !< The test cases that are run as part of the suite. Last item should be NULL.  }
(* Const before declarator ignored *)
{ !< The function that is run after each test. NULL skips.  }

  PSDLTest_TestSuiteReference = ^TSDLTest_TestSuiteReference;
  TSDLTest_TestSuiteReference = record
      name : Pansichar;
      testSetUp : TSDLTest_TestCaseSetUpFp;
      testCases : ^PSDLTest_TestCaseReference;
      testTearDown : TSDLTest_TestCaseTearDownFp;
    end;
{
 * Generates a random run seed string for the harness. The generated seed
 * will contain alphanumeric characters (0-9A-Z).
 *
 * \param buffer Buffer in which to generate the random seed. Must have a capacity of at least length + 1 characters.
 * \param length Number of alphanumeric characters to write to buffer, must be >0
 *
 * \returns A null-terminated seed string and equal to the in put buffer on success, NULL on failure
  }

function SDLTest_GenerateRunSeed(buffer:Pansichar; length:longint):Pansichar;cdecl;external;
{
 * Holds information about the execution of test suites.
 *  }
type
{
 * Create a new test suite runner, that will execute the given test suites.
 * It will register the harness cli arguments to the common SDL state.
 *
 * \param state Common SDL state on which to register CLI arguments.
 * \param testSuites NULL-terminated test suites containing test cases.
 *
 * \returns the test run result: 0 when all tests passed, 1 if any tests failed.
  }

function SDLTest_CreateTestSuiteRunner(state:PSDLTest_CommonState; testSuites:PPSDLTest_TestSuiteReference):PSDLTest_TestSuiteRunner;cdecl;external;
{
 * Destroy a test suite runner.
 * It will unregister the harness cli arguments to the common SDL state.
 *
 * \param runner The runner that should be destroyed.
  }
procedure SDLTest_DestroyTestSuiteRunner(runner:PSDLTest_TestSuiteRunner);cdecl;external;
{
 * Execute a test suite, using the configured run seed, execution key, filter, etc.
 *
 * \param runner The runner that should be executed.
 *
 * \returns the test run result: 0 when all tests passed, 1 if any tests failed.
  }
function SDLTest_ExecuteTestSuiteRunner(runner:PSDLTest_TestSuiteRunner):longint;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_test_h_arness_h  }

implementation


end.
