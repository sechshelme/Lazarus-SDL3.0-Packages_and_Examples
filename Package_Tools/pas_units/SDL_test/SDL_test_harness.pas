unit SDL_test_harness;

interface

uses
  ctypes, SDL3, SDL_test_common;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

const
  TEST_ENABLED = 1;
  TEST_DISABLED = 0;
  TEST_ABORTED = -(1);
  TEST_STARTED = 0;
  TEST_COMPLETED = 1;
  TEST_SKIPPED = 2;
  TEST_RESULT_PASSED = 0;
  TEST_RESULT_FAILED = 1;
  TEST_RESULT_NO_ASSERT = 2;
  TEST_RESULT_SKIPPED = 3;
  TEST_RESULT_SETUP_FAILURE = 4;

type
  TSDLTest_TestSuiteRunner = record
  end;
  PSDLTest_TestSuiteRunner = ^TSDLTest_TestSuiteRunner;

  TSDLTest_TestCaseSetUpFp = procedure(arg: Ppointer); cdecl;
  TSDLTest_TestCaseFp = function(arg: pointer): longint; cdecl;
  TSDLTest_TestCaseTearDownFp = procedure(arg: pointer); cdecl;

  PSDLTest_TestCaseReference = ^TSDLTest_TestCaseReference;

  TSDLTest_TestCaseReference = record
    testCase: TSDLTest_TestCaseFp;
    Name: pansichar;
    description: pansichar;
    Enabled: longint;
  end;

  PSDLTest_TestSuiteReference = ^TSDLTest_TestSuiteReference;
  PPSDLTest_TestSuiteReference = ^PSDLTest_TestSuiteReference;

  TSDLTest_TestSuiteReference = record
    Name: pansichar;
    testSetUp: TSDLTest_TestCaseSetUpFp;
    testCases: ^PSDLTest_TestCaseReference;
    testTearDown: TSDLTest_TestCaseTearDownFp;
  end;

function SDLTest_GenerateRunSeed(buffer: pansichar; length: longint): pansichar; cdecl; external;
function SDLTest_CreateTestSuiteRunner(state: PSDLTest_CommonState; testSuites: PPSDLTest_TestSuiteReference): PSDLTest_TestSuiteRunner; cdecl; external;
procedure SDLTest_DestroyTestSuiteRunner(runner: PSDLTest_TestSuiteRunner); cdecl; external;
function SDLTest_ExecuteTestSuiteRunner(runner: PSDLTest_TestSuiteRunner): longint; cdecl; external;

implementation


end.
