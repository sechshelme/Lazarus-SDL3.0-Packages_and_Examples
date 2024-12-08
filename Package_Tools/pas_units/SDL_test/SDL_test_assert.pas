unit SDL_test_assert;

interface

uses
  ctypes, SDL3;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

const
  ASSERT_FAIL = 0;
  ASSERT_PASS = 1;

procedure SDLTest_Assert(assertCondition: longint; assertDescription: pansichar; args: array of const); cdecl; external;
procedure SDLTest_Assert(assertCondition: longint; assertDescription: pansichar); cdecl; external;
function SDLTest_AssertCheck(assertCondition: longint; assertDescription: pansichar; args: array of const): longint; cdecl; external;
procedure SDLTest_AssertPass(assertDescription: pansichar; args: array of const); cdecl; external;
procedure SDLTest_AssertPass(assertDescription: pansichar); cdecl; external;
procedure SDLTest_ResetAssertSummary; cdecl; external;
procedure SDLTest_LogAssertSummary; cdecl; external;
function SDLTest_AssertSummaryToTestResult: longint; cdecl; external;

implementation


end.
