unit SDL_test_log;

interface

uses
  ctypes, SDL3;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

procedure SDLTest_Log(fmt: pansichar); varargs cdecl; external;
procedure SDLTest_LogEscapedString(prefix: pansichar; buffer: pointer; size: Tsize_t); cdecl; external;
procedure SDLTest_LogError(fmt: pansichar); varargs cdecl; external;

implementation


end.
