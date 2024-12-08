unit SDL_test_memory;

interface

uses
  ctypes, SDL3;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}


procedure SDLTest_TrackAllocations; cdecl; external;
procedure SDLTest_RandFillAllocations; cdecl; external;
procedure SDLTest_LogAllocations; cdecl; external;

implementation


end.
