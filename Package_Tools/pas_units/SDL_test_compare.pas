unit SDL_test_compare;

interface

uses
  ctypes, SDL3;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

function SDLTest_CompareSurfaces(surface: PSDL_Surface; referenceSurface: PSDL_Surface; allowable_error: longint): longint; cdecl; external;
function SDLTest_CompareMemory(actual: pointer; size_actual: Tsize_t; reference: pointer; size_reference: Tsize_t): longint; cdecl; external;

implementation


end.
