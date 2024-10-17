unit SDL_loadso;

interface

uses
  ctypes, SDL_stdinc;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  TSDL_SharedObject = record
  end;
  PSDL_SharedObject = ^TSDL_SharedObject;

function SDL_LoadObject(sofile: pansichar): PSDL_SharedObject; cdecl; external libSDL3;
function SDL_LoadFunction(handle: PSDL_SharedObject; Name: pansichar): TSDL_FunctionPointer; cdecl; external libSDL3;
procedure SDL_UnloadObject(handle: PSDL_SharedObject); cdecl; external libSDL3;

implementation


end.
