unit SDL3_loadso;

interface

uses
  SDL3_stdinc;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

function SDL_LoadObject(sofile: PChar): pointer; cdecl; external sdl3_lib;
function SDL_LoadFunction(handle: pointer; Name: PChar): TSDL_FunctionPointer; cdecl; external sdl3_lib;
procedure SDL_UnloadObject(handle: pointer); cdecl; external sdl3_lib;

implementation

end.
