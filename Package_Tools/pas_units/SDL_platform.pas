unit SDL_platform;

interface

uses
  SDL_stdinc;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}


function SDL_GetPlatform: pansichar; cdecl; external libSDL3;

implementation


end.
