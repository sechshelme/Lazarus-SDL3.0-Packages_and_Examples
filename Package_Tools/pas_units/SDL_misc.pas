unit SDL_misc;

interface

uses
  SDL_stdinc;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

function SDL_OpenURL(url: pansichar): Tbool; cdecl; external libSDL3;

implementation


end.
