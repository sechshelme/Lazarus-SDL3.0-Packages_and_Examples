unit SDL_revision;

interface

uses
  ctypes, SDL_stdinc;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

const
  SDL_REVISION_ = 'Some arbitrary string decided at SDL build time';

implementation


end.
