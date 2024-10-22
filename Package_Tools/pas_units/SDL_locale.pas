unit SDL_locale;

interface

uses
  SDL_stdinc;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  TSDL_Locale = record
    language: pansichar;
    country: pansichar;
  end;
  PSDL_Locale = ^TSDL_Locale;
  PPSDL_Locale = ^PSDL_Locale;

function SDL_GetPreferredLocales(Count: Plongint): PPSDL_Locale; cdecl; external libSDL3;

implementation


end.
