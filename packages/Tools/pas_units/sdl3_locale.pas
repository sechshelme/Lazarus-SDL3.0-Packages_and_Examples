unit SDL3_locale;

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

type
  PSDL_Locale = ^TSDL_Locale;

  TSDL_Locale = record
    language: PChar;
    country: PChar;
  end;

function SDL_GetPreferredLocales: PSDL_Locale; cdecl; external;

implementation

end.
