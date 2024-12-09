
{$IFDEF read_interface}

type
  PSDL_Locale = ^TSDL_Locale;

  TSDL_Locale = record
    language: PChar;
    country: PChar;
  end;

function SDL_GetPreferredLocales: PSDL_Locale; cdecl; external sdl3_lib;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}
