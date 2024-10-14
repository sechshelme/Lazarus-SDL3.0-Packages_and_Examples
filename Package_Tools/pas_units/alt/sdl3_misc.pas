unit SDL3_misc;

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

function SDL_OpenURL(url: PChar): longint; cdecl; external sdl3_lib;

implementation

end.
