unit SDL3_misc;

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

function SDL_OpenURL(url: PChar): longint; cdecl; external;

implementation

end.
