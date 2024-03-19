unit SDL3_platform;

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

function SDL_GetPlatform: PChar; cdecl; external;

implementation

end.
