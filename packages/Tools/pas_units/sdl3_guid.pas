unit SDL3_guid;

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

type
  PSDL_GUID = ^TSDL_GUID;

  TSDL_GUID = record
    Data: array[0..15] of uint8;
  end;

function SDL_GUIDToString(guid: TSDL_GUID; pszGUID: PChar; cbGUID: longint): longint; cdecl; external sdl3_lib;
function SDL_GUIDFromString(pchGUID: PChar): TSDL_GUID; cdecl; external sdl3_lib;

implementation

end.
