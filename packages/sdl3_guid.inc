/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_GUID = ^TSDL_GUID;

  TSDL_GUID = record
    Data: array[0..15] of uint8;
  end;

function SDL_GUIDToString(guid: TSDL_GUID; pszGUID: PChar; cbGUID: longint): longint; cdecl; external;
function SDL_GUIDFromString(pchGUID: PChar): TSDL_GUID; cdecl; external;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}
