/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 19-06-2024   19:57                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_GUID = ^TSDL_GUID;

  TSDL_GUID = record
    Data: array[0..15] of uint8;
  end;

function SDL_GUIDToString(guid: TSDL_GUID; pszGUID: PChar; cbGUID: longint): longint; cdecl; external sdl3_lib;
function SDL_GUIDFromString(pchGUID: PChar): TSDL_GUID; cdecl; external sdl3_lib;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}
