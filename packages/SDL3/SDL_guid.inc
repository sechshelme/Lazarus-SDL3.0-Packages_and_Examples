/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   17:58                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  TSDL_GUID = record
    Data: array[0..15] of TUint8;
  end;
  PSDL_GUID = ^TSDL_GUID;

procedure SDL_GUIDToString(guid: TSDL_GUID; pszGUID: pansichar; cbGUID: longint); cdecl; external libSDL3;
function SDL_StringToGUID(pchGUID: pansichar): TSDL_GUID; cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}
