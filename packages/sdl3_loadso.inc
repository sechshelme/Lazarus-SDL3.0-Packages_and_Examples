/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

function SDL_LoadObject(sofile: PChar): pointer; cdecl; external;
function SDL_LoadFunction(handle: pointer; Name: PChar): TSDL_FunctionPointer; cdecl; external;
procedure SDL_UnloadObject(handle: pointer); cdecl; external;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}
