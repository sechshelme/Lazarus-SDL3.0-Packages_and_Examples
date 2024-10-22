/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 22-10-2024   15:23                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

function SDL_LoadObject(sofile: PChar): pointer; cdecl; external sdl3_lib;
function SDL_LoadFunction(handle: pointer; Name: PChar): TSDL_FunctionPointer; cdecl; external sdl3_lib;
procedure SDL_UnloadObject(handle: pointer); cdecl; external sdl3_lib;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}
