/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_DialogFileFilter = ^TSDL_DialogFileFilter;

  TSDL_DialogFileFilter = record
    Name: PChar;
    pattern: PChar;
  end;

  TSDL_DialogFileCallback = procedure(userdata: pointer; filelist: PPchar; filter: longint); cdecl;

procedure SDL_ShowOpenFileDialog(callback: TSDL_DialogFileCallback; userdata: pointer; window: PSDL_Window; filters: PSDL_DialogFileFilter; default_location: PChar;
  allow_many: TSDL_bool); cdecl; external;
procedure SDL_ShowSaveFileDialog(callback: TSDL_DialogFileCallback; userdata: pointer; window: PSDL_Window; filters: PSDL_DialogFileFilter; default_location: PChar); cdecl; external;
procedure SDL_ShowOpenFolderDialog(callback: TSDL_DialogFileCallback; userdata: pointer; window: PSDL_Window; default_location: PChar; allow_many: TSDL_bool); cdecl; external;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}
