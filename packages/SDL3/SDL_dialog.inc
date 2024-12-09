
{$IFDEF read_interface}

type
  TSDL_DialogFileFilter = record
    Name: pansichar;
    pattern: pansichar;
  end;
  PSDL_DialogFileFilter = ^TSDL_DialogFileFilter;

  TSDL_DialogFileCallback = procedure(userdata: pointer; filelist: PPansichar; filter: longint); cdecl;

procedure SDL_ShowOpenFileDialog(callback: TSDL_DialogFileCallback; userdata: pointer; window: PSDL_Window; filters: PSDL_DialogFileFilter; nfilters: longint;
  default_location: pansichar; allow_many: Tbool); cdecl; external libSDL3;
procedure SDL_ShowSaveFileDialog(callback: TSDL_DialogFileCallback; userdata: pointer; window: PSDL_Window; filters: PSDL_DialogFileFilter; nfilters: longint;
  default_location: pansichar); cdecl; external libSDL3;
procedure SDL_ShowOpenFolderDialog(callback: TSDL_DialogFileCallback; userdata: pointer; window: PSDL_Window; default_location: pansichar; allow_many: Tbool); cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}
