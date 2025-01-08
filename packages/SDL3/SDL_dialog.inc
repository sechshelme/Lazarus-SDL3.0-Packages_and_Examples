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

type
  PSDL_FileDialogType = ^TSDL_FileDialogType;
  TSDL_FileDialogType = longint;

const
  SDL_FILEDIALOG_OPENFILE = 0;
  SDL_FILEDIALOG_SAVEFILE = 1;
  SDL_FILEDIALOG_OPENFOLDER = 2;

procedure SDL_ShowFileDialogWithProperties(_type: TSDL_FileDialogType; callback: TSDL_DialogFileCallback; userdata: pointer; props: TSDL_PropertiesID); cdecl; external libSDL3;

const
  SDL_PROP_FILE_DIALOG_FILTERS_POINTER = 'SDL.filedialog.filters';
  SDL_PROP_FILE_DIALOG_NFILTERS_NUMBER = 'SDL.filedialog.nfilters';
  SDL_PROP_FILE_DIALOG_WINDOW_POINTER = 'SDL.filedialog.window';
  SDL_PROP_FILE_DIALOG_LOCATION_STRING = 'SDL.filedialog.location';
  SDL_PROP_FILE_DIALOG_MANY_BOOLEAN = 'SDL.filedialog.many';
  SDL_PROP_FILE_DIALOG_TITLE_STRING = 'SDL.filedialog.title';
  SDL_PROP_FILE_DIALOG_ACCEPT_STRING = 'SDL.filedialog.accept';
  SDL_PROP_FILE_DIALOG_CANCEL_STRING = 'SDL.filedialog.cancel';


  {$ENDIF read_interface}


  {$IFDEF read_implementation}


  {$ENDIF read_implementation}
