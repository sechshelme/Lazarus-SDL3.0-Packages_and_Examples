unit SDL_dialog;

interface

uses
  ctypes, SDL_stdinc;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


type
  PSDL_FileDialogType = ^TSDL_FileDialogType;
  TSDL_FileDialogType =  Longint;
  Const
    SDL_FILEDIALOG_OPENFILE = 0;
    SDL_FILEDIALOG_SAVEFILE = 1;
    SDL_FILEDIALOG_OPENFOLDER = 2;

procedure SDL_ShowFileDialogWithProperties(_type:TSDL_FileDialogType; callback:TSDL_DialogFileCallback; userdata:pointer; props:TSDL_PropertiesID);cdecl;external libSDL3;
const
  SDL_PROP_FILE_DIALOG_FILTERS_POINTER = 'SDL.filedialog.filters';  
  SDL_PROP_FILE_DIALOG_NFILTERS_NUMBER = 'SDL.filedialog.nfilters';  
  SDL_PROP_FILE_DIALOG_WINDOW_POINTER = 'SDL.filedialog.window';  
  SDL_PROP_FILE_DIALOG_LOCATION_STRING = 'SDL.filedialog.location';  
  SDL_PROP_FILE_DIALOG_MANY_BOOLEAN = 'SDL.filedialog.many';  
  SDL_PROP_FILE_DIALOG_TITLE_STRING = 'SDL.filedialog.title';  
  SDL_PROP_FILE_DIALOG_ACCEPT_STRING = 'SDL.filedialog.accept';  
  SDL_PROP_FILE_DIALOG_CANCEL_STRING = 'SDL.filedialog.cancel';  

implementation


end.
