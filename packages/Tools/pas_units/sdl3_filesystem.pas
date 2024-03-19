unit SDL3_filesystem;

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

function SDL_GetBasePath: PChar; cdecl; external;
function SDL_GetPrefPath(org: PChar; app: PChar): PChar; cdecl; external;

type
  PSDL_Folder = ^TSDL_Folder;
  TSDL_Folder = longint;

const
  SDL_FOLDER_HOME = 0;
  SDL_FOLDER_DESKTOP = 1;
  SDL_FOLDER_DOCUMENTS = 2;
  SDL_FOLDER_DOWNLOADS = 3;
  SDL_FOLDER_MUSIC = 4;
  SDL_FOLDER_PICTURES = 5;
  SDL_FOLDER_PUBLICSHARE = 6;
  SDL_FOLDER_SAVEDGAMES = 7;
  SDL_FOLDER_SCREENSHOTS = 8;
  SDL_FOLDER_TEMPLATES = 9;
  SDL_FOLDER_VIDEOS = 10;

function SDL_GetUserFolder(folder: TSDL_Folder): PChar; cdecl; external;

implementation

end.
