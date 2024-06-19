/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 19-06-2024   19:57                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

function SDL_GetBasePath: PChar; cdecl; external sdl3_lib;
function SDL_GetPrefPath(org: PChar; app: PChar): PChar; cdecl; external sdl3_lib;

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

function SDL_GetUserFolder(folder: TSDL_Folder): PChar; cdecl; external sdl3_lib;

type
  PSDL_PathType = ^TSDL_PathType;
  TSDL_PathType = longint;

const
  SDL_PATHTYPE_NONE = 0;
  SDL_PATHTYPE_FILE = 1;
  SDL_PATHTYPE_DIRECTORY = 2;
  SDL_PATHTYPE_OTHER = 3;

type
  PSDL_PathInfo = ^TSDL_PathInfo;

  TSDL_PathInfo = record
    _type: TSDL_PathType;
    size: TUint64;
    create_time: TSDL_Time;
    modify_time: TSDL_Time;
    access_time: TSDL_Time;
  end;

function SDL_CreateDirectory(path: PChar): longint; cdecl; external sdl3_lib;

type
  TSDL_EnumerateDirectoryCallback = function(userdata: pointer; dirname: PChar; fname: PChar): longint; cdecl;

function SDL_EnumerateDirectory(path: PChar; callback: TSDL_EnumerateDirectoryCallback; userdata: pointer): longint; cdecl; external sdl3_lib;
function SDL_RemovePath(path: PChar): longint; cdecl; external sdl3_lib;
function SDL_RenamePath(oldpath: PChar; newpath: PChar): longint; cdecl; external sdl3_lib;
function SDL_GetPathInfo(path: PChar; info: PSDL_PathInfo): longint; cdecl; external sdl3_lib;

const
  SDL_GLOB_CASEINSENSITIVE = 1 shl 0;

function SDL_GlobDirectory(path: PChar; pattern: PChar; flags: TUint32; Count: Plongint): PPchar; cdecl; external sdl3_lib;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}
