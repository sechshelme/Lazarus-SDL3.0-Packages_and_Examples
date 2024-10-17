/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   19:37                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

function SDL_GetBasePath: pansichar; cdecl; external libSDL3;
function SDL_GetPrefPath(org: pansichar; app: pansichar): pansichar; cdecl; external libSDL3;

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
  SDL_FOLDER_COUNT = 11;

function SDL_GetUserFolder(folder: TSDL_Folder): pansichar; cdecl; external libSDL3;

type
  PSDL_PathType = ^TSDL_PathType;
  TSDL_PathType = longint;

const
  SDL_PATHTYPE_NONE = 0;
  SDL_PATHTYPE_FILE = 1;
  SDL_PATHTYPE_DIRECTORY = 2;
  SDL_PATHTYPE_OTHER = 3;

type
  TSDL_PathInfo = record
    _type: TSDL_PathType;
    size: TUint64;
    create_time: TSDL_Time;
    modify_time: TSDL_Time;
    access_time: TSDL_Time;
  end;
  PSDL_PathInfo = ^TSDL_PathInfo;

  TSDL_GlobFlags = TUint32;
  PSDL_GlobFlags = ^TSDL_GlobFlags;

const
  SDL_GLOB_CASEINSENSITIVE = 1 shl 0;

function SDL_CreateDirectory(path: pansichar): Tbool; cdecl; external libSDL3;

type
  PSDL_EnumerationResult = ^TSDL_EnumerationResult;
  TSDL_EnumerationResult = longint;

const
  SDL_ENUM_CONTINUE = 0;
  SDL_ENUM_SUCCESS = 1;
  SDL_ENUM_FAILURE = 2;

type
  TSDL_EnumerateDirectoryCallback = function(userdata: pointer; dirname: pansichar; fname: pansichar): TSDL_EnumerationResult; cdecl;

function SDL_EnumerateDirectory(path: pansichar; callback: TSDL_EnumerateDirectoryCallback; userdata: pointer): Tbool; cdecl; external libSDL3;
function SDL_RemovePath(path: pansichar): Tbool; cdecl; external libSDL3;
function SDL_RenamePath(oldpath: pansichar; newpath: pansichar): Tbool; cdecl; external libSDL3;
function SDL_CopyFile(oldpath: pansichar; newpath: pansichar): Tbool; cdecl; external libSDL3;
function SDL_GetPathInfo(path: pansichar; info: PSDL_PathInfo): Tbool; cdecl; external libSDL3;
function SDL_GlobDirectory(path: pansichar; pattern: pansichar; flags: TSDL_GlobFlags; Count: Plongint): PPChar; cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}
