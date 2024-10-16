unit SDL_storage;

interface

uses
  ctypes, SDL_stdinc, SDL_properties, SDL_filesystem;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  TSDL_StorageInterface = record
    version: TUint32;
    Close: function(userdata: pointer): Tbool; cdecl;
    ready: function(userdata: pointer): Tbool; cdecl;
    enumerate: function(userdata: pointer; path: pansichar; callback: TSDL_EnumerateDirectoryCallback; callback_userdata: pointer): Tbool; cdecl;
    info: function(userdata: pointer; path: pansichar; info: PSDL_PathInfo): Tbool; cdecl;
    read_file: function(userdata: pointer; path: pansichar; destination: pointer; length: TUint64): Tbool; cdecl;
    write_file: function(userdata: pointer; path: pansichar; Source: pointer; length: TUint64): Tbool; cdecl;
    mkdir: function(userdata: pointer; path: pansichar): Tbool; cdecl;
    remove: function(userdata: pointer; path: pansichar): Tbool; cdecl;
    rename: function(userdata: pointer; oldpath: pansichar; newpath: pansichar): Tbool; cdecl;
    copy: function(userdata: pointer; oldpath: pansichar; newpath: pansichar): Tbool; cdecl;
    space_remaining: function(userdata: pointer): TUint64; cdecl;
  end;
  PSDL_StorageInterface = ^TSDL_StorageInterface;

  TSDL_Storage = record
  end;
  PSDL_Storage = ^TSDL_Storage;

function SDL_OpenTitleStorage(override: pansichar; props: TSDL_PropertiesID): PSDL_Storage; cdecl; external libSDL3;
function SDL_OpenUserStorage(org: pansichar; app: pansichar; props: TSDL_PropertiesID): PSDL_Storage; cdecl; external libSDL3;
function SDL_OpenFileStorage(path: pansichar): PSDL_Storage; cdecl; external libSDL3;
function SDL_OpenStorage(iface: PSDL_StorageInterface; userdata: pointer): PSDL_Storage; cdecl; external libSDL3;
function SDL_CloseStorage(storage: PSDL_Storage): Tbool; cdecl; external libSDL3;
function SDL_StorageReady(storage: PSDL_Storage): Tbool; cdecl; external libSDL3;
function SDL_GetStorageFileSize(storage: PSDL_Storage; path: pansichar; length: PUint64): Tbool; cdecl; external libSDL3;
function SDL_ReadStorageFile(storage: PSDL_Storage; path: pansichar; destination: pointer; length: TUint64): Tbool; cdecl; external libSDL3;
function SDL_WriteStorageFile(storage: PSDL_Storage; path: pansichar; Source: pointer; length: TUint64): Tbool; cdecl; external libSDL3;
function SDL_CreateStorageDirectory(storage: PSDL_Storage; path: pansichar): Tbool; cdecl; external libSDL3;
function SDL_EnumerateStorageDirectory(storage: PSDL_Storage; path: pansichar; callback: TSDL_EnumerateDirectoryCallback; userdata: pointer): Tbool; cdecl; external libSDL3;
function SDL_RemoveStoragePath(storage: PSDL_Storage; path: pansichar): Tbool; cdecl; external libSDL3;
function SDL_RenameStoragePath(storage: PSDL_Storage; oldpath: pansichar; newpath: pansichar): Tbool; cdecl; external libSDL3;
function SDL_CopyStorageFile(storage: PSDL_Storage; oldpath: pansichar; newpath: pansichar): Tbool; cdecl; external libSDL3;
function SDL_GetStoragePathInfo(storage: PSDL_Storage; path: pansichar; info: PSDL_PathInfo): Tbool; cdecl; external libSDL3;
function SDL_GetStorageSpaceRemaining(storage: PSDL_Storage): TUint64; cdecl; external libSDL3;
function SDL_GlobStorageDirectory(storage: PSDL_Storage; path: pansichar; pattern: pansichar; flags: TSDL_GlobFlags; Count: Plongint): PPChar; cdecl; external libSDL3;

implementation


end.
