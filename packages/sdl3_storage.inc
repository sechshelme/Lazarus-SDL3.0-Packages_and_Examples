/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 03-10-2024   17:45                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_Storage = ^TSDL_Storage;
  TSDL_Storage = Pointer;

type
  PSDL_StorageInterface = ^TSDL_StorageInterface;

  TSDL_StorageInterface = record
    Close: function(userdata: pointer): longint; cdecl;
    ready: function(userdata: pointer): TSDL_bool; cdecl;
    enumerate: function(userdata: pointer; path: PChar; callback: TSDL_EnumerateDirectoryCallback; callback_userdata: pointer): longint; cdecl;
    info: function(userdata: pointer; path: PChar; info: PSDL_PathInfo): longint; cdecl;
    read_file: function(userdata: pointer; path: PChar; destination: pointer; length: TUint64): longint; cdecl;
    write_file: function(userdata: pointer; path: PChar; Source: pointer; length: TUint64): longint; cdecl;
    mkdir: function(userdata: pointer; path: PChar): longint; cdecl;
    remove: function(userdata: pointer; path: PChar): longint; cdecl;
    rename: function(userdata: pointer; oldpath: PChar; newpath: PChar): longint; cdecl;
    space_remaining: function(userdata: pointer): TUint64; cdecl;
  end;

function SDL_OpenTitleStorage(override: PChar; props: TSDL_PropertiesID): PSDL_Storage; cdecl; external sdl3_lib;
function SDL_OpenUserStorage(org: PChar; app: PChar; props: TSDL_PropertiesID): PSDL_Storage; cdecl; external sdl3_lib;
function SDL_OpenFileStorage(path: PChar): PSDL_Storage; cdecl; external sdl3_lib;
function SDL_OpenStorage(iface: PSDL_StorageInterface; userdata: pointer): PSDL_Storage; cdecl; external sdl3_lib;
function SDL_CloseStorage(storage: PSDL_Storage): longint; cdecl; external sdl3_lib;
function SDL_StorageReady(storage: PSDL_Storage): TSDL_bool; cdecl; external sdl3_lib;
function SDL_GetStorageFileSize(storage: PSDL_Storage; path: PChar; length: PUint64): longint; cdecl; external sdl3_lib;
function SDL_ReadStorageFile(storage: PSDL_Storage; path: PChar; destination: pointer; length: TUint64): longint; cdecl; external sdl3_lib;
function SDL_WriteStorageFile(storage: PSDL_Storage; path: PChar; Source: pointer; length: TUint64): longint; cdecl; external sdl3_lib;
function SDL_CreateStorageDirectory(storage: PSDL_Storage; path: PChar): longint; cdecl; external sdl3_lib;
function SDL_EnumerateStorageDirectory(storage: PSDL_Storage; path: PChar; callback: TSDL_EnumerateDirectoryCallback; userdata: pointer): longint; cdecl; external sdl3_lib;
function SDL_RemoveStoragePath(storage: PSDL_Storage; path: PChar): longint; cdecl; external sdl3_lib;
function SDL_RenameStoragePath(storage: PSDL_Storage; oldpath: PChar; newpath: PChar): longint; cdecl; external sdl3_lib;
function SDL_GetStoragePathInfo(storage: PSDL_Storage; path: PChar; info: PSDL_PathInfo): longint; cdecl; external sdl3_lib;
function SDL_GetStorageSpaceRemaining(storage: PSDL_Storage): TUint64; cdecl; external sdl3_lib;
function SDL_GlobStorageDirectory(storage: PSDL_Storage; path: PChar; pattern: PChar; flags: TUint32; Count: Plongint): PPchar; cdecl; external sdl3_lib;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}
