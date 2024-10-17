/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   19:37                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_PropertiesID = ^TSDL_PropertiesID;
  TSDL_PropertiesID = uint32;

  PSDL_PropertyType = ^TSDL_PropertyType;
  TSDL_PropertyType = longint;

const
  SDL_PROPERTY_TYPE_INVALID = 0;
  SDL_PROPERTY_TYPE_POINTER = 1;
  SDL_PROPERTY_TYPE_STRING = 2;
  SDL_PROPERTY_TYPE_NUMBER = 3;
  SDL_PROPERTY_TYPE_FLOAT = 4;
  SDL_PROPERTY_TYPE_BOOLEAN = 5;

function SDL_GetGlobalProperties: TSDL_PropertiesID; cdecl; external sdl3_lib;
function SDL_CreateProperties: TSDL_PropertiesID; cdecl; external sdl3_lib;
function SDL_CopyProperties(src: TSDL_PropertiesID; dst: TSDL_PropertiesID): longint; cdecl; external sdl3_lib;
function SDL_LockProperties(props: TSDL_PropertiesID): longint; cdecl; external sdl3_lib;
procedure SDL_UnlockProperties(props: TSDL_PropertiesID); cdecl; external sdl3_lib;

type
  SDL_SetPropertyWithCleanup_func = procedure(userdata: pointer; Value: pointer); cdecl;

function SDL_SetPropertyWithCleanup(props: TSDL_PropertiesID; Name: PChar; Value: pointer; cleanup: SDL_SetPropertyWithCleanup_func; userdata: pointer): longint; cdecl; external sdl3_lib;
function SDL_SetProperty(props: TSDL_PropertiesID; Name: PChar; Value: pointer): longint; cdecl; external sdl3_lib;
function SDL_SetStringProperty(props: TSDL_PropertiesID; Name: PChar; Value: PChar): longint; cdecl; external sdl3_lib;
function SDL_SetNumberProperty(props: TSDL_PropertiesID; Name: PChar; Value: int64): longint; cdecl; external sdl3_lib;
function SDL_SetFloatProperty(props: TSDL_PropertiesID; Name: PChar; Value: single): longint; cdecl; external sdl3_lib;
function SDL_SetBooleanProperty(props: TSDL_PropertiesID; Name: PChar; Value: TSDL_bool): longint; cdecl; external sdl3_lib;
function SDL_HasProperty(props: TSDL_PropertiesID; Name: PChar): TSDL_bool; cdecl; external sdl3_lib;
function SDL_GetPropertyType(props: TSDL_PropertiesID; Name: PChar): TSDL_PropertyType; cdecl; external sdl3_lib;
function SDL_GetProperty(props: TSDL_PropertiesID; Name: PChar; default_value: pointer): pointer; cdecl; external sdl3_lib;
function SDL_GetStringProperty(props: TSDL_PropertiesID; Name: PChar; default_value: PChar): PChar; cdecl; external sdl3_lib;
function SDL_GetNumberProperty(props: TSDL_PropertiesID; Name: PChar; default_value: int64): int64; cdecl; external sdl3_lib;
function SDL_GetFloatProperty(props: TSDL_PropertiesID; Name: PChar; default_value: single): single; cdecl; external sdl3_lib;
function SDL_GetBooleanProperty(props: TSDL_PropertiesID; Name: PChar; default_value: TSDL_bool): TSDL_bool; cdecl; external sdl3_lib;
function SDL_ClearProperty(props: TSDL_PropertiesID; Name: PChar): longint; cdecl; external sdl3_lib;

type
  TSDL_EnumeratePropertiesCallback = procedure(userdata: pointer; props: TSDL_PropertiesID; Name: PChar); cdecl;

function SDL_EnumerateProperties(props: TSDL_PropertiesID; callback: TSDL_EnumeratePropertiesCallback; userdata: pointer): longint; cdecl; external sdl3_lib;
procedure SDL_DestroyProperties(props: TSDL_PropertiesID); cdecl; external sdl3_lib;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}
