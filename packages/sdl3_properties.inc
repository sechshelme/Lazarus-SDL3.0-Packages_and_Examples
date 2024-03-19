/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
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

function SDL_GetGlobalProperties: TSDL_PropertiesID; cdecl; external;
function SDL_CreateProperties: TSDL_PropertiesID; cdecl; external;
function SDL_CopyProperties(src: TSDL_PropertiesID; dst: TSDL_PropertiesID): longint; cdecl; external;
function SDL_LockProperties(props: TSDL_PropertiesID): longint; cdecl; external;
procedure SDL_UnlockProperties(props: TSDL_PropertiesID); cdecl; external;

type
  SDL_SetPropertyWithCleanup_func = procedure(userdata: pointer; Value: pointer); cdecl;

function SDL_SetPropertyWithCleanup(props: TSDL_PropertiesID; Name: PChar; Value: pointer; cleanup: SDL_SetPropertyWithCleanup_func; userdata: pointer): longint; cdecl; external;
function SDL_SetProperty(props: TSDL_PropertiesID; Name: PChar; Value: pointer): longint; cdecl; external;
function SDL_SetStringProperty(props: TSDL_PropertiesID; Name: PChar; Value: PChar): longint; cdecl; external;
function SDL_SetNumberProperty(props: TSDL_PropertiesID; Name: PChar; Value: int64): longint; cdecl; external;
function SDL_SetFloatProperty(props: TSDL_PropertiesID; Name: PChar; Value: single): longint; cdecl; external;
function SDL_SetBooleanProperty(props: TSDL_PropertiesID; Name: PChar; Value: TSDL_bool): longint; cdecl; external;
function SDL_HasProperty(props: TSDL_PropertiesID; Name: PChar): TSDL_bool; cdecl; external;
function SDL_GetPropertyType(props: TSDL_PropertiesID; Name: PChar): TSDL_PropertyType; cdecl; external;
function SDL_GetProperty(props: TSDL_PropertiesID; Name: PChar; default_value: pointer): pointer; cdecl; external;
function SDL_GetStringProperty(props: TSDL_PropertiesID; Name: PChar; default_value: PChar): PChar; cdecl; external;
function SDL_GetNumberProperty(props: TSDL_PropertiesID; Name: PChar; default_value: int64): int64; cdecl; external;
function SDL_GetFloatProperty(props: TSDL_PropertiesID; Name: PChar; default_value: single): single; cdecl; external;
function SDL_GetBooleanProperty(props: TSDL_PropertiesID; Name: PChar; default_value: TSDL_bool): TSDL_bool; cdecl; external;
function SDL_ClearProperty(props: TSDL_PropertiesID; Name: PChar): longint; cdecl; external;

type
  TSDL_EnumeratePropertiesCallback = procedure(userdata: pointer; props: TSDL_PropertiesID; Name: PChar); cdecl;

function SDL_EnumerateProperties(props: TSDL_PropertiesID; callback: TSDL_EnumeratePropertiesCallback; userdata: pointer): longint; cdecl; external;
procedure SDL_DestroyProperties(props: TSDL_PropertiesID); cdecl; external;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}
