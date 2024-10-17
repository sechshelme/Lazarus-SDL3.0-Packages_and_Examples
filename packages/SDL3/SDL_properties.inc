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
  TSDL_PropertiesID = TUint32;

  PSDL_PropertyType = ^TSDL_PropertyType;
  TSDL_PropertyType = longint;

const
  SDL_PROPERTY_TYPE_INVALID = 0;
  SDL_PROPERTY_TYPE_POINTER = 1;
  SDL_PROPERTY_TYPE_STRING = 2;
  SDL_PROPERTY_TYPE_NUMBER = 3;
  SDL_PROPERTY_TYPE_FLOAT = 4;
  SDL_PROPERTY_TYPE_BOOLEAN = 5;

function SDL_GetGlobalProperties: TSDL_PropertiesID; cdecl; external libSDL3;
function SDL_CreateProperties: TSDL_PropertiesID; cdecl; external libSDL3;
function SDL_CopyProperties(src: TSDL_PropertiesID; dst: TSDL_PropertiesID): Tbool; cdecl; external libSDL3;
function SDL_LockProperties(props: TSDL_PropertiesID): Tbool; cdecl; external libSDL3;
procedure SDL_UnlockProperties(props: TSDL_PropertiesID); cdecl; external libSDL3;

type
  TSDL_CleanupPropertyCallback = procedure(userdata: pointer; Value: pointer); cdecl;

function SDL_SetPointerPropertyWithCleanup(props: TSDL_PropertiesID; Name: pansichar; Value: pointer; cleanup: TSDL_CleanupPropertyCallback; userdata: pointer): Tbool; cdecl; external libSDL3;
function SDL_SetPointerProperty(props: TSDL_PropertiesID; Name: pansichar; Value: pointer): Tbool; cdecl; external libSDL3;
function SDL_SetStringProperty(props: TSDL_PropertiesID; Name: pansichar; Value: pansichar): Tbool; cdecl; external libSDL3;
function SDL_SetNumberProperty(props: TSDL_PropertiesID; Name: pansichar; Value: TSint64): Tbool; cdecl; external libSDL3;
function SDL_SetFloatProperty(props: TSDL_PropertiesID; Name: pansichar; Value: single): Tbool; cdecl; external libSDL3;
function SDL_SetBooleanProperty(props: TSDL_PropertiesID; Name: pansichar; Value: Tbool): Tbool; cdecl; external libSDL3;
function SDL_HasProperty(props: TSDL_PropertiesID; Name: pansichar): Tbool; cdecl; external libSDL3;
function SDL_GetPropertyType(props: TSDL_PropertiesID; Name: pansichar): TSDL_PropertyType; cdecl; external libSDL3;
function SDL_GetPointerProperty(props: TSDL_PropertiesID; Name: pansichar; default_value: pointer): pointer; cdecl; external libSDL3;
function SDL_GetStringProperty(props: TSDL_PropertiesID; Name: pansichar; default_value: pansichar): pansichar; cdecl; external libSDL3;
function SDL_GetNumberProperty(props: TSDL_PropertiesID; Name: pansichar; default_value: TSint64): TSint64; cdecl; external libSDL3;
function SDL_GetFloatProperty(props: TSDL_PropertiesID; Name: pansichar; default_value: single): single; cdecl; external libSDL3;
function SDL_GetBooleanProperty(props: TSDL_PropertiesID; Name: pansichar; default_value: Tbool): Tbool; cdecl; external libSDL3;
function SDL_ClearProperty(props: TSDL_PropertiesID; Name: pansichar): Tbool; cdecl; external libSDL3;

type
  TSDL_EnumeratePropertiesCallback = procedure(userdata: pointer; props: TSDL_PropertiesID; Name: pansichar); cdecl;

function SDL_EnumerateProperties(props: TSDL_PropertiesID; callback: TSDL_EnumeratePropertiesCallback; userdata: pointer): Tbool; cdecl; external libSDL3;
procedure SDL_DestroyProperties(props: TSDL_PropertiesID); cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}
