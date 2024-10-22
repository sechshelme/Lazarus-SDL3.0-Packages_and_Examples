/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 22-10-2024   15:23                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_InitFlags = ^TSDL_InitFlags;
  TSDL_InitFlags = TUint32;

const
  SDL_INIT_AUDIO = $00000010;
  SDL_INIT_VIDEO = $00000020;
  SDL_INIT_JOYSTICK = $00000200;
  SDL_INIT_HAPTIC = $00001000;
  SDL_INIT_GAMEPAD = $00002000;
  SDL_INIT_EVENTS = $00004000;
  SDL_INIT_SENSOR = $00008000;
  SDL_INIT_CAMERA = $00010000;

type
  PSDL_AppResult = ^TSDL_AppResult;
  TSDL_AppResult = longint;

const
  SDL_APP_CONTINUE = 0;
  SDL_APP_SUCCESS = 1;
  SDL_APP_FAILURE = 2;

type
  TSDL_AppInit_func = function(appstate: Ppointer; argc: longint; argv: PPansichar): TSDL_AppResult; cdecl;
  TSDL_AppIterate_func = function(appstate: pointer): TSDL_AppResult; cdecl;
  TSDL_AppEvent_func = function(appstate: pointer; event: PSDL_Event): TSDL_AppResult; cdecl;
  TSDL_AppQuit_func = procedure(appstate: pointer; Result: TSDL_AppResult); cdecl;

function SDL_Init(flags: TSDL_InitFlags): Tbool; cdecl; external libSDL3;
function SDL_InitSubSystem(flags: TSDL_InitFlags): Tbool; cdecl; external libSDL3;
procedure SDL_QuitSubSystem(flags: TSDL_InitFlags); cdecl; external libSDL3;
function SDL_WasInit(flags: TSDL_InitFlags): TSDL_InitFlags; cdecl; external libSDL3;
procedure SDL_Quit; cdecl; external libSDL3;
function SDL_SetAppMetadata(appname: pansichar; appversion: pansichar; appidentifier: pansichar): Tbool; cdecl; external libSDL3;
function SDL_SetAppMetadataProperty(Name: pansichar; Value: pansichar): Tbool; cdecl; external libSDL3;

const
  SDL_PROP_APP_METADATA_NAME_STRING = 'SDL.app.metadata.name';
  SDL_PROP_APP_METADATA_VERSION_STRING = 'SDL.app.metadata.version';
  SDL_PROP_APP_METADATA_IDENTIFIER_STRING = 'SDL.app.metadata.identifier';
  SDL_PROP_APP_METADATA_CREATOR_STRING = 'SDL.app.metadata.creator';
  SDL_PROP_APP_METADATA_COPYRIGHT_STRING = 'SDL.app.metadata.copyright';
  SDL_PROP_APP_METADATA_URL_STRING = 'SDL.app.metadata.url';
  SDL_PROP_APP_METADATA_TYPE_STRING = 'SDL.app.metadata.type';

function SDL_GetAppMetadataProperty(Name: pansichar): pansichar; cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}
