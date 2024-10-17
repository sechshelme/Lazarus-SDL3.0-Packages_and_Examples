/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   19:37                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

const
  SDL_MAIN_USE_CALLBACKS = 1;

function SDL_AppInit(appstate: Ppointer; argc: longint; argv: PPansichar): TSDL_AppResult; cdecl; external libSDL3;
function SDL_AppIterate(appstate: pointer): TSDL_AppResult; cdecl; external libSDL3;
function SDL_AppEvent(appstate: pointer; event: PSDL_Event): TSDL_AppResult; cdecl; external libSDL3;
procedure SDL_AppQuit(appstate: pointer; Result: TSDL_AppResult); cdecl; external libSDL3;

type
  TSDL_main_func = function(argc: longint; argv: PPansichar): longint; cdecl;

function SDL_main(argc: longint; argv: PPansichar): longint; cdecl; external libSDL3;
procedure SDL_SetMainReady; cdecl; external libSDL3;
function SDL_RunApp(argc: longint; argv: PPansichar; mainFunction: TSDL_main_func; reserved: pointer): longint; cdecl; external libSDL3;
function SDL_EnterAppMainCallbacks(argc: longint; argv: PPansichar; appinit: TSDL_AppInit_func; appiter: TSDL_AppIterate_func; appevent: TSDL_AppEvent_func;
  appquit: TSDL_AppQuit_func): longint; cdecl; external libSDL3;
function SDL_RegisterApp(Name: pansichar; style: TUint32; hInst: pointer): Tbool; cdecl; external libSDL3;
procedure SDL_UnregisterApp; cdecl; external libSDL3;
procedure SDL_GDKSuspendComplete; cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}
