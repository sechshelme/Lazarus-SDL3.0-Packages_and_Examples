unit SDL3_main;

interface

uses
  SDL3_events;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  TSDL_AppInit_func = function(argc: longint; argv: PPchar): longint; cdecl;
  TSDL_AppIterate_func = function(para1: pointer): longint; cdecl;
  TSDL_AppEvent_func = function(event: PSDL_Event): longint; cdecl;
  TSDL_AppQuit_func = procedure(para1: pointer); cdecl;

function SDL_AppInit(argc: longint; argv: PPchar): longint; cdecl; external;
function SDL_AppIterate: longint; cdecl; external;
function SDL_AppEvent(event: PSDL_Event): longint; cdecl; external;
procedure SDL_AppQuit; cdecl; external;

type
  TSDL_main_func = function(argc: longint; argv: PPchar): longint; cdecl;

function SDL_main(argc: longint; argv: PPchar): longint; cdecl; external;
procedure SDL_SetMainReady; cdecl; external;
function SDL_RunApp(argc: longint; argv: PPchar; mainFunction: TSDL_main_func; reserved: pointer): longint; cdecl; external;
function SDL_EnterAppMainCallbacks(argc: longint; argv: PPchar; appinit: TSDL_AppInit_func; appiter: TSDL_AppIterate_func; appevent: TSDL_AppEvent_func;
  appquit: TSDL_AppQuit_func): longint; cdecl; external;
function SDL_RegisterApp(Name: PChar; style: uint32; hInst: pointer): longint; cdecl; external;
procedure SDL_UnregisterApp; cdecl; external;
procedure SDL_GDKSuspendComplete; cdecl; external;

implementation

end.
