unit SDL3_init;

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

type
  PSDL_InitFlags = ^TSDL_InitFlags;
  TSDL_InitFlags = longint;

const
  SDL_INIT_TIMER = $00000001;
  SDL_INIT_AUDIO = $00000010;
  SDL_INIT_VIDEO = $00000020;
  SDL_INIT_JOYSTICK = $00000200;
  SDL_INIT_HAPTIC = $00001000;
  SDL_INIT_GAMEPAD = $00002000;
  SDL_INIT_EVENTS = $00004000;
  SDL_INIT_SENSOR = $00008000;
  SDL_INIT_CAMERA = $00010000;

function SDL_Init(flags: uint32): longint; cdecl; external sdl3_lib;
function SDL_InitSubSystem(flags: uint32): longint; cdecl; external sdl3_lib;
procedure SDL_QuitSubSystem(flags: uint32); cdecl; external sdl3_lib;
function SDL_WasInit(flags: uint32): uint32; cdecl; external sdl3_lib;
procedure SDL_Quit; cdecl; external sdl3_lib;

implementation

end.
