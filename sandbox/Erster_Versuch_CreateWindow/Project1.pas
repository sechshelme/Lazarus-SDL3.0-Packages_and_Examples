program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal
// https://www.lazarusforum.de/viewtopic.php?f=10&t=15222&p=138351&hilit=linklib#p138351

uses
  ctypes;

  //{$LinkLib 'SDL3.dll'}
  //{$LinkLib 'SDL3.dll',shared}
  //{$LinkLib 'SDL3.dll',static}
  //{$LinkLib 'libSDL3.so.0.1.1',static}
const
  //  SDL_LibName = '/usr/local/lib/libSDL';
  //  SDL_LibName = 'libSDL3.so.0';
  SDL_LibName = 'SDL3';
  //  SDL_LibName  = 'libSDL2.so.0';



type
  PSDL_Version = ^TSDL_Version;

  TSDL_Window = record
  end;

  PSDL_Window = ^TSDL_Window;

  TSDL_Version = record
    major,
    minor,
    patch: cuint8;
  end;

const
  SDL_INIT_VIDEO = $00000020;
  SDL_WINDOW_RESIZABLE = $00000020;

  procedure SDL_GetVersion(ver: PSDL_Version); cdecl; external SDL_LibName;

  function SDL_Init(flags: cuint32): cint; cdecl; external SDL_LibName;
  function SDL_CreateWindow(title: pchar; w, h: cint; flags: cuint32): PSDL_Window; cdecl; external SDL_LibName;
  function SDL_DestroyWindow(window: PSDL_Window): Pointer; cdecl; external SDL_LibName;
  procedure SDL_Delay(ms: cuint32); cdecl; external SDL_LibName;

var
  ver: TSDL_Version;
  window: PSDL_Window;

begin
  SDL_Init(SDL_INIT_VIDEO);

  window := SDL_CreateWindow('SDL3 Window', 320, 200, SDL_WINDOW_RESIZABLE);
  SDL_Delay(3000);
  SDL_DestroyWindow(window);

  SDL_GetVersion(@ver);
  WriteLn(ver.major, '.', ver.minor, '.', ver.patch);
end.
