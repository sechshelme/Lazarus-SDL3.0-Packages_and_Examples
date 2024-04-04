program Project1;

uses
  ctypes;

const
  {$IFDEF Linux}
  sdl3_lib = 'SDL3';
  {$ENDIF}

  {$IFDEF Windows}
  sdl3_lib = 'SDL3.dll';
  {$ENDIF}

type
  TSDL_Window = Pointer;
  PSDL_Window = ^TSDL_Window;
  PPSDL_Window = ^PSDL_Window;

const
  SDL_INIT_VIDEO = $00000020;
  SDL_WINDOW_RESIZABLE = $00000020;


  function SDL_Init(flags: uint32): longint; cdecl; external sdl3_lib;
  function SDL_CreateWindow(title: PChar; w: longint; h: longint; flags: uint32): PSDL_Window; cdecl; external sdl3_lib;
  procedure SDL_Delay(ms: uint32); cdecl; external sdl3_lib;

var
  gWindow: PSDL_Window;

  procedure Init_SDL_and_OpenGL;
  begin
    // --- SDL inizialisieren
    if SDL_Init(SDL_INIT_VIDEO) < 0 then begin
      WriteLn('SDL could not initialize! SDL_Error');
      Halt(1);
    end;

    gwindow := SDL_CreateWindow('SDL3 Window', 320, 200, SDL_WINDOW_RESIZABLE);

    SDL_Delay(5000);
  end;

begin
  Init_SDL_and_OpenGL;
end.
