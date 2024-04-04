program Project1;

uses
  ctypes,SDL3;

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
