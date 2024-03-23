program Project1;

// https://wiki.libsdl.org/SDL3/SDL_CreateWindow

uses
SDL3;


  procedure main;
  var
    win: PSDL_Window;
  begin
    SDL_init(SDL_INIT_VIDEO);
    win := SDL_CreateWindow('SDL3 Window', 1640, 480, SDL_WINDOW_RESIZABLE);

    if win = nil then begin
      WriteLn('Could not create window: ', SDL_GetError);
      Halt(1);
    end;

    SDL_Delay(3000);
    SDL_DestroyWindow(win);
    SDL_Quit;
  end;

begin
  main;
end.
