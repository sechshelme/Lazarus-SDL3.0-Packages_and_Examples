program Project1;

// https://wiki.libsdl.org/SDL3/SDL_CreatePopupWindow

uses
  ctypes,
  SDL3;

  procedure main;
  var
    win,popWin: PSDL_Window;
  begin
    SDL_init(SDL_INIT_VIDEO);

    win := SDL_CreateWindow('SDL3 Window', 640, 480, SDL_WINDOW_RESIZABLE);
    popWin := SDL_CreatePopupWindow(win, 10,10, 320, 240, SDL_WINDOW_POPUP_MENU);

    if win = nil then begin
      WriteLn('Could not create window: ', SDL_GetError);
      Halt(1);
    end;

    SDL_Delay(3000);
    SDL_DestroyWindow(popWin);
    SDL_DestroyWindow(win);
    SDL_Quit;
  end;

begin
  main;
end.
