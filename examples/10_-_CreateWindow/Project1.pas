program Project1;

// https://wiki.libsdl.org/SDL3/SDL_CreateWindow

uses
  SDL3;

  procedure main;
  var
    win: PSDL_Window;
  begin
    SDL_init(SDL_INIT_VIDEO);

//    WriteLn(SDL_Error(2));

SDL_SetError('Fehler 1');
SDL_SetError('Fehler 2');
  WriteLn(SDL_GetError);
  WriteLn(SDL_GetError);

  SDL_LogSetPriority(SDL_LOG_CATEGORY_ERROR, SDL_LOG_PRIORITY_DEBUG);
  //SDL_LogSetAllPriority(SDL_LOG_PRIORITY_ERROR);
WriteLn(SDL_GetError);
    SDL_InvalidParamError('test');
    WriteLn(SDL_GetError);

    win := SDL_CreateWindow('SDL3 Window', 640, 480, SDL_WINDOW_RESIZABLE);

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
