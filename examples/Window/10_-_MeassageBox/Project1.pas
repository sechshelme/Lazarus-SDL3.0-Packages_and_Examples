program Project1;

// https://github.com/libsdl-org/SDL/issues/9565

uses
  SDL3;

  procedure main;
  var
    win: PSDL_Window;
    renderer: PSDL_Renderer;
    quit: TSDL_bool = SDL_FALSE;
    e: TSDL_Event;
  begin
    SDL_init(SDL_INIT_VIDEO);
    win := SDL_CreateWindow('SDL3 Window', 640, 480, SDL_WINDOW_RESIZABLE);
    if win = nil then begin
      WriteLn('Could not create window: ', SDL_GetError);
      Halt(1);
    end;
    renderer := SDL_CreateRenderer(win, nil);
    if renderer = nil then begin
      WriteLn('Could not create renderer: ', SDL_GetError);
      Halt(1);
    end;

    while not quit do begin
      while SDL_PollEvent(@e) do begin
        case e._type of
          SDL_EVENT_KEY_DOWN: begin
            case e.key.key of
              SDLK_ESCAPE: begin
                quit := True;
              end;
              SDLK_SPACE: begin
                SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_INFORMATION, 'Info', 'Es wurde [SPACE] gedrückt !', nil);
              end;
            end;
          end;
          SDL_EVENT_QUIT: begin
            quit := True;
          end;
        end;
      end;

      SDL_SetRenderDrawColor(renderer, 20, 120, 20, 255);
      SDL_RenderClear(renderer);

      SDL_RenderPresent(renderer);
    end;


    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(win);
    SDL_Quit;
  end;

begin
  main;
end.
