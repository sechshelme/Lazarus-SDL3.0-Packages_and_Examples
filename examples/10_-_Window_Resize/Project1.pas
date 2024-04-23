program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  ctypes,
  SDL3;

const
  Width = 800;
  heigt = 600;

var
  window: PSDL_Window;
  renderer: PSDL_Renderer;

  procedure Run;
  var
    e: TSDL_Event;
    quit: boolean = False;
    red, green, blue, time: single;
    pc: PChar;
  begin
    while not quit do begin
      while SDL_PollEvent(@e) do begin
        case e.type_ of
          SDL_EVENT_KEY_DOWN: begin
            case e.key.keysym.sym of

              SDLK_ESCAPE: begin
                quit := True;
              end;
            end;
          end;
          SDL_EVENT_QUIT: begin
            quit := True;
          end;
          SDL_EVENT_WINDOW_RESIZED: begin
            SDL_Log('X: %i   Y: %i', e.window.data1, e.window.data2);
            SDL_asprintf(@pc, 'X: %i   Y: %i', [e.window.data1, e.window.data2]);
            SDL_SetWindowTitle(window, pc);
            SDL_free(pc);
          end;
        end;
      end;

      time := SDL_GetTicks / 1000;
      red := (SDL_sinf(time) + 1) / 2.0;
      green := (SDL_sinf(time / 2) + 1) / 2.0;
      blue := (SDL_sinf(time / 3) + 1) / 2.0;

      SDL_SetRenderDrawColorFloat(renderer, red, green, blue, SDL_ALPHA_OPAQUE);
      SDL_RenderClear(renderer);

      SDL_RenderPresent(renderer);
    end;
  end;

  procedure main;
  begin
    SDL_init(SDL_INIT_VIDEO);

    window := SDL_CreateWindow('SDL3 Window', Width, heigt, SDL_WINDOW_RESIZABLE);
    if window = nil then begin
      SDL_Log('Kann kein SDL-Fenster erzeugen !');
    end;

    renderer := SDL_CreateRenderer(window, nil, SDL_RENDERER_ACCELERATED);
    if renderer = nil then begin
      SDL_Log('Kann kein SDL-Renderer erzeugen !');
    end;

    Run;

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit;
  end;

begin
  main;
end.
