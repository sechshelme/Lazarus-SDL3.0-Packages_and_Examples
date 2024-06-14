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

  procedure SDLMain;
  var
    e: TSDL_Event;
    quit: boolean = False;
    rect: TSDL_FRect;
    VPrect: TSDL_Rect;
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
        end;
      end;

      SDL_SetRenderDrawColorFloat(renderer, 0, 0, 0, SDL_ALPHA_OPAQUE);
//      SDL_RenderClear(renderer);

      vprect.x := 100;
      vprect.y := 100;
      vprect.w := 100;
      vprect.h := 100;
      SDL_SetRenderViewport(renderer, @VPrect);


      rect.x := Random * 500;
      rect.y := Random * 500;
      rect.w := Random * 500;
      rect.h := Random * 500;

      SDL_SetRenderDrawColorFloat(renderer, Random, Random, Random, SDL_ALPHA_OPAQUE);
      SDL_RenderFillRect(renderer, @rect);

      SDL_RenderPresent(renderer);
    end;
  end;

begin
  SDL_init(SDL_INIT_VIDEO);

  window := SDL_CreateWindow('SDL3 Window', Width, heigt, SDL_WINDOW_RESIZABLE);
  if window = nil then begin
    SDL_Log('Kann kein SDL-Fenster erzeugen !');
  end;

  renderer := SDL_CreateRenderer(window, nil);

  if renderer = nil then begin
    SDL_Log('Kann kein SDL-Renderer erzeugen !');
  end;

  SDLMain;

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  SDL_Quit;
end.
