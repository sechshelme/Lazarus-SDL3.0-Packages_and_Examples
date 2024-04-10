program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

// SDL2 - https://wiki.libsdl.org/SDL2/SDL_RenderGeometry
// SDL3 - https://wiki.libsdl.org/SDL3/SDL_RenderGeometry
// https://discourse.libsdl.org/t/am-i-using-sdl-rendergeometry-correctly/32995

uses
  SDL3;

var
  window: PSDL_Window;
  renderer: PSDL_Renderer;

  vert: array[0..2] of TSDL_Vertex = (
  (position: (x: 400; y: 150); color: (r: $FF; g: $00; b: $00; a: $FF); tex_coord: (x: 0.5; y: 1.0)),
  (position: (x: 200; y: 450); color: (r: $00; g: $00; b: $FF; a: $FF); tex_coord: (x: 0.0; y: 0.0)),
  (position: (x: 600; y: 450); color: (r: $00; g: $FF; b: $00; a: $FF); tex_coord: (x: 1.0; y: 0.0)));

  quit: boolean = False;
  e: TSDL_Event;

begin
  if SDL_Init(SDL_INIT_VIDEO) < 0 then begin
    Halt;
  end;

  window := SDL_CreateWindow('Triangle Example', 800, 600, SDL_WINDOW_RESIZABLE);
  if window = nil then begin
    Halt;
  end;

  renderer := SDL_CreateRenderer(window, nil, SDL_RENDERER_ACCELERATED);
  if renderer = nil then begin
    Halt;
  end;

  while not quit do begin
    while SDL_PollEvent(@e) <> 0 do begin
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

    SDL_SetRenderDrawColor(renderer, $00, $00, $00, $FF);
    SDL_RenderClear(renderer);
    SDL_RenderGeometry(renderer, nil, vert, Length(vert), nil, 0);

    SDL_RenderPresent(renderer);
  end;

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit;

end.
