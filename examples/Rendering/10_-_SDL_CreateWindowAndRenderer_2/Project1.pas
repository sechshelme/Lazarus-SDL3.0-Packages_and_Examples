program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  ctypes,
  SDL3;

const
  winCount = 16;
var
  window: array[0..winCount] of PSDL_Window;
  renderer: array[0..winCount] of PSDL_Renderer;
  i: integer;

  procedure SDLMain;
  var
    e: TSDL_Event;
    quit: boolean = False;
    rDest: TSDL_FRect;
    i: integer;
    timeStart: uint64;
  begin
    rDest.items := [150, 150, 256, 256];
    while not quit do begin
      while SDL_PollEvent(@e) do begin
        case e._type of
          SDL_EVENT_KEY_DOWN: begin
            case e.key.key of

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

      rDest.x += 0.1;
      if rDest.x > 300 then begin
        rDest.x := 0;
      end;

      timeStart := SDL_GetTicksNS;
      for i := 0 to winCount - 1 do begin
        SDL_SetRenderDrawColorFloat(renderer[i], 0.5, 1.0, 1.0, SDL_ALPHA_OPAQUE);
        SDL_RenderClear(renderer[i]);

        SDL_SetRenderDrawColorFloat(renderer[i], 1.0, 0.5, 0.5, SDL_ALPHA_OPAQUE);
        SDL_RenderFillRect(renderer[i], @rDest);

        SDL_RenderPresent(renderer[i]);
      end;
      SDL_Log('Time: %i', (SDL_GetTicksNS - timeStart) div 1000);
    end;
  end;

begin
  SDL_init(SDL_INIT_VIDEO);

  for i := 0 to winCount - 1 do begin
    window[i] := SDL_CreateWindow('Fenster', 320, 200, 0);

    renderer[i] := SDL_CreateRenderer(window[i], 'vulkan');
    if renderer[i] = nil then begin
      SDL_Log('Renderer Error: %s', SDL_GetError);
      Halt;
    end;
    SDL_SetWindowPosition(window[i], i mod 4 * 350, i div 4 * 250);
  end;

  SDLMain;

  for i := 0 to winCount - 1 do begin
    SDL_DestroyRenderer(renderer[i]);
    SDL_DestroyWindow(window[i]);
  end;

  SDL_Quit;
end.
