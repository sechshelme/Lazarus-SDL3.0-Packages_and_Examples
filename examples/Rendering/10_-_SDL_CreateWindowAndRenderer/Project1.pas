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
  bitmapSurface: PSDL_Surface;
  renderer: PSDL_Renderer;
  bitmapTex: PSDL_Texture;

  function CreateSurface: PSDL_Surface;
  const
    size = 64;
  var
    r: TSDL_Rect;
  begin
    Result := SDL_CreateSurface(size, size, SDL_PIXELFORMAT_RGBA32);
    if Result = nil then begin
      SDL_Log('Kann kein Surface erzeugen !');
    end;
    r.x := 0;
    r.y := 0;
    r.w := size;
    r.h := size;
    SDL_FillSurfaceRect(Result, @r, $8888FFFF);
  end;

  procedure SDLMain;
  const
    step = 0.01;
  var
    e: TSDL_Event;
    quit: boolean = False;
    rDest: TSDL_FRect;
    time: extended;
    red, green, blue: single;
    keyStat: PBoolean;
  begin
    rDest.w := 100;
    rDest.h := 100;
    rDest.x := (Width - rDest.w) / 2;
    rDest.y := (heigt - rDest.h) / 2;
    while not quit do begin
      keyStat := SDL_GetKeyboardState(nil);
      if keyStat[SDL_SCANCODE_SPACE] then begin
      end;

      if keyStat[SDL_SCANCODE_RIGHT] then begin
        if keyStat[SDL_SCANCODE_LSHIFT] then begin
          rDest.x -= step;
          rDest.w += step * 2;
        end else begin
          rDest.x += step;
        end;
      end;
      if keyStat[SDL_SCANCODE_LEFT] then begin
        if keyStat[SDL_SCANCODE_LSHIFT] then begin
          if rDest.w > 1 then begin
            rDest.x += step;
            rDest.w -= step * 2;
          end;
        end else begin
          rDest.x -= step;
        end;
      end;
      if keyStat[SDL_SCANCODE_DOWN] then begin
        if keyStat[SDL_SCANCODE_LSHIFT] then begin
          rDest.y -= step;
          rDest.h += step * 2;
        end else begin
          rDest.y += step;
        end;
      end;
      if keyStat[SDL_SCANCODE_UP] then begin
        if keyStat[SDL_SCANCODE_LSHIFT] then begin
          if rDest.h > 1 then begin
            rDest.y += step;
            rDest.h -= step * 2;
          end;
        end else begin
          rDest.y -= step;
        end;
      end;

      if rDest.h < 1 then begin
        rDest.h := 1;
      end;


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

      time := SDL_GetTicks / 1000;
      red := (SDL_sinf(time) + 1) / 2.0;
      green := (SDL_sinf(time / 2) + 1) / 2.0;
      blue := (SDL_sinf(time / 3) + 1) / 2.0;

      SDL_SetRenderDrawColorFloat(renderer, red, green, blue, SDL_ALPHA_OPAQUE);
      SDL_RenderClear(renderer);

      SDL_RenderTexture(renderer, bitmapTex, nil, @rDest);
      SDL_RenderPresent(renderer);
    end;
  end;

begin
  SDL_init(SDL_INIT_VIDEO);

  SDL_CreateWindowAndRenderer('titel', Width, heigt, SDL_WINDOW_HIDDEN, @window, @renderer);

  bitmapSurface := CreateSurface;

  bitmapTex := SDL_CreateTextureFromSurface(renderer, bitmapSurface);
  if bitmapSurface = nil then begin
    SDL_Log('Kann bmp nicht laden !');
  end;

  SDL_DestroySurface(bitmapSurface);
  SDL_ShowWindow(window);

  SDLMain;

  SDL_DestroyTexture(bitmapTex);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  SDL_Quit;
end.
