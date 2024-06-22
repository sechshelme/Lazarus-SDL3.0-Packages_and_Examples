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
  bitmapTex: PSDL_Texture;
  sur: PSDL_Surface;

  function CreateSurface: PSDL_Texture;
  const
    size = 64;
  var
    r: TSDL_Rect;
    surface: PSDL_Surface;
  begin
    surface := SDL_CreateSurface(size, size, SDL_PIXELFORMAT_RGBA32);
    if surface = nil then begin
      SDL_Log('Kann kein Surface erzeugen !');
    end;
    r.x := 0;
    r.y := 0;
    r.w := size;
    r.h := size;
    SDL_FillSurfaceRect(surface, @r, $8888FFFF);


    Result := SDL_CreateTextureFromSurface(renderer, surface);
    if Result = nil then begin
      SDL_Log('Kann bmp nicht laden !');
    end;

    SDL_DestroySurface(surface);
  end;

  procedure SDLMain;
  var
    step: single;
    e: TSDL_Event;
    quit: boolean = False;
    rDest: TSDL_FRect;
    keyStat: PUInt8;
    time: extended;
    red, green, blue: single;
    IsCtrl: TSDL_bool;
  begin
    rDest.w := 100;
    rDest.h := 100;
    rDest.x := (Width - rDest.w) / 2;
    rDest.y := (heigt - rDest.h) / 2;
    while not quit do begin
      keyStat := SDL_GetKeyboardState(nil);
      if (keyStat[SDL_SCANCODE_LSHIFT] <> 0) or (keyStat[SDL_SCANCODE_RSHIFT] <> 0) then begin
        step := 0.1;
      end else begin
        step := 0.01;
      end;

      IsCtrl := (keyStat[SDL_SCANCODE_LCTRL] <> 0) or (keyStat[SDL_SCANCODE_RCTRL] <> 0);

      if keyStat[SDL_SCANCODE_RIGHT] <> 0 then begin
        if IsCtrl then begin
          rDest.x -= step;
          rDest.w += step * 2;
        end else begin
          rDest.x += step;
        end;
      end;
      if keyStat[SDL_SCANCODE_LEFT] <> 0 then begin
        if IsCtrl then begin
          if rDest.w > 1 then begin
            rDest.x += step;
            rDest.w -= step * 2;
          end;
        end else begin
          rDest.x -= step;
        end;
      end;
      if keyStat[SDL_SCANCODE_DOWN] <> 0 then begin
        if IsCtrl then begin
          rDest.y -= step;
          rDest.h += step * 2;
        end else begin
          rDest.y += step;
        end;
      end;
      if keyStat[SDL_SCANCODE_UP] <> 0 then begin
        if IsCtrl then begin
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

  window := SDL_CreateWindow('SDL3 Window', Width, heigt, SDL_WINDOW_RESIZABLE);
  if window = nil then begin
    SDL_Log('Kann kein SDL-Fenster erzeugen !');
  end;

  renderer := SDL_CreateRenderer(window, nil);

  if renderer = nil then begin
    SDL_Log('Kann kein SDL-Renderer erzeugen !');
  end;

  bitmapTex := CreateSurface;

  SDLMain;

  SDL_DestroyTexture(bitmapTex);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  SDL_Quit;
end.
