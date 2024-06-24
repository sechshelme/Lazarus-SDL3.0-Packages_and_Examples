program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  SDL3, SurfaceWindow;

var
  window, window2: PSDL_Window;
  renderer, renderer2: PSDL_Renderer;
  bitmapTex: PSDL_Texture;

  surWin:TSurfaceWindow;

  function CreateTexture: PSDL_Texture;
  var
    bitmapSurface: PSDL_Surface;
  begin
    bitmapSurface := SDL_LoadBMP('mauer.bmp');
    if bitmapSurface = nil then  begin
      SDL_Log('Kann keine Textur erzeugen !');
    end;

    Result := SDL_CreateTextureFromSurface(renderer, bitmapSurface);
    if Result = nil then begin
      SDL_Log('Kann Textur nicht laden ezeugen !');
    end;
    SDL_DestroySurface(bitmapSurface);
  end;

  procedure Window2Pos(ofs: integer);
  var
    x, y: longint;
  begin
    SDL_GetWindowPosition(window2, @x, @y);
    Inc(x, ofs);
    SDL_SetWindowPosition(window2, x, y);
  end;

  procedure Window2Resize(ofs: integer);
  var
    w, h: longint;
  begin
    SDL_GetWindowSize(window2, @w, @h);
    Inc(w, ofs);
    SDL_SetWindowSize(window2, w, h);
  end;

  procedure Windows2ShowPos(const ev: TSDL_Event);
  var
    win: PSDL_Window;
    x, y, w, h: longint;
  begin
    win := SDL_GetWindowFromID(ev.window.windowID);
    SDL_GetWindowPosition(win, @x, @y);
    SDL_GetWindowSize(win, @w, @h);
    SDL_Log('Left: %i  Right: %i,  Width: %i  Height: %i', x, y, w, h);
    SDL_RenderClear(renderer2);
    SDL_RenderPresent(renderer2);

  end;

  procedure SDLMain;
  var
    step, red, green, blue: single;
    e: TSDL_Event;
    quit: boolean = False;
    rSrc, rDest, r: TSDL_FRect;
    keyStat: PUInt8 = nil;
    IsShift, IsCtrl: TSDL_bool;
    time: extended;

  begin
    rDest.x := 0;
    rDest.y := 0;
    rDest.w := 100;
    rDest.h := 100;
    while not quit do begin
      keyStat := SDL_GetKeyboardState(nil);
      IsShift := (keyStat[SDL_SCANCODE_LSHIFT] <> 0) or (keyStat[SDL_SCANCODE_RSHIFT] <> 0);
      IsCtrl := (keyStat[SDL_SCANCODE_LCTRL] <> 0) or (keyStat[SDL_SCANCODE_RCTRL] <> 0);

      if IsShift then begin
        step := 0.1;
      end else begin
        step := 0.01;
      end;

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
        surWin.EventHandle(e);

        case e._type of
          SDL_EVENT_KEY_DOWN: begin
            case e.key.key of
              SDLK_ESCAPE: begin
                quit := True;
              end;
              SDLK_r: begin
                if IsCtrl then begin
                  Window2Resize(1);
                end else begin
                  Window2Pos(3);
                end;
              end;
              SDLK_l: begin
                if IsCtrl then begin
                  Window2Resize(-1);
                end else begin
                  Window2Pos(-3);
                end;
              end;
            end;
          end;
          SDL_EVENT_WINDOW_MOVED,
          SDL_EVENT_WINDOW_RESIZED: begin
            Windows2ShowPos(e);
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

      rSrc.items := [0, 0, 400, 400];
      SDL_RenderTexture(renderer, bitmapTex, @rSrc, @rDest);

      r.items := [200, 200, 64, 64];
      SDL_RenderTexture(renderer, bitmapTex, @rSrc, @r);

      SDL_RenderPresent(renderer);
    end;
  end;

begin
  SDL_init(SDL_INIT_VIDEO);

  surWin:=TSurfaceWindow.Create;;

  window := SDL_CreateWindow('SDL3 Window', 800, 600, SDL_WINDOW_RESIZABLE);
  if window = nil then begin
    SDL_Log('Kann kein SDL-Fenster erzeugen !');
  end;

  renderer := SDL_CreateRenderer(window, nil);
  if renderer = nil then begin
    SDL_Log('Kann kein SDL-Renderer erzeugen !');
  end;

  bitmapTex := CreateTexture;

  SDL_CreateWindowAndRenderer('Window 2', 320, 200, SDL_WINDOW_RESIZABLE, @window2, @renderer2);
  SDL_SetWindowPosition(window2, 50, 50);

  SDLMain;

  surWin.Free;

  SDL_DestroyTexture(bitmapTex);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_DestroyRenderer(renderer2);
  SDL_DestroyWindow(window2);

  SDL_Quit;
end.
