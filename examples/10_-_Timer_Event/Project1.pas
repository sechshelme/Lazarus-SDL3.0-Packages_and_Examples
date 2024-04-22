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
    keyStat: PUInt8;
    red, green, blue: int64;
    time: extended;
  begin
    rDest.w := 100;
    rDest.h := 100;
    rDest.x := (Width - rDest.w) / 2;
    rDest.y := (heigt - rDest.h) / 2;
    while not quit do begin
      keyStat := SDL_GetKeyboardState(nil);
      if keyStat[SDL_SCANCODE_SPACE] <> 0 then begin
      end;

      if keyStat[SDL_SCANCODE_RIGHT] <> 0 then begin
        if keyStat[SDL_SCANCODE_LSHIFT] <> 0 then begin
          rDest.x -= step;
          rDest.w += step * 2;
        end else begin
          rDest.x += step;
        end;
      end;
      if keyStat[SDL_SCANCODE_LEFT] <> 0 then begin
        if keyStat[SDL_SCANCODE_LSHIFT] <> 0 then begin
          if rDest.w > 1 then begin
            rDest.x += step;
            rDest.w -= step * 2;
          end;
        end else begin
          rDest.x -= step;
        end;
      end;
      if keyStat[SDL_SCANCODE_DOWN] <> 0 then begin
        if keyStat[SDL_SCANCODE_LSHIFT] <> 0 then begin
          rDest.y -= step;
          rDest.h += step * 2;
        end else begin
          rDest.y += step;
        end;
      end;
      if keyStat[SDL_SCANCODE_UP] <> 0 then begin
        if keyStat[SDL_SCANCODE_LSHIFT] <> 0 then begin
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
          SDL_EVENT_USER: begin
            SDL_Log('user');
            rDest := PSDL_FRect(e.user.data1)^;

          end;
        end;
      end;

      time := SDL_GetTicks / 1000;
      red := Trunc((SDL_sinf(time) + 1) / 2.0 * 255);
      green := Trunc((SDL_sinf(time / 2) + 1) / 2.0 * 255);
      blue := Trunc((SDL_sinf(time / 3) + 1) / 2.0 * 255);

      SDL_SetRenderDrawColor(renderer, red, green, blue, SDL_ALPHA_OPAQUE);
      SDL_RenderClear(renderer);

      SDL_RenderTexture(renderer, bitmapTex, nil, @rDest);
      SDL_RenderPresent(renderer);
    end;
  end;

  function PointMoveProc(interval: uint32; param: pointer): uint32; cdecl;
  var
    event: TSDL_Event;
    userevent: TSDL_UserEvent;
    rect: PSDL_FRect absolute param;
  begin
    rect^.x += 0.1;
    SDL_Log('Timer');

    userevent._type := SDL_EVENT_USER;
    userevent.code := 0;
    userevent.data1 := param;
    userevent.data2 := nil;

    event.type_ := SDL_EVENT_USER;
    event.user := userevent;

    SDL_PushEvent(@event);
    Result := interval;
  end;

  procedure main;
  var
    Pointrect: TSDL_FRect;

  begin
    Pointrect.x := 100;
    Pointrect.y := 100;
    Pointrect.w := 100;
    Pointrect.h := 100;

    SDL_init(SDL_INIT_VIDEO);

    SDL_AddTimer(10, @PointMoveProc, @Pointrect);

    window := SDL_CreateWindow('SDL3 Window', Width, heigt, SDL_WINDOW_RESIZABLE);
    if window = nil then begin
      SDL_Log('Kann kein SDL-Fenster erzeugen !');
    end;

    renderer := SDL_CreateRenderer(window, nil, SDL_RENDERER_ACCELERATED);
    if renderer = nil then begin
      SDL_Log('Kann kein SDL-Renderer erzeugen !');
    end;

    bitmapSurface := CreateSurface;

    bitmapTex := SDL_CreateTextureFromSurface(renderer, bitmapSurface);
    if bitmapSurface = nil then begin
      SDL_Log('Kann bmp nicht laden !');
    end;

    SDL_DestroySurface(bitmapSurface);

    SDLMain;

    SDL_DestroyTexture(bitmapTex);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);

    SDL_Quit;
  end;

begin
  main;
end.
