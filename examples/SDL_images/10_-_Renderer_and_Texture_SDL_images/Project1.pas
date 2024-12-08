program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  ctypes,
  SDL3,
  SDL3_image;

var
  window: PSDL_Window;
  bitmapSurface: PSDL_Surface;
  renderer: PSDL_Renderer;
  bitmapTex: PSDL_Texture;

  function CreateSurface: PSDL_Surface;
  var
    r: TSDL_Rect;
  begin
    Result := IMG_Load('image.png');
    if Result = nil then begin
      SDL_Log('Kann keine Textur erzeugen !');
    end;
    r.x := 1;
    r.y := 1;
    r.w := 2;
    r.h := 2;
    SDL_FillSurfaceRect(Result, @r, $FFFF);
  end;

  procedure SDLMain;
  var
    step: single;
    e: TSDL_Event;
    quit: boolean = False;
    rSrc, rDest: TSDL_FRect;
    keyStat: PBoolean;
    IsCtrl: TSDL_bool;
  begin
    rDest.x := 0;
    rDest.y := 0;
    rDest.w := 100;
    rDest.h := 100;
    while not quit do begin
      keyStat := SDL_GetKeyboardState(nil);
      if (keyStat[SDL_SCANCODE_LSHIFT]) or (keyStat[SDL_SCANCODE_RSHIFT]) then begin
        step := 0.1;
      end else begin
        step := 0.01;
      end;

      IsCtrl := (keyStat[SDL_SCANCODE_LCTRL]) or (keyStat[SDL_SCANCODE_RCTRL]);

      if keyStat[SDL_SCANCODE_RIGHT] then begin
        if IsCtrl then begin
          rDest.x -= step;
          rDest.w += step * 2;
        end else begin
          rDest.x += step;
        end;
      end;
      if keyStat[SDL_SCANCODE_LEFT] then begin
        if IsCtrl then begin
          if rDest.w > 1 then begin
            rDest.x += step;
            rDest.w -= step * 2;
          end;
        end else begin
          rDest.x -= step;
        end;
      end;
      if keyStat[SDL_SCANCODE_DOWN] then begin
        if IsCtrl then begin
          rDest.y -= step;
          rDest.h += step * 2;
        end else begin
          rDest.y += step;
        end;
      end;
      if keyStat[SDL_SCANCODE_UP] then begin
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

      SDL_RenderClear(renderer);
      rSrc.x := 0;
      rSrc.y := 0;
      rSrc.w := 400;
      rSrc.h := 400;

      SDL_RenderTexture(renderer, bitmapTex, @rSrc, @rDest);
      SDL_RenderPresent(renderer);
    end;
  end;

begin
  SDL_init(SDL_INIT_VIDEO);
//  IMG_Init(IMG_INIT_JPG);

  window := SDL_CreateWindow('SDL3 Window', 800, 600, SDL_WINDOW_RESIZABLE);
  if window = nil then begin
    SDL_Log('Kann kein SDL-Fenster erzeugen !');
  end;

  renderer := SDL_CreateRenderer(window, nil);
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

//  IMG_Quit;
  SDL_Quit;
end.
