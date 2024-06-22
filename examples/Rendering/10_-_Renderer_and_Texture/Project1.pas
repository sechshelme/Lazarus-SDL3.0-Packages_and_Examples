program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  SDL3;

var
  window: PSDL_Window;
  renderer: PSDL_Renderer;
  bitmapTex: PSDL_Texture;

  function CreateTexture: PSDL_Texture;
  var
    bitmapSurface: PSDL_Surface;
  begin
    bitmapSurface := SDL_LoadBMP('mauer.bmp');
    if bitmapSurface = nil then  begin
      SDL_Log('Kann keine textur erzeugen !');
    end;

    Result := SDL_CreateTextureFromSurface(renderer, bitmapSurface);
    if Result = nil then begin
      SDL_Log('Kann Textur nicht laden ezeugen !');
    end;
    SDL_DestroySurface(bitmapSurface);
  end;

  procedure SDLMain;
  var
    step: single;
    e: TSDL_Event;
    quit: boolean = False;
    rSrc, rDest: TSDL_FRect;
    keyStat: PUInt8;
    IsCtrl: TSDL_bool;
  begin
    rDest.x := 0;
    rDest.y := 0;
    rDest.w := 100;
    rDest.h := 100;
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

      SDL_RenderClear(renderer);

      rSrc.items:=[0,0 ,1050,1000];
      SDL_SetRenderViewport(renderer, @rSrc);

      //   SDL_RenderTexture(renderer, bitmapTex, nil, @distrect);
      rSrc.x := 0;
      rSrc.y := 0;
      rSrc.w := 400;
      rSrc.h := 400;

      //WriteLn(SDL_QueryTexture(bitmapTex, @format, access, @x, @y));
      //WriteLn(x,'   ',y,'    ',format);
      //if access = nil then WriteLn('nil');
      //access[0] := $FFFFFFFF;
      // access[1] := $FFFFFFFF;
      // access[2] := $FFFFFFFF;
      // access[3] := $FFFFFFFF;

      SDL_RenderTexture(renderer, bitmapTex, @rSrc, @rDest);
      SDL_RenderPresent(renderer);
    end;
  end;

begin
  SDL_init(SDL_INIT_VIDEO);

  window := SDL_CreateWindow('SDL3 Window', 800, 600, SDL_WINDOW_RESIZABLE);
  if window = nil then begin
    SDL_Log('Kann kein SDL-Fenster erzeugen !');
  end;

  renderer := SDL_CreateRenderer(window, nil);
  if renderer = nil then begin
    SDL_Log('Kann kein SDL-Renderer erzeugen !');
  end;

  bitmapTex := CreateTexture;

  SDLMain;

  SDL_DestroyTexture(bitmapTex);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  SDL_Quit;
end.
