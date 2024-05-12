program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  SDL3;

var
  window: PSDL_Window;
  bitmapSurface: PSDL_Surface;
  renderer: PSDL_Renderer;
  bitmapTex: PSDL_Texture;

  function CreateSurface: PSDL_Surface;
  var
    r: TSDL_Rect;
  begin
    Result := SDL_LoadBMP('mauer.bmp');
    if Result = nil then
    begin
      SDL_Log('Kann keine textur erzeugen !');
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
    keyStat: PUInt8;
    IsCtrl: TSDL_bool;
    format: DWord;
    x, y: longint;
    access: Plongint;
  begin
    rDest.x := 0;
    rDest.y := 0;
    rDest.w := 100;
    rDest.h := 100;
    while not quit do
    begin
      keyStat := SDL_GetKeyboardState(nil);
      if (keyStat[SDL_SCANCODE_LSHIFT] <> 0) or (keyStat[SDL_SCANCODE_RSHIFT] <> 0) then
      begin
        step := 0.1;
      end
      else
      begin
        step := 0.01;
      end;

      IsCtrl := (keyStat[SDL_SCANCODE_LCTRL] <> 0) or (keyStat[SDL_SCANCODE_RCTRL] <> 0);

      if keyStat[SDL_SCANCODE_RIGHT] <> 0 then
      begin
        if IsCtrl then
        begin
          rDest.x -= step;
          rDest.w += step * 2;
        end
        else
        begin
          rDest.x += step;
        end;
      end;
      if keyStat[SDL_SCANCODE_LEFT] <> 0 then
      begin
        if IsCtrl then
        begin
          if rDest.w > 1 then
          begin
            rDest.x += step;
            rDest.w -= step * 2;
          end;
        end
        else
        begin
          rDest.x -= step;
        end;
      end;
      if keyStat[SDL_SCANCODE_DOWN] <> 0 then
      begin
        if IsCtrl then
        begin
          rDest.y -= step;
          rDest.h += step * 2;
        end
        else
        begin
          rDest.y += step;
        end;
      end;
      if keyStat[SDL_SCANCODE_UP] <> 0 then
      begin
        if IsCtrl then
        begin
          if rDest.h > 1 then
          begin
            rDest.y += step;
            rDest.h -= step * 2;
          end;
        end
        else
        begin
          rDest.y -= step;
        end;
      end;

      if rDest.h < 1 then
      begin
        rDest.h := 1;
      end;


      while SDL_PollEvent(@e) do
      begin
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

      SDL_RenderClear(renderer);
      //   SDL_RenderTexture(renderer, bitmapTex, nil, @distrect);
      rSrc.x := 0;
      rSrc.y := 0;
      rSrc.w := 400;
      rSrc.h := 400;

      WriteLn(SDL_QueryTexture(bitmapTex, @format, access, @x, @y));
      WriteLn(x,'   ',y,'    ',format);
      if access = nil then WriteLn('nil');
      access[0] := $FFFFFFFF;
       access[1] := $FFFFFFFF;
       access[2] := $FFFFFFFF;
       access[3] := $FFFFFFFF;

      SDL_RenderTexture(renderer, bitmapTex, @rSrc, @rDest);
      SDL_RenderPresent(renderer);
    end;
  end;

begin
  SDL_init(SDL_INIT_VIDEO);

  window := SDL_CreateWindow('SDL3 Window', 800, 600, SDL_WINDOW_RESIZABLE);
  if window = nil then
  begin
    SDL_Log('Kann kein SDL-Fenster erzeugen !');
  end;

  renderer := SDL_CreateRenderer(window, nil, SDL_RENDERER_ACCELERATED);
  if renderer = nil then
  begin
    SDL_Log('Kann kein SDL-Renderer erzeugen !');
  end;

  bitmapSurface := CreateSurface;

  bitmapTex := SDL_CreateTextureFromSurface(renderer, bitmapSurface);
  if bitmapSurface = nil then
  begin
    SDL_Log('Kann bmp nicht laden !');
  end;

  SDL_DestroySurface(bitmapSurface);

  SDLMain;

  SDL_DestroyTexture(bitmapTex);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  SDL_Quit;
end.
