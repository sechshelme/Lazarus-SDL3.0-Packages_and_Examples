program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  ctypes,
  SDL3;

var
  window: PSDL_Window;
  bitmapSurface: PSDL_Surface;
  dstrect: TSDL_Rect = (x: 100; y: 100; w: 200; h: 200);
  renderer: PSDL_Renderer;
  Width, Height, bbwidth, bbheight: longint;

  procedure SDLFail(const err: string);
  begin
    SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, PChar('Fehler: ' + err));
    Halt(1);
  end;

  procedure SDLMain;
  var
    e: TSDL_Event;
    quit: boolean = False;
    time: single;
    red, green, blue: byte;
  begin
    while not quit do begin
      while SDL_PollEvent(@e) <> 0 do begin
        WriteLn('event: ', e.type_); // neu
        case e.type_ of
          SDL_EVENT_KEY_DOWN: begin
            WriteLn('key: ', e.key.keysym.sym); // neu
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


      time := SDL_GetTicks / 1000;
      red := Trunc((SDL_sinf(time) + 1) / 2.0 * 255);
      green := Trunc((SDL_sinf(time / 2) + 1) / 2.0 * 255);
      blue := Trunc((SDL_sinf(time / 3) + 1) / 2.0 * 255);

      SDL_SetRenderDrawColor(renderer, red, green, blue, SDL_ALPHA_OPAQUE);

      SDL_RenderClear(renderer);
      //    SDL_RenderTexture(renderer, bitmapTex, nil, nil);
      SDL_RenderPresent(renderer);
    end;
  end;

begin
  SDL_init(SDL_INIT_VIDEO);

  window := SDL_CreateWindow('SDL3 Window', 320, 200, SDL_WINDOW_RESIZABLE);
  if window = nil then begin
    SDLFail('Kann kein SDL-Fenster erzeugen !');
  end;
  renderer := SDL_CreateRenderer(window, nil, SDL_RENDERER_ACCELERATED);
  if renderer = nil then begin
    SDLFail('Kann kein SDL-Renderer erzeugen !');
  end;

  SDL_ShowWindow(window);

  SDL_GetWindowSize(window, @Width, @Height);
  SDL_GetWindowSizeInPixels(window, @bbwidth, @bbheight);
  SDL_LogCritical(0, 'Window size: %ix%i', bbwidth, bbheight);
  SDL_LogCritical(0, 'blabla');
  //  SDL_Log('Window size: %ix%i',bbwidth,bbheight);
  SDL_Log('log');
  SDL_LogWarn(0, 'warn');
  WriteLn('Window size: ', bbwidth, 'x', bbheight);

  if Width <> bbwidth then  begin
    SDL_Log('This is a highdpi environment.');
  end;


  SDLMain;

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  SDL_Quit;
  SDL_Log('Application quit successfully!');
end.
