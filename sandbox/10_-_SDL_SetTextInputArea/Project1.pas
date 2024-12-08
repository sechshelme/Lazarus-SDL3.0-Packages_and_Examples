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

  procedure Run;
  var
    event: TSDL_Event;
    quit: boolean = False;
    time: single;
    red, green, blue: byte;
    keyStat: PUInt8;
    cnt: integer = 0;
  begin
    while not quit do begin
      keyStat := SDL_GetKeyboardState(nil);
      if keyStat[SDL_SCANCODE_SPACE] <> 0 then begin
        SDL_Log('Space is pressed   %i',cnt);
        inc( cnt);
      end;
      if keyStat[SDL_SCANCODE_LEFT] <> 0 then begin
        SDL_Log('Left is pressed   %i',cnt);
        inc( cnt);
      end;
      if keyStat[SDL_SCANCODE_RIGHT] <> 0 then begin
        SDL_Log('Right is pressed   %i',cnt);
        inc( cnt);
      end;

      while SDL_PollEvent(@event) do begin
        case event._type of
          SDL_EVENT_KEY_DOWN: begin
            SDL_Log('key: %i', event.key.key); // neu

            case event.key.key of
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

      WriteLn('SDL_SetTextInputArea: ',  SDL_SetTextInputArea(window, nil, 0));

      SDL_RenderPresent(renderer);
    end;
  end;

// https://wiki.libsdl.org/SDL3/SDL_SetTextInputArea

begin
  SDL_init(SDL_INIT_VIDEO);

  window := SDL_CreateWindow('SDL3 Window', 320, 200, SDL_WINDOW_RESIZABLE);
  if window = nil then begin
    SDLFail('Kann kein SDL-Fenster erzeugen !');
  end;
  renderer := SDL_CreateRenderer(window, nil);
  if renderer = nil then begin
    SDLFail('Kann kein SDL-Renderer erzeugen !');
  end;

  SDL_ShowWindow(window);

  SDL_GetWindowSize(window, @Width, @Height);
  SDL_GetWindowSizeInPixels(window, @bbwidth, @bbheight);
  SDL_LogCritical(0, 'Window size: %ix%i', bbwidth, bbheight);
  SDL_LogCritical(0, 'blabla');
  SDL_Log('Window size: %ix%i', bbwidth, bbheight);
  SDL_Log('log');
  SDL_LogWarn(0, 'warn');
  WriteLn('Window size: ', bbwidth, 'x', bbheight);

  if Width <> bbwidth then  begin
    SDL_Log('This is a highdpi environment.');
  end;

//  SDL_SetTextInputRect(nil);

  Run;

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  SDL_Quit;
  SDL_Log('Application quit successfully!');
end.
