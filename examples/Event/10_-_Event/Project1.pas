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
    keyStat: PUInt8;
    TimeStart: UInt64;

  begin
    while not quit do begin
      TimeStart:=SDL_GetTicks;
      keyStat := SDL_GetKeyboardState(nil);
      if keyStat[SDL_SCANCODE_SPACE] <> 0 then begin
        SDL_Log('Space is pressed');
      end;
      if keyStat[SDL_SCANCODE_LEFT] <> 0 then begin
        SDL_Log('Left is pressed');
      end;

      while SDL_PollEvent(@event) do begin
        case event._type of
          SDL_EVENT_KEY_DOWN: begin
            SDL_Log('key: %i', event.key.key);

            case event.key.key of
              SDLK_ESCAPE: begin
                quit := True;
              end;
              SDLK_m: begin
                //                SwitchMouseButton;
              end;

            end;
          end;
          SDL_EVENT_MOUSE_BUTTON_DOWN: begin
            SDL_Log('Mouse down');
          end;
          SDL_EVENT_MOUSE_BUTTON_UP: begin
            SDL_Log('Mouse up');
          end;
          SDL_EVENT_QUIT: begin
            quit := True;
          end;
        end;
      end;

      SDL_SetRenderDrawColorFloat(renderer, Random, Random, Random, SDL_ALPHA_OPAQUE);

      SDL_RenderClear(renderer);
      SDL_RenderPresent(renderer);

      SDL_Log('Tick: %i',SDL_GetTicks-TimeStart);
    end;
  end;

  procedure main;
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

    Run;

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);

    SDL_Quit;
    SDL_Log('Application quit successfully!');
  end;

begin
  main;
end.
