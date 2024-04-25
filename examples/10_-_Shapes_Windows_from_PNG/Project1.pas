program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  ctypes,
  SDL3, SDL_image;

var
  window: PSDL_Window;
  renderer: PSDL_Renderer;

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
    blue, green, red: Single;
    time: Extended;

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
        case event.type_ of
          SDL_EVENT_KEY_DOWN: begin
            SDL_Log('key: %i', event.key.keysym.sym); // neu

            case event.key.keysym.sym of
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

      time := SDL_GetTicks / 1000;
      red := (SDL_sinf(time) + 1) / 2.0;
      green := (SDL_sinf(time / 2) + 1) / 2.0;
      blue := (SDL_sinf(time / 3) + 1) / 2.0;

      SDL_SetRenderDrawColorFloat(renderer, red, green, blue, SDL_ALPHA_OPAQUE);
      SDL_RenderClear(renderer);
      SDL_RenderPresent(renderer);

      SDL_Log('Tick: %i',SDL_GetTicks-TimeStart);
    end;
  end;

  procedure main;
  var
    windowFlags, initFlags: Integer;
    shapeSurface: PSDL_Surface;
    r: TSDL_Rect;
  begin
    initFlags := SDL_INIT_VIDEO or SDL_INIT_AUDIO or SDL_INIT_EVENTS;
    SDL_init(initFlags);
    IMG_Init(IMG_INIT_PNG);

    shapeSurface:=IMG_Load('sdl.png');
    if shapeSurface = nil then begin
      SDLFail('Kann Sahbe nicht laden !');
    end;

    shapeSurface:=SDL_CreateSurface(800,600,SDL_PIXELFORMAT_RGBA8888);
    r.x:=0;
    r.y:=0;
    r.w:=400;
    r.h:=300;
    SDL_FillSurfaceRect(shapeSurface, @r, $FFFFFFFF);
    r.x:=400;
    r.y:=300;
    SDL_FillSurfaceRect(shapeSurface, @r, $FFFFFFFF);

    windowFlags:=SDL_WINDOW_HIDDEN or SDL_WINDOW_TRANSPARENT or SDL_WINDOW_BORDERLESS;
    window := SDL_CreateWindow('SDL3 Window', shapeSurface^.w, shapeSurface^.h, windowFlags);
    if window = nil then begin
      SDLFail('Kann kein SDL-Fenster erzeugen !');
    end;

    renderer := SDL_CreateRenderer(window, nil, SDL_RENDERER_PRESENTVSYNC);
//    renderer := SDL_CreateRenderer(window, nil, SDL_RENDERER_SOFTWARE);
    if renderer = nil then begin
      SDLFail('Kann kein SDL-Renderer erzeugen !');
    end;

    SDL_SetWindowShape(window,shapeSurface);
    SDL_ShowWindow(window);

    Run;

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);

    IMG_Quit;
    SDL_Quit;
    SDL_Log('Application quit successfully!');
  end;

begin
  main;
end.
