program project1;

uses
  SDL3;

type
  TAppstate = record
    window: PSDL_Window;
    renderer: PSDL_Renderer;
    Value: integer;
  end;
  PAppstate = ^TAppstate;


  function AppInit(appstate: Ppointer; argc: longint; argv: PPansichar): TSDL_AppResult; cdecl;
  var
    app: PAppstate = nil;
  begin
    SDL_Init(SDL_INIT_VIDEO);

    app := SDL_malloc(SizeOf(TAppstate));
    appstate^ := app;
    app^.window := SDL_CreateWindow('SDL3 Window', 640, 480, SDL_WINDOW_RESIZABLE);
    if app^.window = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_ERROR, 'Kann kein SDL-Fenster erzeugen !');
    end;

    app^.renderer := SDL_CreateRenderer(app^.window, nil);
    if app^.renderer = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_ERROR, 'Kann kein SDL-Renderer erzeugen !');
    end;

    app^.Value := 100;
    Result := SDL_APP_CONTINUE;
  end;

  function AppIterate(appstate: pointer): TSDL_AppResult; cdecl;
  var
    app: PAppstate absolute appstate;
    rect: TSDL_FRect;
  begin
    SDL_SetRenderDrawColorFloat(app^.renderer, 0, 0, 0, SDL_ALPHA_OPAQUE);

    rect.x := SDL_rand(640) - 320;
    rect.y := SDL_rand(480) - 240;
    rect.w := SDL_rand(640);
    rect.h := SDL_rand(480);

    SDL_SetRenderDrawColorFloat(app^.renderer, Random, Random, Random, SDL_ALPHA_OPAQUE);
    SDL_RenderRect(app^.renderer, @rect);

    SDL_RenderPresent(app^.renderer);
    Result := SDL_APP_CONTINUE;
  end;

  function AppEvent(appstate: pointer; event: PSDL_Event): TSDL_AppResult; cdecl;
  var
    app: PAppstate absolute appstate;
  begin
    Result := SDL_APP_CONTINUE;
    case event^._type of
      SDL_EVENT_KEY_DOWN: begin
        case event^.key.key of
          SDLK_ESCAPE: begin
            Result := SDL_APP_SUCCESS;
          end;
          SDLK_SPACE: begin
            Inc(app^.Value);
//            SDL_LogError(0,'value: %i', app^.Value);
            SDL_Log('value: %i', app^.Value);
          end;
        end;
      end;
    end;
  end;

  procedure AppQuit(appstate: pointer; Result: TSDL_AppResult); cdecl;
  var
    app: PAppstate absolute appstate;
  begin
    SDL_DestroyRenderer(app^.renderer);
    SDL_DestroyWindow(app^.window);

    SDL_free(app);
    WriteLn('ende');

    SDL_Quit;
  end;

begin
  SDL_EnterAppMainCallbacks(argc, argv, @AppInit, @AppIterate, @AppEvent, @AppQuit);
end.
