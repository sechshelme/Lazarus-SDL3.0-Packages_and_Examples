program project1;

uses
  SDL3;

type
  TAppstate = record
    window: PSDL_Window;
    renderer: PSDL_Renderer;
    camera: PSDL_Camera;
    texture: PSDL_Texture;
    Value: integer;
  end;
  PAppstate = ^TAppstate;


  function AppInit(appstate: Ppointer; argc: longint; argv: PPansichar): TSDL_AppResult; cdecl;
  var
    app: PAppstate = nil;
    devices: PSDL_CameraID;
    devcount: longint;
  begin
    app := SDL_malloc(SizeOf(TAppstate));
    app^ := Default(TAppstate);
    appstate^ := app;

    SDL_SetAppMetadata('Example Camera Read and Draw', '1.0', 'com.example.camera-read-and-draw');

    if not SDL_Init(SDL_INIT_VIDEO or SDL_INIT_CAMERA) then  begin
      SDL_Log('Kann SDL nict inizialisieren: %s', SDL_GetError);
      exit(SDL_APP_FAILURE);
    end;

    app^.window := SDL_CreateWindow('SDL3 Window', 640, 480, SDL_WINDOW_RESIZABLE);
    if app^.window = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_ERROR, 'Kann kein SDL-Fenster erzeugen !');
    end;

    app^.renderer := SDL_CreateRenderer(app^.window, nil);
    if app^.renderer = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_ERROR, 'Kann kein SDL-Renderer erzeugen !');
    end;

    devices := SDL_GetCameras(@devcount);
    if devices = nil then begin
      SDL_Log('Keine Kamera Device gefunden: %s', SDL_GetError);
      exit(SDL_APP_FAILURE);
    end else if devcount = 0 then begin
      SDL_Log('Keine Kamera gefunden');
      exit(SDL_APP_FAILURE);
    end;

    app^.camera := SDL_OpenCamera(devices[0], nil);
    SDL_free(devices);
    if app^.camera = nil then begin
      SDL_Log('Kann Kamera nicht öffnen: %s', SDL_GetError);
    end;

    app^.Value := 100;
    Result := SDL_APP_CONTINUE;
  end;

  function AppIterate(appstate: pointer): TSDL_AppResult; cdecl;
  var
    app: PAppstate absolute appstate;
    timestampNS: uint64;
    frame: PSDL_Surface;
  begin
    frame := SDL_AcquireCameraFrame(app^.camera, @timestampNS);

    if frame <> nil then begin
      if app^.texture = nil then begin
        SDL_SetWindowSize(app^.window, frame^.w, frame^.h);
        app^.texture := SDL_CreateTexture(app^.renderer, frame^.format, SDL_TEXTUREACCESS_STREAMING, frame^.w, frame^.h);
      end;

      if app^.texture <> nil then begin
        SDL_UpdateTexture(app^.texture,nil,frame^.pixels,frame^.pitch);
      end;

      SDL_ReleaseCameraFrame(app^.camera,frame);
    end;

    SDL_SetRenderDrawColor(app^.renderer, $99,$99,$99,$FF);
    SDL_RenderClear(app^.renderer);

    if app^.texture<>nil then SDL_RenderTexture(app^.renderer,app^.texture,nil,nil);

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
      SDL_EVENT_QUIT: begin
        Result := SDL_APP_SUCCESS;
      end;
    end;
  end;

  procedure AppQuit(appstate: pointer; Result: TSDL_AppResult); cdecl;
  var
    app: PAppstate absolute appstate;
  begin
    SDL_CloseCamera(app^.camera);
    SDL_DestroyRenderer(app^.renderer);
    SDL_DestroyTexture(app^.texture);
    SDL_DestroyWindow(app^.window);

    SDL_free(app);
    WriteLn('ende');

    SDL_Quit;
  end;

begin
  SDL_EnterAppMainCallbacks(argc, argv, @AppInit, @AppIterate, @AppEvent, @AppQuit);
end.
