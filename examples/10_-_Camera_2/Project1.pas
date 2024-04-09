program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  SDL3,
  ctypes;

var
  renderer: PSDL_Renderer;
  camera: PSDL_Camera;
  spec: TSDL_CameraSpec;
  front_camera: TSDL_CameraDeviceID = 0;
  back_camera: TSDL_CameraDeviceID = 0;

  procedure AppInit;
  var
    devCount: longint;
    devices: PSDL_CameraDeviceID;
    i, j: integer;
    Name: PChar;
    win: PSDL_Window;


    device, devid: TSDL_CameraDeviceID;

    posstr: PChar = '';
    position: TSDL_CameraPosition;
    pspec: PSDL_CameraSpec = nil;


  begin

    SDL_Init(SDL_INIT_VIDEO or SDL_INIT_CAMERA);
    win := SDL_CreateWindow('Camera', 640, 480, SDL_WINDOW_RESIZABLE);
    if win = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'Konnte Fenster nicht erzeugen !');
    end;

    renderer := SDL_CreateRenderer(win, nil, SDL_RENDERER_ACCELERATED);
    if renderer = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'Kann kein SDL-Renderer erzeugen !');
    end;

    //WriteLn('Fensterpos: ',  SDL_SetWindowPosition(win,  SDL_WINDOWPOS_CENTERED,  SDL_WINDOWPOS_CENTERED));
    //SDL_SyncWindow(win);
    //  SDL_LogSetAllPriority(SDL_LOG_CATEGORY_APPLICATION or SDL_LOG_PRIORITY_INFO);

    //  winSurface := SDL_GetWindowSurface(win);
    //  SDL_SetcoSurfaceColorMod(winSurface, $88, $FF, $88);
    //
    //  pCameraDriverCount := SDL_GetNumCameraDrivers;
    //  WriteLn('Camera driver count: ', pCameraDriverCount);
    //  for i := 0 to pCameraDriverCount - 1 do begin
    //    WriteLn('  ', i, ':  Drivername: %s', SDL_GetCameraDriver(i));
    //
    //  end;
    //  WriteLn();
    //

    devices := SDL_GetCameraDevices(@devCount);
    if devices = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'SDL_GetCameraDevices failed: %s', SDL_GetError());
      Halt(-1);
    end;


    //  WriteLn('Camera count: ', devCount);
    for i := 0 to devCount - 1 do begin
      device := devices[i];
      Name := SDL_GetCameraDeviceName(device);
      position := SDL_GetCameraDevicePosition(device);
      if position = SDL_CAMERA_POSITION_FRONT_FACING then begin
        front_camera := device;
        posstr := '[front-facing] ';
      end else if position = SDL_CAMERA_POSITION_BACK_FACING then begin
        back_camera := device;
        posstr := '[back-facing] ';
      end else begin
        //      back_camera := device;
        posstr := 'unbekannt ';
      end;
      SDL_Log('  - Camera #%d: --- %s --- %s', i, posstr, Name);
      SDL_free(Name);
      //
      //
      //
      //    pName := SDL_GetCameraDeviceName(devices[i]);
      //    WriteLn('  ', i, ': ', pName);
      //
      //    spec := SDL_GetCameraDeviceSupportedFormats(devices[i], @pSpecCount);
      //    for j := 0 to pSpecCount - 1 do begin
      //      WriteLn('    spec w: ', spec[j].Width, ' h: ', spec[j].Height, ' f: ', spec[j].format, ' inum: ', spec[j].interval_numerator, ' ideno: ', spec[j].interval_denominator);
      //    end;
    end;

    if front_camera <> 0 then begin
      devid := front_camera;
    end else begin
      devid := devices[0];
    end;
    SDL_free(devices);
    if devid = 0 then begin
      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'No cameras available ?');
      Halt(-1);
    end;

    camera := SDL_OpenCameraDevice(devid, pspec);
    if camera = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'Failed to open camera device: %s', SDL_GetError());
      Halt(-1);
    end;
  end;

  function AppIterate: cint;
  begin
    SDL_SetRenderDrawColor(renderer, $99, $99, $9, 255);
    SDL_RenderClear(renderer);

    SDL_RenderPresent(renderer);
    Result := 0;
  end;

  function FlipCamera: cint;
  var
    last_flip: TUint64 = 0;
    current, nextcam: TSDL_CameraDeviceID;
  begin
    if SDL_GetTicks - last_flip < 3000 then begin
      exit(0);
    end;
    if camera <> nil then begin
      current := SDL_GetCameraInstanceID(camera);
      nextcam := 0;
      if current = front_camera then begin
        nextcam := back_camera;
      end else if current = back_camera then begin
        nextcam := front_camera;
      end;

      if nextcam <> 0 then begin
        SDL_Log('Flip camera');
      end;

      // ====================
    end;
  end;

  procedure AppEvent;
  var
    quit: boolean = False;
    e: TSDL_Event;
    sym: TSDL_KeyCode;
    texture: PSDL_Texture;
  begin
    while not quit do begin
      while SDL_PollEvent(@e) <> 0 do begin
        //        WriteLn('event: ', e.type_); // neu
        case e.type_ of
          SDL_EVENT_KEY_DOWN: begin
            sym := e.key.keysym.sym;
            case sym of
              SDLK_ESCAPE, SDLK_AC_BACK: begin
                quit := True;
              end;
              SDLK_SPACE: begin
                FlipCamera;
              end;
            end;
          end;
          SDL_EVENT_QUIT: begin
            quit := True;
          end;
          SDL_EVENT_CAMERA_DEVICE_APPROVED: begin
            SDL_Log('Camera approved !');
            if SDL_GetCameraFormat(camera, @spec) < 0 then  begin
              SDL_Log('Couldn''t get camera spec: %s', SDL_GetError());
              Halt(-1);
            end;

            //            sdl_assert
            texture := SDL_CreateTexture(renderer, spec.format, SDL_TEXTUREACCESS_STATIC, spec.Width, spec.Height);
            if texture = nil then begin
              SDL_Log('Couldn''t create texture: %s', SDL_GetError());
              Halt(-1);
            end;

          end;
        end;
      end;

      AppIterate;
    end;
  end;

begin
  AppInit;

  AppEvent;

  //  SDL_Delay(3000);

end.
