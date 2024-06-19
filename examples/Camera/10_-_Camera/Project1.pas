program Project1;

// https://github.com/libsdl-org/SDL/issues/9496

uses
  SDL3,
  ctypes;

var
  renderer: PSDL_Renderer;
  camera: PSDL_Camera;
  spec: TSDL_CameraSpec;
  texture: PSDL_Texture;
  frame_current: PSDL_Surface = nil;
  texture_updated: TSDL_bool = SDL_FALSE;

  procedure Init;
  var
    devCount: longint;
    devices: PSDL_CameraDeviceID;
    i: integer;
    Name: PChar;
    win: PSDL_Window;

    devid: TSDL_CameraDeviceID;

    posstr: PChar = '';
    position: TSDL_CameraPosition;
    pspec: TSDL_CameraSpec;


  begin
    SDL_Init(SDL_INIT_VIDEO or SDL_INIT_CAMERA);
    win := SDL_CreateWindow('Camera', 640, 480, SDL_WINDOW_RESIZABLE);
    if win = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'Konnte Fenster nicht erzeugen !');
      Halt(-1);
    end;

    renderer := SDL_CreateRenderer(win, nil);
    if renderer = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'Kann kein SDL-Renderer erzeugen !');
      Halt(-1);
    end;

    devices := SDL_GetCameraDevices(@devCount);
    if devices = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'SDL_GetCameraDevices failed: %s', SDL_GetError());
      Halt(-1);
    end;

    for i := 0 to devCount - 1 do begin
      devid := devices[i];
      Name := SDL_GetCameraDeviceName(devid);
      position := SDL_GetCameraDevicePosition(devid);
      case position of
        SDL_CAMERA_POSITION_FRONT_FACING: begin
          posstr := '[front-facing] ';
        end;
        SDL_CAMERA_POSITION_BACK_FACING: begin
          posstr := '[back-facing] ';
        end;
        else begin
          posstr := '[unknow]';
        end;
      end;
      SDL_Log('  - Camera #%d: %s %s', i, posstr, Name);
      SDL_free(Name);
    end;

    devid := devices[0];
    SDL_free(devices);
    if devid = 0 then begin

      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'No cameras available ?');
      Halt(-1);
    end;

    //spec.format := SDL_PIXELFORMAT_UYVY;
    //spec.interval_numerator := 1;
    //spec.interval_denominator := 30;
    //spec.Width := 640;
    //spec.Height := 480;
    //camera := SDL_OpenCameraDevice(devid, @pspec);

    camera := SDL_OpenCameraDevice(devid, nil);
    SDL_Log('w: %i   h: %i', pspec.Width, pspec.Height);

    if camera = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'Failed to open camera device: %s', SDL_GetError());
      Halt(-1);
    end;
  end;

  procedure Render;
  var
    tw,th:cfloat;
    win_w, win_h: cint;
    rect: TSDL_FRect;
    timestampNS: TUint64;
    frame_next: PSDL_Surface;
  begin
    SDL_SetRenderDrawColor(renderer, $99, $99, $9, 255);
    SDL_RenderClear(renderer);

    if texture <> nil then  begin
      if camera <> nil then begin
        frame_next := SDL_AcquireCameraFrame(camera, @timestampNS);
      end else begin
        frame_next := nil;
      end;

      if frame_next <> nil then begin
        if frame_current <> nil then begin
          if SDL_ReleaseCameraFrame(camera, frame_current) < 0 then  begin
            SDL_Log('err SDL_ReleaseCameraFrame: %s', SDL_GetError());
          end;
        end;
        frame_current := frame_next;
        texture_updated := SDL_FALSE;
      end;

      if (frame_current <> nil) and (texture_updated = SDL_FALSE) then begin
        SDL_UpdateTexture(texture, nil, frame_current^.pixels, frame_current^.pitch);
        texture_updated := SDL_TRUE;
      end;

      SDL_GetTextureSize(texture, @tw, @th);
//      SDL_QueryTexture(texture, nil, nil, @tw, @th);
      SDL_GetRenderOutputSize(renderer, @win_w, @win_h);

      rect.x := (win_w - tw) / 2;
      rect.y := (win_h - th) / 2;
      rect.w := tw;
      rect.h := th;

      SDL_RenderTexture(renderer, texture, nil, @rect);
    end;

    SDL_RenderPresent(renderer);
  end;

  procedure Event;
  var
    quit: boolean = False;
    e: TSDL_Event;
    sym: TSDL_KeyCode;
  begin
    while not quit do begin
      while SDL_PollEvent(@e) do begin
        case e.type_ of
          SDL_EVENT_KEY_DOWN: begin
            sym := e.key.keysym.sym;
            case sym of
              SDLK_ESCAPE, SDLK_AC_BACK: begin
                quit := True;
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

            texture := SDL_CreateTexture(renderer, spec.format, SDL_TEXTUREACCESS_STATIC, spec.Width, spec.Height);
            if texture = nil then begin
              SDL_Log('Couldn''t create texture: %s', SDL_GetError());
              Halt(-1);
            end;
          end;
        end;
      end;

      Render;
    end;
  end;

begin
  Init;
  Event;
  SDL_Quit;
end.
