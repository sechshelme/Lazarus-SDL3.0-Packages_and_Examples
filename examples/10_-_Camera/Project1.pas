program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  SDL3,
  ctypes;

var
  nDevices, pSpecCount, pCameraDriverCount: longint;
  pDeviceList: PSDL_CameraDeviceID;
  i, j: integer;
  pName: PChar;
  pSpec: PSDL_CameraSpec;
  pCamera: PSDL_Camera;
  win: PSDL_Window;
  winSurface, camSurface: PSDL_Surface;
  quit: boolean = False;
  e: TSDL_Event;
  timeStamp: uint64;
begin
  SDL_Init(SDL_INIT_VIDEO or SDL_INIT_CAMERA);
  win := SDL_CreateWindow('Camera', 640, 480, SDL_WINDOW_RESIZABLE);
  if win = nil then begin
    SDL_LogError(0, 'Konnte Fenster nicht erzeugen !');
  end;
WriteLn('Fensterpos: ',  SDL_SetWindowPosition(win,  SDL_WINDOWPOS_CENTERED,  SDL_WINDOWPOS_CENTERED));
SDL_SyncWindow(win);
  winSurface := SDL_GetWindowSurface(win);

  pCameraDriverCount := SDL_GetNumCameraDrivers;
  WriteLn('Camera driver count: ', pCameraDriverCount);
  for i := 0 to pCameraDriverCount - 1 do begin
    WriteLn('  ', i, ':  Drivername: %s', SDL_GetCameraDriver(i));

  end;
  WriteLn();


  pDeviceList := SDL_GetCameraDevices(@nDevices);
  WriteLn('Camera count: ', nDevices);
  for i := 0 to nDevices - 1 do begin
    pName := SDL_GetCameraDeviceName(pDeviceList[i]);
    WriteLn('  ', i, ': ', pName);

    pSpec := SDL_GetCameraDeviceSupportedFormats(pDeviceList[i], @pSpecCount);
    for j := 0 to pSpecCount - 1 do begin
      WriteLn('    spec w: ', pSpec[j].Width, ' h: ', pSpec[j].Height, ' f: ', pSpec[j].format, ' inum: ', pSpec[j].interval_numerator, ' ideno: ', pSpec[j].interval_denominator);
    end;
  end;

  if (pDeviceList <> nil) and (pSpec <> nil) then begin

    pCamera := SDL_OpenCameraDevice(pDeviceList[0], @pSpec[0]);
    if pCamera = nil then begin
      WriteLn('Open Camera Error');
    end;
    //    SDL_ReleaseCameraFrame(pCamera, winSurface);
    camSurface := SDL_AcquireCameraFrame(pCamera, @timeStamp);
    if camSurface = nil then begin
      WriteLn('camSurface Error');
    end;

    WriteLn('Open Camera  ', timeStamp);
  end;

  while not quit do begin
    while SDL_PollEvent(@e) <> 0 do begin
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

      SDL_BlitSurface(camSurface, nil, winSurface, nil);
    end;
  end;

  //  SDL_FreeSurface(winSurface);

//  SDL_DestroyWindow(win);
//  SDL_Quit;

  WriteLn('ende');
end.
