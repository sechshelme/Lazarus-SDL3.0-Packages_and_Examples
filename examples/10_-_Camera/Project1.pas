program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  SDL3,
  ctypes;

var
  nDevices: longint;
  pDeviceLiat: PSDL_CameraDeviceID;
  i: integer;
  pName: PChar;
begin
  SDL_Init(SDL_INIT_CAMERA);

  SDL_Log('Camera count: %i', SDL_GetNumCameraDrivers);

  pDeviceLiat := SDL_GetCameraDevices(@nDevices);
  SDL_Log('Camera count: %i', nDevices);
  for i := 0 to nDevices - 1 do begin
    pName := SDL_GetCameraDeviceName(pDeviceLiat[i]);
    WriteLn('  ',pName);
  end;

end.
